library(shiny)
library(bslib)
library(ellmer)
library(pdftools)

# Provider config: env var name and available models
providers <- list(
  Anthropic = list(
    env_var = "ANTHROPIC_API_KEY",
    fn = chat_anthropic,
    models = ellmer::models_anthropic()
  ),
  OpenAI = list(
    env_var = "OPENAI_API_KEY",
    fn = chat_openai,
    models = c("gpt-4o", "gpt-4o-mini", "gpt-4.1", "gpt-4.1-mini")
  ),
  `Google Gemini` = list(
    env_var = "GOOGLE_API_KEY",
    fn = chat_google_gemini,
    models = c("gemini-2.0-flash", "gemini-1.5-pro", "gemini-1.5-flash")
  )
)

# Simple structured schema for PDF extraction
doc_schema <- type_object(
  "DocumentSummary",
  title = type_string("Document title (or best guess)"),
  authors = type_string("Author(s), comma-separated if multiple"),
  date = type_string("Publication or document date, if found"),
  summary = type_string("2-3 sentence plain-English summary"),
  keywords = type_array(
    "Up to 5 key topics or themes",
    items = type_string()
  )
)

fmt_bytes <- function(b) {
  if (b >= 1e6) {
    sprintf("%.1f MB", b / 1e6)
  } else {
    sprintf("%.0f KB", b / 1e3)
  }
}

# ── UI ──────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "PDF Structured Extraction",
  theme = bs_theme(version = 5, preset = "shiny"),

  sidebar = sidebar(
    width = 300,

    selectInput(
      "provider",
      "LLM Provider",
      choices = names(providers),
      selected = "Anthropic"
    ),

    selectInput(
      "model",
      "Model",
      choices = providers[["Anthropic"]]$models
    ),

    passwordInput(
      "api_key",
      "API Key",
      placeholder = "Paste your key here"
    ),

    hr(),

    fileInput(
      "pdf",
      "Upload PDF",
      accept = ".pdf",
      buttonLabel = "Browse…",
      placeholder = "No file selected"
    ),

    # PDF info appears here after upload
    uiOutput("pdf_info_ui"),

    input_task_button("extract", "Extract", icon = bsicons::bs_icon("stars"))
  ),

  uiOutput("results_ui")
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  # Update model choices when provider changes
  observeEvent(input$provider, {
    updateSelectInput(
      session,
      "model",
      choices = providers[[input$provider]]$models
    )
  })

  # PDF metadata — fires immediately on upload, no API call needed
  pdf_meta <- reactive({
    req(input$pdf)
    path <- input$pdf$datapath
    info <- pdf_info(path)
    size <- file.size(path)
    list(pages = info$pages, size = size)
  })

  output$pdf_info_ui <- renderUI({
    req(pdf_meta())
    m <- pdf_meta()
    div(
      class = "d-flex gap-3 mb-2",
      div(
        class = "text-center flex-fill border rounded p-2",
        div(class = "fs-4 fw-bold text-primary", m$pages),
        div(class = "small text-muted", "pages")
      ),
      div(
        class = "text-center flex-fill border rounded p-2",
        div(class = "fs-4 fw-bold text-primary", fmt_bytes(m$size)),
        div(class = "small text-muted", "file size")
      )
    )
  })

  # Extraction — returns result + cost + elapsed time
  extraction <- bindEvent(
    reactive({
      req(input$pdf, nchar(trimws(input$api_key)) > 0)

      provider_cfg <- providers[[input$provider]]

      do.call(
        Sys.setenv,
        setNames(
          list(trimws(input$api_key)),
          provider_cfg$env_var
        )
      )

      chat <- provider_cfg$fn(model = input$model)

      elapsed <- system.time(
        result <- chat$chat_structured(
          "Extract key metadata and a brief summary from this document.",
          content_pdf_file(input$pdf$datapath),
          type = doc_schema
        )
      )

      cost <- tryCatch(
        chat$get_cost(include = "all"),
        error = function(e) NA_real_
      )

      list(
        result = result,
        cost = cost,
        elapsed = as.numeric(elapsed["elapsed"])
      )
    }),
    input$extract,
    ignoreNULL = TRUE
  )

  output$results_ui <- renderUI({
    # Placeholder before any extraction
    if (!isTruthy(try(extraction(), silent = TRUE))) {
      return(
        card(
          card_body(
            class = "text-muted text-center py-5",
            bsicons::bs_icon("file-earmark-text", size = "3em"),
            p("Upload a PDF and click Extract to see results.")
          )
        )
      )
    }

    tryCatch(
      {
        ex <- extraction()
        r <- ex$result

        cost_label <- if (!is.na(ex$cost)) {
          sprintf("$%.4f", ex$cost)
        } else {
          "n/a"
        }

        time_label <- sprintf("%.1f s", ex$elapsed)

        layout_column_wrap(
          width = 1,
          fill = FALSE,

          # Extraction stats row
          layout_column_wrap(
            width = 1 / 2,
            fill = FALSE,
            value_box(
              title = "Extraction time",
              value = time_label,
              theme = "secondary",
              showcase = bsicons::bs_icon("stopwatch")
            ),
            value_box(
              title = "API cost",
              value = cost_label,
              theme = "secondary",
              showcase = bsicons::bs_icon("currency-dollar")
            )
          ),

          # Document metadata
          card(
            card_header("Document Metadata"),
            card_body(
              tags$dl(
                class = "row mb-0",
                tags$dt(class = "col-sm-3", "Title"),
                tags$dd(class = "col-sm-9", r$title %||% "—"),
                tags$dt(class = "col-sm-3", "Authors"),
                tags$dd(class = "col-sm-9", r$authors %||% "—"),
                tags$dt(class = "col-sm-3", "Date"),
                tags$dd(class = "col-sm-9", r$date %||% "—"),
                tags$dt(class = "col-sm-3", "Keywords"),
                tags$dd(
                  class = "col-sm-9",
                  if (length(r$keywords) > 0) {
                    paste(r$keywords, collapse = " · ")
                  } else {
                    "—"
                  }
                )
              )
            )
          ),

          # Summary
          card(
            card_header("Summary"),
            card_body(p(r$summary %||% "No summary returned."))
          )
        )
      },
      error = function(e) {
        card(
          card_body(
            class = "text-danger",
            bsicons::bs_icon("exclamation-triangle-fill"),
            strong(" Extraction failed: "),
            conditionMessage(e)
          )
        )
      }
    )
  })
}

shinyApp(ui, server)
