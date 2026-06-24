# LLM Extraction Module ----
# A Shiny module for PDF upload and automated data extraction using Claude

# TODO: Report paper size (mb and page number)
# TODO: Remove excessively complicated BS layout code

# hard-coded list of possible providers. will need to be updated manually.
# can only call models_*() if an API key is already available
# curated_models: named vector (display name = API id) of recommended mid-tier models.
# Update when providers release new generations. Current as of June 2026.
provider_options <- list(
  Anthropic = list(
    env_var = "ANTHROPIC_API_KEY",
    fn = "chat_anthropic",
    models = "models_anthropic",
    curated_models = c(
      "Claude Haiku 4.5" = "claude-haiku-4-5",
      "Claude Sonnet 4.6" = "claude-sonnet-4-6",
      "Claude Opus 4.7" = "claude-opus-4-7",
      "Claude Opus 4.8" = "claude-opus-4-8"
    )
  ),
  OpenAI = list(
    env_var = "OPENAI_API_KEY",
    fn = "chat_openai",
    models = "models_openai",
    curated_models = c(
      "GPT-5.4 Mini" = "gpt-5.4-mini",
      "GPT-5.4" = "gpt-5.4",
      "GPT-5.5" = "gpt-5.5"
    )
  ),
  `Google Gemini` = list(
    env_var = "GOOGLE_API_KEY",
    fn = "chat_google_gemini",
    models = "models_google_gemini",
    curated_models = c(
      # TODO: Check these all actually work
      # TODO: So far, none of them do...
      "Gemini 2.5 Pro" = "gemini-2.5-pro",
      "Gemini 3.1 Pro" = "gemini-3.1-pro-preview",
      "Gemini 3.5 Flash" = "gemini-3.5-flash"
    )
  )
)


#' LLM Extraction UI Function ----
#' @noRd
#'
#' @importFrom shiny NS tagList fileInput textInput actionButton downloadButton downloadHandler
#' @importFrom bslib card card_body accordion accordion_panel tooltip layout_column_wrap input_task_button accordion_panel_open bind_task_button
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs disabled
#' @import eDataDRF
mod_llm_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(
      ".btn-file {
    padding: 6px 20px !important;
}"
    ))),
    # Enable shinyjs
    useShinyjs(),

    # Main extraction card ----
    card(
      card_body(
        ## Info accordion ----
        info_accordion(
          content_file = "inst/app/www/md/intro_llm.md"
        ),

        ## Upload and API section ----
        layout_column_wrap(
          width = "400px",
          fill = FALSE,
          fillable = FALSE,
          ### ENTERED_BY
          textInput(
            inputId = ns("ENTERED_BY"),
            label = tooltip(
              list("Entered By", bs_icon("info-circle-fill")),
              "Name/contact details."
            ),
            value = Sys.getenv("EDATA_USERNAME", unset = ""),
            placeholder = "Ole Nordman",
            width = "100%"
          ),
          div(
            fileInput(
              inputId = ns("pdf_file"),
              label = tooltip(
                list("Upload PDF", bs_icon("info-circle-fill")),
                "Upload a research paper or report (pdf) of up to 100 pages/30 MB containing environmental exposure data."
              ),
              accept = ".pdf",
              width = "100%",
              buttonLabel = "Browse...",
            ),
            uiOutput(ns("pdf_info_ui"))
          ),

          ### Provider / model / advanced settings ----
          selectInput(
            inputId = ns("select_provider"),
            label = tooltip(
              list("Choose provider", bs_icon("info-circle-fill")),
              "Select the AI provider: Anthropic (Claude), OpenAI (GPT), or Google (Gemini)."
            ),
            choices = names(provider_options),
            multiple = FALSE
          ),
          div(
            class = "input-group",
            div(
              class = "flex-grow-1",
              selectInput(
                inputId = ns("select_model"),
                label = tooltip(
                  list("Choose LLM", bs_icon("info-circle-fill")),
                  "Select the specific language model. Models are populated after a valid API key is detected."
                ),
                choices = c("Select a provider first"),
                multiple = FALSE,
                width = "100%"
              )
            ),
            div(
              style = "align-items: end; display: flex;",
              tooltip(
                input_task_button(
                  id = ns("test_model"),
                  label = bs_icon("activity"),
                  label_busy = "",
                  class = "btn-info-sm",
                  style = "height: calc(1.5em + 0.75rem + calc(var(--bs-border-width) * 2));
                  --bs-btn-padding-y: 0.25em; margin-bottom: 1rem;"
                ),
                "Ping the selected model with a minimal request to check your key and connection. Requires a functioning API key."
              )
            )
          ),
          ### API key input ----
          passwordInput(
            inputId = ns("api_key"),
            label = tooltip(
              list("LLM API Key", bs_icon("info-circle-fill")),
              "Your API key for the selected provider. Set the corresponding environment variable (ANTHROPIC_API_KEY, OPENAI_API_KEY, GOOGLE_API_KEY) to avoid re-entering it each session."
            ),
            # TODO: Is it a good idea to use Anthropic by default?
            value = Sys.getenv("ANTHROPIC_API_KEY", unset = ""),
            placeholder = "e.g. sk-ant-..., sk-..., or AQ...",
            width = "100%"
          ),
          ### max_tokens input ----
          # TODO: Slider?
          numericInput(
            inputId = ns("max_tokens"),
            label = tooltip(
              list("Max Tokens", bs_icon("info-circle-fill")),
              "Defines the quantity of information the extraction returns. 6000 is a sensible default, but longer papers/more complex models may need more. This will increase cost, so use sparingly."
            ),
            value = 6000,
            min = 1000,
            max = 20000,
            step = 1000,
            width = "100%"
          ),
          # TODO: Add warning (or better yet prevent entirely) when dependent data missing
          # TODO: Rename comments to "Screening" for consistency
          selectizeInput(
            inputId = ns("schema_components"),
            label = tooltip(
              list("Extract", bs_icon("info-circle-fill")),
              "Choose which data sections to extract. All are included by default; deselect any you don't need to reduce cost and response time."
            ),
            choices = schema_component_choices,
            selected = unname(schema_component_choices),
            multiple = TRUE,
            width = "100%",
            options = list(plugins = list("remove_button"))
          )
        ),

        ## Extract buttons ----
        layout_columns(
          fill = FALSE,
          tooltip(
            actionButton(
              inputId = ns("llm_advanced_options"),
              label = tagList(bs_icon("gear")),
              class = "btn-secondary",
            ),
            "Advanced settings for LLM extraction. Recommended for experienced users."
          ),
          # TODO: Enable cancellation of started extraction
          # TODO: Important - population doesn't re-trigger if you screen then extract!!!
          tooltip(
            input_task_button(
              id = ns("screen_data"),
              label = HTML(paste(
                bs_icon("lightning-charge"),
                "Screen PDF"
              )),
              class = "btn-info"
            ) |>
              disabled(),
            "LLM Rapidly screens the .pdf for its suitability in answering your research question. Equivalent to setting Extract to Comments only."
          ),
          tooltip(
            input_task_button(
              id = ns("extract_data"),
              label = HTML(paste(
                bs_icon("cpu"),
                "Extract Data"
              )),
              class = "btn-primary"
            ) |>
              disabled(),
            "Extract data from a .pdf using the chosen LLM. A .pdf must be uploaded to enable this function."
          ),

          tooltip(
            input_task_button(
              id = ns("load_dummy_data"),
              label = "Load Dummy Data",
              icon = icon("flask"),
              class = "btn-light"
            ),
            "Load a short dummy dataset for testing or demonstrations, as if you had extracted it from a paper."
          )
        ),

        ## Status and results ----
        div(
          uiOutput(ns("extraction_status"))
        ),

        # Extraction appraisal
        # TODO: Update styling
        # TODO: Add comments to downloadable data
        div(
          h5(
            "Extraction Appraisal"
          ),
          htmlOutput(ns("extraction_comments"))
        ),

        ## Extraction results accordion ----
        accordion(
          id = ns("results_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Extraction Raw Data",
            value = "extraction_results",
            icon = bs_icon("cpu"),
            div(
              verbatimTextOutput(ns("extraction_results"))
            )
          )
        )

        # TODO: Reduce sensitivity of module population triggers so they only fire if their data has been updated
        # ## Action buttons for extracted data  ----
        # layout_columns(
        #   fill = FALSE,
        #   tooltip(
        #     (input_task_button(
        #       id = ns("populate_forms"),
        #       label = "Populate Modules",
        #       icon = icon("download"),
        #       class = "btn-primary"
        #     ) |>
        #       disabled()),
        #     "Send extracted data to the data entry modules for checking and validation."
        #   ),

        #   input_task_button(
        #     id = ns("clear_extraction"),
        #     label = "Clear Extraction",
        #     icon = icon("trash"),
        #     class = "btn-danger"
        #   ) |>
        #     disabled()
        # )
      )
    )
  )
}

#' LLM Extraction Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateTextAreaInput ExtendedTask showModal modalDialog modalButton updateSelectInput
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom mirai mirai
#' @importFrom shinyjs enable disable
#' @importFrom stringr str_replace str_extract
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @importFrom ellmer chat_anthropic params content_pdf_file type_object type_string type_integer type_number type_array
#' @importFrom utils str
#' @importFrom tibble as_tibble
#' @importFrom pdftools pdf_info
mod_llm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## InputValidator$new: iv ----
    iv <- InputValidator$new()

    iv$add_rule("ENTERED_BY", sv_required())
    iv$add_rule("ENTERED_BY", function(value) {
      if (isTruthy(value) && nchar(value) < 2) {
        "Entered By must be at least 2 characters"
      }
    })

    iv$add_rule("pdf_file", function(value) {
      if (is.null(value)) {
        "Please upload a PDF"
      }
    })

    iv$add_rule("api_key", sv_required())
    iv$add_rule("api_key", function(value) {
      if (!isTruthy(value)) {
        return(NULL)
      }
      provider <- input$select_provider
      valid <- switch(
        provider %||% "Anthropic",
        "Anthropic" = startsWith(value, "sk-ant-"),
        "OpenAI" = startsWith(value, "sk-") && !startsWith(value, "sk-ant-"),
        "Google Gemini" = startsWith(value, "AQ."),
        TRUE
      )
      if (!valid) {
        hint <- switch(
          provider %||% "Anthropic",
          "Anthropic" = "'sk-ant-'",
          "OpenAI" = "'sk-'",
          "Google Gemini" = "'AQ.'",
          "the expected prefix"
        )
        paste0("API keys for ", provider, " should start ", hint)
      }
    })
    iv$add_rule("api_key", function(value) {
      if (isTruthy(value) && nchar(value) < 20) {
        "API key must be at least 20 characters long"
      }
    })

    ## InputValidator$enable() ----
    iv$enable()

    ## # ReactiveValues: moduleState -----
    moduleState <- reactiveValues(
      extraction_successful = FALSE,
      raw_extraction = NULL,
      structured_data = NULL,
      error_message = NULL
    )

    # 2. Observers and Reactives ----

    # reactive: PDF metadata — fires immediately on upload, no API call needed
    pdf_meta <- reactive({
      req(input$pdf_file)
      path <- input$pdf_file$datapath
      info <- pdf_info(path)
      size <- file.size(path)
      list(pages = info$pages, size = size)
    })

    ## # observe: Reset configuration to defaults ----
    observe({
      updateTextAreaInput(
        session,
        "extraction_prompt",
        value = create_extraction_prompt()
      )
      updateTextAreaInput(
        session,
        "extraction_schema_display",
        value = get_schema_display()
      )
      showNotification("Configuration reset to defaults", type = "message")
    }) |>
      bindEvent(input$reset_defaults)

    ## # observe: Update API key field when provider changes ----
    observe({
      provider <- input$select_provider
      req(provider)
      env_var <- provider_options[[provider]]$env_var
      updateTextInput(
        session,
        "api_key",
        value = Sys.getenv(env_var, unset = "")
      )
    }) |>
      bindEvent(input$select_provider, ignoreInit = TRUE)

    ## # observe: Update model list when provider, key, or filter changes ----
    observe({
      provider <- input$select_provider
      api_key <- input$api_key
      req(provider)

      # NULL before modal is opened = default curated view
      show_all <- isTRUE(input$enable_all_models)

      if (!show_all) {
        updateSelectInput(
          session,
          "select_model",
          choices = provider_options[[provider]]$curated_models
        )
        return()
      }

      # Full model list via ellmer API call
      model_spec <- provider_options[[provider]]$models
      if (isTruthy(api_key)) {
        tryCatch(
          {
            models <- do.call(
              getExportedValue("ellmer", model_spec),
              list()
            )

            # Use named vector: label = pretty name, value = bare ID
            model_choices <- if ("name" %in% colnames(models)) {
              setNames(pull(models, "id"), pull(models, "name"))
            } else {
              pull(models, "id")
            }

            updateSelectInput(session, "select_model", choices = model_choices)
          },
          error = function(e) {
            updateSelectInput(
              session,
              "select_model",
              choices = c("Could not fetch models - check API key")
            )
          }
        )
      } else {
        updateSelectInput(
          session,
          "select_model",
          choices = c("Enter a valid API key to load models")
        )
      }
    }) |>
      bindEvent(
        input$select_provider,
        input$api_key,
        input$enable_all_models,
        ignoreNULL = FALSE
      )

    ## # observe: Advanced settings modal ----
    observe({
      showModal(modalDialog(
        title = tagList(bs_icon("sliders"), " Advanced LLM Settings"),
        h6("Model Selection"),
        checkboxInput(
          ns("enable_all_models"),
          label = "Show all available models",
          value = isTRUE(input$enable_all_models)
        ),
        helpText(
          "By default only recommended mid-tier models are shown. Check to display all available models (including those which probably won't work)."
        ),
        tags$hr(),
        h6("Provider Status"),
        p("Check provider status dashboard for any reported model issues."),
        tags$ul(
          tags$li(tags$a(
            "Anthropic",
            href = "https://status.anthropic.com/",
            target = "_blank"
          )),
          tags$li(tags$a(
            "OpenAI",
            href = "https://status.openai.com/",
            target = "_blank"
          )),
          tags$li(tags$a(
            "Google AI",
            href = "https://aistudio.google.com/status",
            target = "_blank"
          ))
        ),
        tags$hr(),
        h6("Call Parameters"),
        p(
          "Additional LLM call settings (temperature, retry behaviour, etc.) will appear here."
        ),
        ## Prompt and Schema Configuration ----

        # TODO: Add upload/download button for schema and prompt
        # TODO: Make schema represent user options
        # TODO: Make schema less ugly
        p("Modify Prompt and Data Structure (Advanced)"),
        div(
          textAreaInput(
            inputId = ns("extraction_prompt"),
            label = "Extraction Instructions",
            value = create_extraction_prompt(),
            rows = 8,
            width = "100%"
          ),

          textAreaInput(
            inputId = ns("extraction_schema_display"),
            label = "Schema Definition",
            value = get_schema_display(),
            rows = 12,
            width = "100%"
          ),

          div(
            style = "margin-top: 10px;",
            actionButton(
              ns("reset_defaults"),
              "Reset to Defaults",
              class = "btn-secondary btn-sm"
            )
          )
        ),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "xl"
      ))
    }) |>
      bindEvent(input$llm_advanced_options)

    ## # observe: Enable extract/screen buttons when PDF and API key available ----
    # upstream: input$pdf_file, input$api_key, iv
    # downstream: extract_data, screen_data button state
    observe({
      if (!is.null(input$pdf_file) && iv$is_valid()) {
        enable("extract_data")
        enable("screen_data")
      } else {
        disable("extract_data")
        disable("screen_data")
      }
    })

    ## # observe: Load dummy data ----
    # upstream: user clicks input$load_dummy_data
    # downstream: moduleState$*, session$userData$reactiveValues$*DataLLM
    observe({
      # disable buttons where simultaneous running could cause problems
      disable("extract_data")

      populate_session_with_dummy_data(session = session)

      # Also save outputs to server data so we can download them later if needed
      session$userData$reactiveValues$promptLLM <- if (
        isTruthy(input$extraction_prompt)
      ) {
        input$extraction_prompt
      } else {
        create_extraction_prompt()
      }
      # session$userData$reactiveValues$rawLLM <- dummy_data

      showNotification(
        "Dummy data loaded successfully!",
        type = "default"
      )

      # set success flags last
      session$userData$reactiveValues$llmExtractionComplete <- TRUE
      session$userData$reactiveValues$llmExtractionSuccessful <- TRUE

      # need to set populate modules to trigger some downstream logic (why?)
      session$userData$reactiveValues$llmPopulateModules <- TRUE

      # Enable extraction-dependent and extract_data button again
      enable("populate_forms")
      enable("clear_extraction")
      enable("extract_data")
    }) |>
      bindEvent(input$load_dummy_data)

    ## # ExtendedTask: PDF data extraction ----
    # upstream: user clicks input$extract_data
    llm_task <- ExtendedTask$new(
      function(
        pdf_path,
        model_provider,
        model_name,
        env_var,
        chat_fn,
        api_key,
        extraction_prompt,
        extraction_schema,
        max_tokens
      ) {
        mirai(
          {
            extract_pdf_with_llm(
              pdf_path,
              model_provider,
              model_name,
              env_var,
              chat_fn,
              api_key,
              extraction_prompt,
              extraction_schema,
              max_tokens
            )
          },
          pdf_path = pdf_path,
          model_provider = model_provider,
          model_name = model_name,
          env_var = env_var,
          chat_fn = chat_fn,
          api_key = api_key,
          extraction_prompt = extraction_prompt,
          extraction_schema = extraction_schema,
          max_tokens = max_tokens
        )
      }
    ) |>
      # we need both this and to invoke in the below observers
      bind_task_button("extract_data") |>
      bind_task_button("screen_data")

    ## # ExtendedTask: model connection test ----
    test_task <- ExtendedTask$new(
      function(model_provider, model_name, env_var, chat_fn, api_key) {
        mirai(
          {
            test_llm_connection(
              model_provider,
              model_name,
              env_var,
              chat_fn,
              api_key
            )
          },
          model_provider = model_provider,
          model_name = model_name,
          env_var = env_var,
          chat_fn = chat_fn,
          api_key = api_key
        )
      }
    ) |>
      bind_task_button("test_model")

    ## # observe: invoke model connection test ----
    observe({
      req(input$api_key, input$select_model, input$select_provider)
      provider <- input$select_provider
      test_task$invoke(
        model_provider = provider,
        model_name = input$select_model,
        env_var = provider_options[[provider]]$env_var,
        chat_fn = provider_options[[provider]]$fn,
        api_key = input$api_key
      )
    }) |>
      bindEvent(input$test_model)

    ## # observe: show test result notification ----
    observe({
      result <- test_task$result()
      req(!is.null(result))
      if (isTRUE(result$success)) {
        showNotification(result$message, type = "message", duration = 10)
      } else {
        showNotification(result$message, type = "error", duration = 15)
      }
    })

    ## # observe: Screen PDF (comments-only extraction) ----
    # upstream: user clicks input$screen_data
    observe({
      req(input$pdf_file, input$api_key)

      if (!iv$is_valid()) {
        showNotification(
          "Please fix validation errors before screening.",
          type = "warning"
        )
        return()
      }

      tryCatch(
        {
          validate_api_key(input$api_key, input$select_provider)
        },
        error = function(e) {
          showNotification(e$message, type = "warning")
          return()
        }
      )

      provider <- input$select_provider
      llm_task$invoke(
        pdf_path = input$pdf_file$datapath,
        model_provider = provider,
        model_name = input$select_model,
        env_var = provider_options[[provider]]$env_var,
        chat_fn = provider_options[[provider]]$fn,
        api_key = input$api_key,
        extraction_prompt = if (isTruthy(input$extraction_prompt)) {
          input$extraction_prompt
        } else {
          create_extraction_prompt()
        },
        extraction_schema = create_extraction_schema(include = "comments"),
        max_tokens = input$max_tokens
      )
    }) |>
      bindEvent(input$screen_data)

    ## # observe: PDF data extraction ----
    # upstream: user clicks input$extract_data
    # downstream: moduleState$*, session$userData$reactiveValues$*DataLLM
    # Create ExtendedTask at module initialization
    observe({
      req(input$pdf_file, input$api_key)

      # Validate inputs before proceeding
      if (!iv$is_valid()) {
        showNotification(
          "Please fix validation errors before extracting data.",
          type = "warning"
        )
        return()
      }

      # Synchronous validation (fast)
      tryCatch(
        {
          validate_api_key(input$api_key, input$select_provider)
        },
        error = function(e) {
          showNotification(e$message, type = "warning")
          return()
        }
      )

      # Launch async extraction
      provider <- input$select_provider
      llm_task$invoke(
        pdf_path = input$pdf_file$datapath,
        model_provider = provider,
        model_name = input$select_model,
        env_var = provider_options[[provider]]$env_var,
        chat_fn = provider_options[[provider]]$fn,
        api_key = input$api_key,
        extraction_prompt = if (isTruthy(input$extraction_prompt)) {
          input$extraction_prompt
        } else {
          create_extraction_prompt()
        },
        extraction_schema = create_extraction_schema(
          include = input$schema_components
        ),
        max_tokens = input$max_tokens
      )
    }) |>
      bindEvent(input$extract_data)

    ## # observe: Enable download button when extraction is complete ----
    # upstream: session$userData$reactiveValues$llmExtractionComplete, session$userData$reactiveValues$llmExtractionSuccessful
    # downstream: download_extraction button state
    observe({
      if (
        session$userData$reactiveValues$llmExtractionComplete &&
          session$userData$reactiveValues$llmExtractionSuccessful
      ) {
        enable("download_extraction")
      } else {
        disable("download_extraction")
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$llmExtractionComplete,
        session$userData$reactiveValues$llmExtractionSuccessful
      )

    ## # observe: Populate forms with extracted data ----
    # upstream: user clicks input$populate_forms
    # downstream: trigger form population in other modules
    observe({
      req(!is.null(moduleState$structured_data))

      # Campaign data
      tryCatch(
        {
          if (!is.null(moduleState$structured_data$campaign)) {
            session$userData$reactiveValues$campaignDataLLM <- moduleState$structured_data$campaign
          }
        },
        error = function(e) {
          showNotification(
            paste("Error populating campaign data:", e$message),
            type = "error",
            duration = 10
          )
        }
      )

      # References data
      tryCatch(
        {
          if (!is.null(moduleState$structured_data$references)) {
            session$userData$reactiveValues$referenceDataLLM <- moduleState$structured_data$references |>
              as_tibble()
          }
        },
        error = function(e) {
          showNotification(
            paste("Error populating references data:", e$message),
            type = "error",
            duration = 10
          )
        }
      )

      # Sites data
      tryCatch(
        {
          if (!is.null(moduleState$structured_data$sites)) {
            sites_data <- create_sites_from_llm(
              moduleState$structured_data$sites,
              moduleState$structured_data$campaign,
              session
            )
            session$userData$reactiveValues$sitesDataLLM <- sites_data
          }
        },
        error = function(e) {
          showNotification(
            paste("Error populating sites data:", e$message),
            type = "error",
            duration = 10
          )
        }
      )

      # Parameters data
      tryCatch(
        {
          if (!is.null(moduleState$structured_data$parameters)) {
            parameters_data <- create_parameters_from_llm(
              moduleState$structured_data$parameters,
              session = session,
              chemical_parameters = eDataDRF::parameters_vocabulary()
            )
            session$userData$reactiveValues$parametersDataLLM <- parameters_data
          }
        },
        error = function(e) {
          showNotification(
            paste("Error populating parameters data:", e$message),
            type = "error",
            duration = 10
          )
        }
      )

      # Compartments data
      tryCatch(
        {
          if (!is.null(moduleState$structured_data$compartments)) {
            compartments_data <- create_compartments_from_llm(
              moduleState$structured_data$compartments
            )
            session$userData$reactiveValues$compartmentsDataLLM <- compartments_data
          }
        },
        error = function(e) {
          showNotification(
            paste("Error populating compartments data:", e$message),
            type = "error",
            duration = 10
          )
        }
      )

      # Biota data
      tryCatch(
        {
          if (!is.null(moduleState$structured_data$biota)) {
            biota_data <- create_biota_from_llm(
              moduleState$structured_data$biota
            )
            session$userData$reactiveValues$biotaDataLLM <- biota_data
          }
        },
        error = function(e) {
          showNotification(
            paste("Error populating biota data:", e$message),
            type = "error",
            duration = 10
          )
        }
      )

      # Methods data
      tryCatch(
        {
          if (!is.null(moduleState$structured_data$methods)) {
            methods_data <- create_methods_from_llm(
              moduleState$structured_data$methods,
              moduleState$structured_data$campaign
            )
            session$userData$reactiveValues$methodsDataLLM <- methods_data
          }
        },
        error = function(e) {
          showNotification(
            paste("Error populating methods data:", e$message),
            type = "error",
            duration = 10
          )
        }
      )

      # Samples data
      tryCatch(
        {
          if (!is.null(moduleState$structured_data$samples)) {
            samples_data <- create_samples_from_llm(
              moduleState$structured_data$samples
            )
            session$userData$reactiveValues$samplesDataLLM <- samples_data
          }
        },
        error = function(e) {
          showNotification(
            paste("Error populating samples data:", e$message),
            type = "error",
            duration = 10
          )
        }
      )

      # Set extraction status flags
      session$userData$reactiveValues$llmPopulateModules <- TRUE

      showNotification(
        "Forms populated with extracted data! Review and correct in each module.",
        type = "message"
      )
    }) |>
      bindEvent(isTRUE(session$userData$reactiveValues$llmExtractionSuccessful))

    ## # observe: Clear extraction ----
    # upstream: user clicks input$clear_extraction
    # downstream: reset module state and session data
    observe({
      # Clear module state
      moduleState$raw_extraction <- NULL
      moduleState$structured_data <- NULL
      moduleState$error_message <- NULL
      moduleState$api_metadata <- NULL

      # Clear session LLM data and status flags
      clear_llm_data_from_session(session)
      session$userData$reactiveValues$llmExtractionComplete <- FALSE
      session$userData$reactiveValues$llmExtractionSuccessful <- FALSE
      session$userData$reactiveValues$llmPopulateModules <- FALSE

      # Disable buttons
      # disable("populate_forms")
      disable("clear_extraction")
      disable("download_extraction")

      showNotification("Extraction cleared", type = "message")
    }) |>
      bindEvent(input$clear_extraction)

    ## # observe ~ bindEvent: Set session username from ENTERED_BY ----
    observe({
      req(input$ENTERED_BY)

      # only trigger if a username doesn't already exist in the session
      if (!isTruthy(session$userData$reactiveValues$ENTERED_BY)) {
        # Set the reactive value
        session$userData$reactiveValues$ENTERED_BY <- input$ENTERED_BY

        showNotification(
          glue("Saved your username {input$ENTERED_BY} to session data."),
          type = "message"
        )
      }
    }) |>
      bindEvent(input$ENTERED_BY |> debounce(2000), ignoreInit = FALSE)

    # 3. Outputs ----

    ## # output: pdf_info_ui ----
    # reports page number and size of uploaded PDFs
    output$pdf_info_ui <- renderUI({
      m <- pdf_meta()
      div(
        class = "text-muted small",
        style = "margin-top: -1rem;",
        m$pages,
        "pages, ",
        fmt_bytes(m$size)
      )
    })

    ## # output: extraction_status ----
    # upstream: moduleState
    # downstream: UI status display
    output$extraction_status <- renderUI({
      # Changed from renderText to renderUI

      # Check if task is still running
      if (identical(llm_task$status(), "running")) {
        return(div(
          bs_icon("hourglass-split"),
          "Extracting data from PDF... This may take 30-60 seconds.",
          class = "validation-status validation-info"
        ))
      }

      # Try to get result (won't block if not ready)
      result <- llm_task$result()

      if (is.null(result)) {
        # No extraction attempted yet
        div(
          bs_icon("info-circle"),
          "Upload a PDF and provide your API key to begin extraction, or use dummy data for testing.",
          class = "validation-status validation-info"
        )
      } else if (!is.null(result$success) && result$success) {
        # Store successful results
        # TODO: This probably shouldn't be in an output block...
        moduleState$structured_data <- result$result
        moduleState$api_metadata <- result$metadata
        moduleState$raw_extraction <- result$result

        # Update session data
        session$userData$reactiveValues$rawLLM <- result$result
        session$userData$reactiveValues$llmExtractionComplete <- TRUE
        session$userData$reactiveValues$llmExtractionSuccessful <- TRUE
        if (!is.null(result$result$comments)) {
          session$userData$reactiveValues$llmExtractionComments <- result$result$comments
        }

        # Enable buttons
        # enable("populate_forms")
        enable("clear_extraction")

        # Build status message
        status_text <- "Extraction successful."

        # TODO: Fix cost being NA (sometimes)
        if (!is.null(result$metadata$total_cost)) {
          metadata_text <- paste0(
            " (Cost: $",
            round(result$metadata$total_cost, 2),
            ")"
          )
          status_text <- paste0(status_text, metadata_text)
        }

        div(
          bs_icon("check-circle"),
          status_text,
          class = "validation-status validation-complete"
        )
      } else {
        # Extraction failed
        error_msg <- if (!is.null(result$error)) {
          result$error
        } else {
          "Unknown error occurred"
        }

        moduleState$error_message <- error_msg
        session$userData$reactiveValues$llmExtractionComplete <- TRUE
        session$userData$reactiveValues$llmExtractionSuccessful <- FALSE

        showNotification(
          paste("Extraction failed:", error_msg),
          type = "error",
          duration = 15
        )

        div(
          bs_icon("exclamation-triangle"),
          paste("Extraction failed:", error_msg),
          class = "validation-status validation-warning"
        )
      }
    })

    ## # output: extraction_results ----
    # upstream: moduleState$raw_extraction
    # downstream: UI results display
    output$extraction_results <- renderText({
      if (!is.null(moduleState$raw_extraction)) {
        # Format the extraction results for display
        if (is.list(moduleState$raw_extraction)) {
          # Pretty print the structured data
          capture.output(str(
            moduleState$raw_extraction,
            max.level = 6,
            vec.len = 10,
            nchar.max = 5000
          )) |>
            paste(collapse = "\n")
        } else {
          as.character(moduleState$raw_extraction)
        }
      } else if (!is.null(moduleState$error_message)) {
        paste("Extraction Error:", moduleState$error_message)
      } else {
        "No extraction performed yet."
      }
    })

    ## # output: download_extraction ----
    # upstream: moduleState$raw_extraction
    # downstream: file download
    output$download_extraction <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("llm_extraction_", timestamp, ".txt")
      },
      content = function(file) {
        # Create comprehensive output including metadata
        output_lines <- c()

        # Header information ----
        output_lines <- c(
          output_lines,
          "=== LLM PDF Data Extraction Results ===",
          paste("Generated:", Sys.time()),
          ""
        )

        # API metadata if available ----
        if (!is.null(moduleState$api_metadata)) {
          output_lines <- c(
            output_lines,
            "=== API Usage Statistics ===",
            paste(
              "Total Cost: $",
              sprintf("%.4f", moduleState$api_metadata$total_cost)
            ),
            paste("Input Tokens:", moduleState$api_metadata$total_input_tokens),
            paste(
              "Output Tokens:",
              moduleState$api_metadata$total_output_tokens
            ),
            paste("API Calls:", moduleState$api_metadata$call_count),
            ""
          )
        }

        # Structured data output ----
        if (!is.null(moduleState$raw_extraction)) {
          output_lines <- c(
            output_lines,
            "=== Extracted Structured Data ===",
            ""
          )

          # Convert structured data to readable format
          if (is.list(moduleState$raw_extraction)) {
            structured_output <- capture.output(
              str(
                moduleState$raw_extraction,
                max.level = 100,
                vec.len = 200,
                nchar.max = 5000
              )
            )
            output_lines <- c(output_lines, structured_output)
          } else {
            output_lines <- c(
              output_lines,
              as.character(moduleState$raw_extraction)
            )
          }
        }

        # Write to file
        writeLines(output_lines, file, useBytes = TRUE)
      }
    )

    ## # output: extraction commentary ----
    output$extraction_comments <- renderUI({
      if (
        !is.null(
          session$userData$reactiveValues$llmExtractionComments
        ) &&
          length(session$userData$reactiveValues$llmExtractionComments) != 0
      ) {
        render_extraction_comments(
          session$userData$reactiveValues$llmExtractionComments
        )
      } else {
        "The LLM's self-appraisal of its performance will appear here once you extract data."
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$llmExtractionComments,
        ignoreNULL = TRUE,
        ignoreInit = FALSE
      )
  })
}

# 4. Helper Functions ----

#' Create extraction prompt with controlled vocabulary
#' @description Creates the system prompt for Claude extraction
#' @importFrom readr read_file
#' @noRd
create_extraction_prompt <- function() {
  read_file("inst/app/www/md/extraction_prompt.md")
}

#' Clear LLM data from session reactiveValues
#' @param session Shiny session object
#' @noRd
clear_llm_data_from_session <- function(session) {
  session$userData$reactiveValues$campaignDataLLM <- NULL
  session$userData$reactiveValues$referenceDataLLM <- NULL
  session$userData$reactiveValues$sitesDataLLM <- NULL
  session$userData$reactiveValues$parametersDataLLM <- NULL
  session$userData$reactiveValues$compartmentsDataLLM <- NULL
  session$userData$reactiveValues$biotaDataLLM <- NULL
  session$userData$reactiveValues$methodsDataLLM <- NULL
  session$userData$reactiveValues$samplesDataLLM <- NULL
  showNotification(
    "Cleared all LLM extracted data from session",
    type = "message"
  )
}

render_extraction_comments <- function(named_list) {
  tagList(
    lapply(names(named_list), function(nm) {
      pretty_name <- c(
        "paper_relevance" = "Relevance",
        "paper_reliability" = "Reliability",
        "paper_data_source" = "Original Data",
        "paper_data_available" = "Data Availability",
        "extraction_assessement" = "Extraction Suitability"
      )
      score_emoji <- c(
        # emoji coloured circles
        "Score: 5" = "\U0001F7E2",
        "Score: 4" = "\U0001F7E2",
        "Score: 3" = "\U0001F7E1",
        "Score: 2" = "\U0001F7E0",
        "Score: 1" = "\U0001F534"
      )
      tags$div(
        tags$strong(paste0(pretty_name[[nm]], ": ")),
        score_emoji[[str_extract(
          named_list[[nm]],
          pattern = "Score: [1-5]"
        )]],
        str_replace(
          string = named_list[[nm]],
          pattern = "Score: [1-5]",
          replacement = ""
        )
      )
    })
  )
}

## To be copied in the UI ----
# mod_llm_ui("llm_1")

## To be copied in the server ----
# mod_llm_server("llm_1")
