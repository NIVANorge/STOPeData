# Setup token for Zenodo API connection

# Initialize Zenodo managers and get options
library(zen4R) # FIXME: Better practice pls.
zenodo_sandbox <- ZenodoManager$new(
  url = "http://sandbox.zenodo.org/api",
  sandbox = TRUE,
  token = Sys.getenv('ZENSANDTOKEN'),
  logger = "DEBUG"
)

zenodo_production <- ZenodoManager$new(
  url = "https://zenodo.org/api",
  sandbox = FALSE,
  token = Sys.getenv('ZENTOKEN'),
  logger = "INFO"
)

# Get licenses and resource types
licenses_list <- zenodo_sandbox$getLicenses(pretty = TRUE)
licenses <- licenses_list[[1]]
resourceType <- zenodo_sandbox$getResourceTypes()

# fixme: import tags

#' zenodo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import bslib
mod_Zenodo_ui <- function(id) {
  ns <- NS(id)
  tagList(bslib::card_body(
    fillable = FALSE,

    # ===== Input Mode (Upload vs Template) =====
    tags$h5(
      style = "margin-top: 0; margin-bottom: 15px; color: #0d6efd; border-bottom: 2px solid #0d6efd; padding-bottom: 5px;",
      icon("toggle-on"),
      " Choose Input Mode"
    ),
    radioButtons(
      ns("inputMode"),
      label = NULL,
      choices = c(
        "Upload file (.xlsx)" = "upload",
        "Use README template" = "template"
      ),
      selected = "upload",
      inline = TRUE
    ),

    # ===== Environment card =====
    div(
      style = "background-color: #f8f9fa; border: 2px solid #dee2e6; border-radius: 6px; padding: 15px; margin-bottom: 20px;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 15px;",
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          tags$strong("Environment:"),
          shinyWidgets::materialSwitch(
            inputId = ns("zenEnvironment"),
            label = NULL,
            value = TRUE,
            status = "success",
            right = TRUE
          ),
          tags$span(
            id = ns("envLabel"),
            style = "font-weight: 500;",
            "Sandbox (Testing)"
          )
        ),
        tags$span(
          id = ns("envBadge"),
          style = "background-color: #ffc107; color: #000; padding: 6px 16px; border-radius: 20px; font-size: 0.8em; font-weight: bold; letter-spacing: 0.5px;",
          "TEST MODE"
        )
      ),
      uiOutput(ns("environmentWarning"))
    ),

    # ===== Two-column layout: left (fields) | right (preview) =====
    bslib::layout_columns(
      col_widths = c(8, 4),

      # ---------- LEFT COLUMN: all form fields ----------
      div(
        # Section 1: File Upload (only if "upload" mode)
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("inputMode")),
          tags$h5(
            style = "margin-top: 0; margin-bottom: 15px; color: #0d6efd; border-bottom: 2px solid #0d6efd; padding-bottom: 5px;",
            icon("upload"),
            " 1. Upload Your Data"
          ),
          fileInput(
            ns('zenUpload'),
            "Select your file*",
            accept = ".xlsx",
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          )
        ),

        # Section 2: Dataset Information
        tags$h5(
          style = "margin-top: 25px; margin-bottom: 15px; color: #0d6efd; border-bottom: 2px solid #0d6efd; padding-bottom: 5px;",
          icon("info-circle"),
          " 2. Dataset Information"
        ),
        textInput(
          ns("zenTitle"),
          "Dataset Title*",
          placeholder = "Enter a descriptive title for your dataset",
          width = '600px'
        ),

        # EMODnet URL (only shown in template mode)
        conditionalPanel(
          condition = sprintf("input['%s'] == 'template'", ns("inputMode")),
          textInput(
            ns("emodnetUrl"),
            "EMODnet Dataset URL*",
            placeholder = "https://emodnet.ec.europa.eu/...",
            width = '600px'
          )
        ),

        textAreaInput(
          ns("zenDescription"),
          "Description*",
          placeholder = "Provide a detailed description of your dataset",
          width = '600px',
          height = '100px'
        ),

        # Section 3: Author Information
        # Section 3: Author Information
        tags$h5(
          style = "margin-top: 25px; margin-bottom: 15px; color: #0d6efd; border-bottom: 2px solid #0d6efd; padding-bottom: 5px;",
          icon("user"),
          " 3. Author Information"
        ),

        # Dynamic author rows
        uiOutput(ns("authorFields")),

        # Add author button
        actionButton(
          ns("addAuthor"),
          "Add Another Author",
          icon = icon("plus-circle"),
          class = "btn-primary"
        ),

        # Section 4: Contact Information
        tags$h5(
          style = "margin-top: 25px; margin-bottom: 15px; color: #0d6efd; border-bottom: 2px solid #0d6efd; padding-bottom: 5px;",
          icon("envelope"),
          " 4. Contact Information"
        ),
        textInput(
          ns("contactName"),
          "Contact Name/Role",
          placeholder = "e.g., Dr. Jane Smith or Data Curator"
        ),
        textInput(
          ns("contactEmail"),
          "Contact Email*",
          placeholder = "contact@example.com"
        ),

        # Section 5: Metadata
        tags$h5(
          style = "margin-top: 25px; margin-bottom: 15px; color: #0d6efd; border-bottom: 2px solid #0d6efd; padding-bottom: 5px;",
          icon("tags"),
          " 5. Metadata & Licensing"
        ),
        selectizeInput(
          ns("zenResourceType"),
          "Resource type*",
          choices = resourceType,
          selected = "dataset"
        ),
        selectizeInput(
          ns("zenLicense"),
          "License*",
          choices = licenses,
          selected = "cc-by-4.0"
        ),
        tags$small(
          style = "color: #6c757d; display: block; margin-top: -10px; margin-bottom: 15px;",
          icon("question-circle"),
          " ",
          tags$a(
            href = "https://creativecommons.org/licenses/",
            target = "_blank",
            "Learn about licenses"
          )
        ),
        selectizeInput(
          ns("zenAccess"),
          "Access rights*",
          choices = c(
            "Open access" = "open",
            "Embargoed" = "embargoed",
            "Restricted" = "restricted",
            "Closed" = "closed"
          ),
          selected = "open"
        ),

        # Section 6: Optional Information
        tags$h5(
          style = "margin-top: 25px; margin-bottom: 15px; color: #6c757d; border-bottom: 2px solid #dee2e6; padding-bottom: 5px;",
          icon("ellipsis-h"),
          " 6. Optional Information"
        ),
        textInput(
          ns("zenGrant"),
          "Grant agreement ID",
          placeholder = "e.g., 101057014",
          value = "101057014"
        ),
        textAreaInput(
          ns("zenComment"),
          "Comment to curator",
          placeholder = "Any additional information (optional)",
          rows = 2
        ),

        # Submit section
        div(
          style = "margin-top: 30px; padding-top: 20px; border-top: 2px solid #dee2e6;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 15px;",
            tags$small(
              style = "color: #6c757d; font-style: italic;",
              "* Required fields"
            ),
            input_task_button(
              ns('submitZen'),
              "Submit to Zenodo",
              color = "primary",
              style = "material-flat",
              icon = icon("cloud-upload-alt"),
              size = 'md'
            )
          )
        )
      ),

      # ---------- RIGHT COLUMN: README preview (only if "template" mode) ----------
      conditionalPanel(
        condition = sprintf("input['%s'] == 'template'", ns("inputMode")),
        div(
          style = "
          position: sticky; top: 12px;
          background: #fff; border: 1px solid #e5e7eb; border-radius: 8px; padding: 12px;
        ",

          selectInput(
            ns('templateChoice'),
            label = 'README template',
            choices = c('EMODnet basic' = 'emodnet_basic'),
            selected = 'emodnet_basic'
          ),

          div(
            style = "display: flex; justify-content: space-between; align-items: center; gap: 8px; margin-bottom: 8px;",
            radioButtons(
              ns("viewMode"),
              NULL,
              choices = c("Preview" = "rich", "Markdown Code" = "raw"),
              inline = TRUE
            ),
            actionButton(
              ns("copyReadme"),
              "Copy README",
              class = "btn-sm",
              icon = icon("copy")
            )
          ),

          div(
            style = "flex: 1 1 auto; min-height: 0; overflow: auto;",
            uiOutput(ns("readmePreview"))
          )
        )
      )
    )
  ))
}

#' zenodo Server Functions
#'
#' @noRd
mod_Zenodo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Null-coalescing helper ─────────────────────────────────────────────────
    `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b

    # ── Reactive to get current Zenodo manager ────────────────────────────────
    current_zenodo <- reactive({
      if (input$zenEnvironment) zenodo_sandbox else zenodo_production
    })

    # ═════════════════════════════════════════════════════════════════════════
    # SECTION 3 — Multi-author state & UI
    # ═════════════════════════════════════════════════════════════════════════

    author_count <- reactiveVal(1)

    observeEvent(input$addAuthor, {
      author_count(author_count() + 1)
    })

    # Render one row per author
    output$authorFields <- renderUI({
      n <- author_count()
      lapply(seq_len(n), function(i) {
        div(
          style = paste0(
            "border: 1px solid #dee2e6; border-radius: 6px; padding: 14px; margin-bottom: 12px;",
            if (n > 1) " position: relative;" else ""
          ),
          # Remove button (all rows except the first)
          if (i > 1) {
            div(
              style = "position: absolute; top: 8px; right: 10px;",
              actionButton(
                ns(paste0("removeAuthor_", i)),
                NULL,
                icon = icon("times"),
                class = "btn-sm btn-outline-danger",
                style = "padding: 2px 8px; font-size: 0.75rem;",
                title = paste("Remove author", i)
              )
            )
          },
          tags$small(
            style = "color: #6c757d; font-weight: 600; display: block; margin-bottom: 8px;",
            if (i == 1) "Primary Author" else paste("Author", i)
          ),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
            textInput(
              ns(paste0("zenFirstName_", i)),
              "First name*",
              placeholder = "e.g., Jane"
            ),
            textInput(
              ns(paste0("zenLastName_", i)),
              "Last name*",
              placeholder = "e.g., Smith"
            )
          ),
          textInput(
            ns(paste0("zenAffiliation_", i)),
            "Affiliation/Institution*",
            placeholder = "e.g., University of Example"
          ),
          textInput(
            ns(paste0("zenAuthorOrcid_", i)),
            "ORCID iD",
            placeholder = "e.g., 0000-0002-1825-0097"
          ),
          tags$small(
            style = "color: #6c757d; display: block; margin-top: -10px; margin-bottom: 5px;",
            icon("question-circle"),
            " Don't have an ORCID? ",
            tags$a(
              href = "https://orcid.org/register",
              target = "_blank",
              "Register here"
            )
          )
        )
      })
    })

    # Observers for each remove button — recreated whenever author_count changes
    observe({
      n <- author_count()
      lapply(seq_len(n), function(i) {
        if (i == 1) {
          return()
        } # no remove button on row 1
        btn_id <- paste0("removeAuthor_", i)
        observeEvent(
          input[[btn_id]],
          {
            new_n <- author_count() - 1
            if (new_n < 1) {
              return()
            }
            # Shift all rows above i down by one to fill the gap
            for (j in i:new_n) {
              updateTextInput(
                session,
                paste0("zenFirstName_", j),
                value = input[[paste0("zenFirstName_", j + 1)]] %||% ""
              )
              updateTextInput(
                session,
                paste0("zenLastName_", j),
                value = input[[paste0("zenLastName_", j + 1)]] %||% ""
              )
              updateTextInput(
                session,
                paste0("zenAffiliation_", j),
                value = input[[paste0("zenAffiliation_", j + 1)]] %||% ""
              )
              updateTextInput(
                session,
                paste0("zenAuthorOrcid_", j),
                value = input[[paste0("zenAuthorOrcid_", j + 1)]] %||% ""
              )
            }
            author_count(new_n)
          },
          ignoreInit = TRUE,
          once = TRUE
        )
      })
    })

    # Collect all author rows into a structured list
    all_authors <- reactive({
      n <- author_count()
      lapply(seq_len(n), function(i) {
        list(
          first = input[[paste0("zenFirstName_", i)]] %||% "",
          last = input[[paste0("zenLastName_", i)]] %||% "",
          affiliation = input[[paste0("zenAffiliation_", i)]] %||% "",
          orcid = input[[paste0("zenAuthorOrcid_", i)]] %||% ""
        )
      })
    })

    # ═════════════════════════════════════════════════════════════════════════
    # README generator
    # ═════════════════════════════════════════════════════════════════════════

    generateReadme <- reactive({
      authors <- all_authors()
      current_year <- format(Sys.Date(), "%Y")
      license_display <- if (input$zenLicense == "cc-by-4.0") {
        "CC BY 4.0"
      } else {
        input$zenLicense
      }

      # Per-author block shown in the Authors section
      author_lines <- vapply(
        seq_along(authors),
        function(i) {
          a <- authors[[i]]
          name <- paste(
            if (nzchar(a$first)) a$first else "[First]",
            if (nzchar(a$last)) a$last else "[Last]"
          )
          label <- if (i == 1) "Primary Author" else paste0("Author ", i)
          parts <- c(
            paste0("**", label, ":** ", name),
            if (nzchar(a$affiliation)) {
              paste0("  **Institution:** ", a$affiliation)
            },
            if (nzchar(a$orcid)) paste0("  **ORCID:** ", a$orcid)
          )
          paste(parts, collapse = "\n")
        },
        character(1)
      )

      # Citation line — "Smith J., Jones A., …"
      citation_authors <- paste(
        vapply(
          authors,
          function(a) {
            ln <- if (nzchar(a$last)) a$last else "[Last]"
            fn <- if (nzchar(a$first)) substr(a$first, 1, 1) else ""
            if (nzchar(fn)) paste0(ln, " ", fn, ".") else ln
          },
          character(1)
        ),
        collapse = ", "
      )

      # Contact block (uses primary author ORCID / affiliation)
      contact_parts <- c()
      if (nzchar(input$contactName)) {
        contact_parts <- c(
          contact_parts,
          paste0("**Name:** ", input$contactName)
        )
      }
      if (nzchar(input$contactEmail)) {
        contact_parts <- c(
          contact_parts,
          paste0("**Email:** ", input$contactEmail)
        )
      }
      if (nzchar(authors[[1]]$orcid)) {
        contact_parts <- c(
          contact_parts,
          paste0("**ORCID:** ", authors[[1]]$orcid)
        )
      }
      if (nzchar(authors[[1]]$affiliation)) {
        contact_parts <- c(
          contact_parts,
          paste0("**Institution:** ", authors[[1]]$affiliation)
        )
      }
      contact_section <- if (length(contact_parts)) {
        paste(contact_parts, collapse = "\n- ")
      } else {
        "[Contact information]"
      }

      dataset_title <- if (nzchar(input$zenTitle)) {
        input$zenTitle
      } else {
        "[Dataset Title]"
      }
      emod_url <- if (nzchar(input$emodnetUrl)) input$emodnetUrl else "#"
      emod_label <- if (nzchar(input$emodnetUrl)) input$emodnetUrl else "[URL]"

      sprintf(
        "# Metadata for %s

### Access the data via EMODnet: [%s](%s)

- **Project:** Partnership for the Assessment of Risks from Chemicals (PARC)
- **EU funding:** HORIZON.2.6
- **Project DOI:** [10.3030/101057014](https://cordis.europa.eu/project/id/101057014)
- **Zenodo DOI:** (Will be generated upon upload)

---

## What is this?

This Zenodo record provides metadata for the dataset **%s**.

%s

---

## Authors

%s

---

## How to cite

%s (%s). *%s*. Zenodo. https://doi.org/[DOI-will-be-generated]

---

## License

Released under **%s**.

---

## Contact

- %s",
        dataset_title,
        emod_label,
        emod_url,
        dataset_title,
        if (nzchar(input$zenDescription)) input$zenDescription else "",
        paste(author_lines, collapse = "\n\n"),
        citation_authors,
        current_year,
        dataset_title,
        license_display,
        contact_section
      )
    })

    # ═════════════════════════════════════════════════════════════════════════
    # README preview & copy
    # ═════════════════════════════════════════════════════════════════════════

    output$readmePreview <- renderUI({
      readme_content <- generateReadme()
      if (input$viewMode == "rich") {
        div(
          class = "preview-box",
          style = "background-color: #ffffff; border: 1px solid #e5e7eb; border-radius: 0.5rem; padding: 1.5rem; max-height: 400px; overflow-y: auto;",
          HTML(markdown::mark_html(text = readme_content, template = FALSE))
        )
      } else {
        div(
          class = "preview-box",
          style = "font-family: monospace; white-space: pre-wrap; background: #f8fafc; color: #1e293b; padding: 1rem; border: 1px solid #e5e7eb; border-radius: 0.5rem; max-height: 400px; overflow-y: auto;",
          readme_content
        )
      }
    })

    observeEvent(input$copyReadme, {
      session$sendCustomMessage("copyToClipboard", generateReadme())
      showNotification(
        "README copied to clipboard!",
        type = "message",
        duration = 2
      )
    })

    # ═════════════════════════════════════════════════════════════════════════
    # Environment UI observers (unchanged from original)
    # ═════════════════════════════════════════════════════════════════════════

    observe({
      if (input$zenEnvironment) {
        shinyjs::html("headerBadge", "TEST MODE")
        shinyjs::runjs(sprintf(
          "$('#%s').css({'background-color': '#ffc107', 'color': '#000'});",
          ns("headerBadge")
        ))
      } else {
        shinyjs::html("headerBadge", "PRODUCTION")
        shinyjs::runjs(sprintf(
          "$('#%s').css({'background-color': '#dc3545', 'color': '#fff'});",
          ns("headerBadge")
        ))
      }
    })

    output$environmentWarning <- renderUI({
      if (input$zenEnvironment) {
        div(
          style = "background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; padding: 10px; margin-top: 12px;",
          div(
            style = "display: flex; align-items: flex-start; gap: 8px;",
            icon("info-circle", style = "color: #0c5460; margin-top: 2px;"),
            tags$small(
              style = "color: #0c5460;",
              tags$strong("Sandbox mode:"),
              " Uploads are for testing only and will not be preserved. Perfect for trying things out!"
            )
          )
        )
      } else {
        div(
          style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; padding: 10px; margin-top: 12px;",
          div(
            style = "display: flex; align-items: flex-start; gap: 8px;",
            icon(
              "exclamation-triangle",
              style = "color: #721c24; margin-top: 2px;"
            ),
            tags$small(
              style = "color: #721c24;",
              tags$strong("Production mode:"),
              " You are uploading to the live Zenodo repository. Uploads will be ",
              tags$strong("permanent"),
              " and publicly accessible."
            )
          )
        )
      }
    })

    observe({
      shinyjs::html(
        "envLabel",
        if (input$zenEnvironment) "Sandbox (Testing)" else "Production (Live)"
      )
    })

    observe({
      if (input$zenEnvironment) {
        shinyjs::html("envBadge", "TEST MODE")
        shinyjs::runjs(sprintf(
          "$('#%s').css({'background-color': '#ffc107', 'color': '#000'});",
          ns("envBadge")
        ))
      } else {
        shinyjs::html("envBadge", "LIVE")
        shinyjs::runjs(sprintf(
          "$('#%s').css({'background-color': '#dc3545', 'color': '#fff'});",
          ns("envBadge")
        ))
      }
    })

    # ═════════════════════════════════════════════════════════════════════════
    # Form submission
    # ═════════════════════════════════════════════════════════════════════════

    observeEvent(input$submitZen, {
      is_upload_mode <- identical(input$inputMode, "upload")
      is_template_mode <- identical(input$inputMode, "template")

      # Check every author row has at minimum first name, last name, affiliation
      author_errors <- vapply(
        seq_len(author_count()),
        function(i) {
          fn <- input[[paste0("zenFirstName_", i)]] %||% ""
          ln <- input[[paste0("zenLastName_", i)]] %||% ""
          aff <- input[[paste0("zenAffiliation_", i)]] %||% ""
          fn == "" || ln == "" || aff == ""
        },
        logical(1)
      )

      required_missing <- c(
        input$zenTitle == "",
        input$zenDescription == "",
        input$contactEmail == "",
        any(author_errors),
        is_upload_mode && is.null(input$zenUpload),
        is_template_mode &&
          (is.null(input$emodnetUrl) || input$emodnetUrl == "")
      )

      if (any(required_missing)) {
        showNotification(
          paste(
            "Please fill in all required fields marked with *",
            if (any(author_errors)) {
              "(all authors need first name, last name and affiliation)"
            } else {
              ""
            },
            if (is_upload_mode && is.null(input$zenUpload)) {
              "(upload mode requires a file)"
            } else {
              ""
            },
            if (
              is_template_mode &&
                (is.null(input$emodnetUrl) || input$emodnetUrl == "")
            ) {
              "(template mode requires an EMODnet URL)"
            } else {
              ""
            }
          ),
          type = "error",
          duration = 5
        )
        return()
      }

      # Production confirmation modal
      if (!input$zenEnvironment) {
        showModal(modalDialog(
          title = tags$div(
            style = "display: flex; align-items: center; gap: 8px; color: #dc3545;",
            icon("exclamation-triangle", style = "font-size: 1.2em;"),
            "Confirm Production Upload"
          ),
          tags$div(
            tags$p(
              style = "font-weight: 500; margin-bottom: 10px;",
              "You are about to upload to the LIVE Zenodo repository."
            ),
            tags$p(
              "This upload will be ",
              tags$strong("permanent"),
              " and ",
              tags$strong("publicly accessible"),
              ". Are you sure you want to proceed?"
            ),
            tags$hr(),
            tags$p(
              style = "font-size: 0.9em; color: #6c757d;",
              icon("lightbulb"),
              " Tip: Use Sandbox mode to test your upload first."
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirmSubmit"),
              "Yes, Upload to Production",
              class = "btn-danger",
              icon = icon("check")
            )
          ),
          easyClose = FALSE,
          size = "m"
        ))
        return()
      }

      performUpload()
    })

    observeEvent(input$confirmSubmit, {
      removeModal()
      performUpload()
    })

    # ═════════════════════════════════════════════════════════════════════════
    # Upload function
    # ═════════════════════════════════════════════════════════════════════════

    performUpload <- function() {
      zenodo <- current_zenodo()
      env_name <- if (input$zenEnvironment) "Sandbox" else "Production"

      tryCatch(
        {
          showNotification(
            paste0("Uploading to Zenodo ", env_name, "..."),
            type = "message",
            duration = NULL,
            id = "upload_notification"
          )
        },
        error = function(e) {
          showNotification(
            paste("Failed to start upload:", e$message),
            type = "error",
            duration = 10
          )
          stop(e)
        }
      )

      # Build record
      myrec <- ZenodoRecord$new()
      myrec$setTitle(input$zenTitle)
      myrec$setDescription(input$zenDescription)
      myrec$setResourceType(input$zenResourceType)

      # Add every author as a creator
      authors <- all_authors()
      for (auth in authors) {
        if (nzchar(auth$first) && nzchar(auth$last)) {
          myrec$addCreator(
            firstname = auth$first,
            lastname = auth$last,
            affiliation = if (nzchar(auth$affiliation)) {
              auth$affiliation
            } else {
              NULL
            },
            orcid = if (nzchar(auth$orcid)) auth$orcid else NULL
          )
        }
      }

      myrec$setLicense(input$zenLicense)
      myrec$setPublisher("Zenodo")
      myrec$setPublicationDate(Sys.Date())

      # Deposit
      myrec <- zenodo$depositRecord(myrec)

      # Upload data file (required in upload mode, optional otherwise)
      if (!is.null(input$zenUpload)) {
        zenodo$uploadFile(input$zenUpload$datapath, myrec)
      }

      # Upload README only in template mode
      if (identical(input$inputMode, "template")) {
        readme_content <- generateReadme()
        readme_temp <- tempfile(fileext = ".md")
        writeLines(readme_content, readme_temp)
        zenodo$uploadFile(readme_temp, myrec)
        unlink(readme_temp)
      }

      # Submit for community review
      msg <- paste0(
        "Submitted by: ",
        input$contactEmail,
        "\n\n",
        if (!is.null(input$zenComment) && nzchar(input$zenComment)) {
          input$zenComment
        } else {
          "No additional comments"
        }
      )

      zenodo$createReviewRequest(myrec, "stop-test")
      ok <- zenodo$submitRecordForReview(myrec$id, msg)

      removeNotification("upload_notification")

      record_id <- if (!is.null(myrec$id)) as.character(myrec$id) else ""
      tail_msg <- if (!input$zenEnvironment) {
        paste0("<small>Record ID: ", record_id, "</small>")
      } else {
        "<small>This is a test upload and will not be preserved.</small>"
      }

      if (!isTRUE(ok)) {
        showNotification(
          "Upload successful, but submission for review failed. Please check your Zenodo account.",
          type = "warning",
          duration = 10
        )
      } else {
        showNotification(
          HTML(paste0(
            "<strong>Success!</strong><br>",
            "Your data ",
            if (identical(input$inputMode, "template")) "and README " else "",
            "have been uploaded to Zenodo ",
            env_name,
            ".<br>",
            tail_msg
          )),
          type = "message",
          duration = 10
        )
      }
    }
  })
}

## To be copied in the UI
# mod_Zenodo_ui("zenodo_1")

## To be copied in the server
# mod_Zenodo_server("zenodo_1")
