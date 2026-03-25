#' Zenodo Upload UI Function
#'
#' @description A shiny Module for uploading datasets to Zenodo.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import zen4R
#' @importFrom bslib card card_body layout_columns accordion accordion_panel
#' @importFrom bsicons bs_icon
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinyjs html runjs
#' @importFrom glue glue
#' @importFrom stringr str_split str_trim
#' @importFrom purrr map walk2 map_chr
#' @importFrom dplyr arrange pull desc
mod_Zenodo_ui <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      full_screen = TRUE,
      card_body(
        fillable = FALSE,
        style = "padding-bottom: 80px;",

        # ===== Info accordion =====
        info_accordion(content_file = "inst/app/www/md/intro_zenodo.md"),

        hr(),

        div(
          id = "get-module-data-well",
          class = "navigation-buttons-container",
          style = "display: flex; align-items: start; margin: 0 5px 10px 5px;",
          actionButton(
            ns("get_session_data"),
            label = list(
              "Get Data from Modules",
              bs_icon("arrow-down-circle-fill")
            ),
            class = "btn-primary"
          )
        ),
        uiOutput(ns("session_data_summary")),

        hr(),

        # ===== 1. Dataset Information =====

        h5(bs_icon("file-earmark-text"), " 1. Dataset Information"),
        textInput(
          ns("zenTitle"),
          label = tooltip(
            list("Dataset Title", bs_icon("info-circle-fill")),
            "A descriptive title for your dataset on Zenodo."
          ),
          width = "900px",
          placeholder = "Enter a descriptive title for your dataset"
        ),
        textAreaInput(
          ns("zenDescription"),
          label = tooltip(
            list("Description", bs_icon("info-circle-fill")),
            "Explain what the dataset contains, how it was collected, and its scientific context."
          ),
          placeholder = "Provide a detailed description of your dataset",
          height = "200px",
          width = "900px"
        ),
        hr(),

        # ===== 2. Authors =====
        h5(bs_icon("person"), " 2. Authors"),
        p(
          style = "color: #6c757d; font-size: 0.9em;",
          "Author names are auto-filled from the Reference module where available. Affiliation and ORCID must be entered manually.",
          " Don't have an ORCID? ",
          tags$a(
            href = "https://orcid.org/register",
            target = "_blank",
            "Register here."
          )
        ),

        # Dynamic author rows
        uiOutput(ns("authorFields")),

        tooltip(
          actionButton(
            ns("addAuthor"),
            label = bs_icon("plus-circle"),
            class = "btn-outline-success"
          ),
          "Add another author."
        ),

        hr(),

        # ===== 3. Contact Information =====
        h5(bs_icon("envelope"), " 3. User Information"),
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,
          textInput(
            ns("contactName"),
            label = tooltip(
              list("Contact Name/Role", bs_icon("info-circle-fill")),
              "Name or role of the person to contact about this dataset, e.g. 'Dr. Jane Smith' or 'Data Curator'."
            ),
            width = "800px",

            placeholder = "e.g., Dr. Jane Smith or Data Curator",
          ),
          textInput(
            ns("contactEmail"),
            label = tooltip(
              list("Contact Email", bs_icon("info-circle-fill")),
              "Email address shown on the Zenodo record for data enquiries."
            ),
            width = "800px",

            placeholder = "contact@example.com",
          )
        ),

        hr(),

        # ===== 4. Metadata & Licensing =====
        h5(bs_icon("tags"), " 4. Metadata & Licensing"),
        layout_column_wrap(
          width = "400px",
          fill = FALSE,
          fillable = FALSE,
          selectizeInput(
            ns("zenResourceType"),
            label = tooltip(
              list("Resource type", bs_icon("info-circle-fill")),
              "The type of research output. Choose 'Dataset' for environmental monitoring data."
            ),
            choices = zenodo_resource_types,
            selected = "dataset"
          ),
          selectizeInput(
            ns("zenAccess"),
            label = tooltip(
              list("Access rights", bs_icon("info-circle-fill")),
              "Open access makes your data findable and reusable. Embargoed allows you to set a future release date."
            ),
            choices = c(
              "Open access" = "open",
              "Embargoed" = "embargoed",
              "Restricted" = "restricted",
              "Closed" = "closed"
            ),
            selected = "open"
          ),

          selectizeInput(
            ns("zenLicense"),
            label = tooltip(
              list("License", bs_icon("info-circle-fill")),
              "CC BY 4.0 is recommended for open research data \u2014 it allows reuse with attribution."
            ),
            choices = zenodo_licenses |>
              arrange(desc(popular)) |>
              pull(id, name = title),
            selected = "cc-by-4.0"
          )
        ),

        hr(),

        # ===== 5. Optional Information =====
        h5(
          style = "color: #6c757d;",
          bs_icon("three-dots"),
          " 5. Optional Information"
        ),
        layout_column_wrap(
          width = "400px",
          fill = FALSE,
          fillable = FALSE,
          textInput(
            ns("zenGrant"),
            label = tooltip(
              list("Grant agreement ID", bs_icon("info-circle-fill")),
              "EU or other funder grant agreement ID, e.g. 101057014 for EU Horizon PARC."
            ),
            placeholder = "e.g., 101057014"
          ),
          textAreaInput(
            ns("zenComment"),
            label = tooltip(
              list("Comment to curator", bs_icon("info-circle-fill")),
              "Optional note for the Zenodo community curator reviewing this submission."
            ),
            placeholder = "Any additional information (optional)",
            rows = 2
          )
        )
      ),

      actionButton(
        ns("previewReadme"),
        "Preview README",
        icon = bs_icon("file-text"),
        class = "btn-primary"
      ),

      # ===== Submit row =====
      div(
        style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 15px;",

        # Left: environment switch + badge
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          materialSwitch(
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
          ),
          tags$span(
            id = ns("envBadge"),
            style = "background-color: #ffc107; color: #000; padding: 4px 12px; border-radius: 20px; font-size: 0.75em; font-weight: bold; letter-spacing: 0.5px;",
            "TEST MODE"
          )
        ),

        # Right: Submit button
        div(
          style = "display: flex; gap: 10px; align-items: center;",
          input_task_button(
            ns("submitZen"),
            "Submit to Zenodo",
            color = "primary",
            style = "material-flat",
            icon = bs_icon("cloud-upload"),
            size = "md"
          )
        ),
        uiOutput(ns("environmentWarning"))
      )
    )
  )
}

#' Zenodo Server Functions
#'
#' @noRd
#' @importFrom purrr imap
#' @importFrom markdown markdownToHTML
mod_Zenodo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----

    ## Null-coalescing helper: returns a if non-NULL and non-empty, else b ----
    `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b

    ## Initialise Zenodo managers once per session ----
    # Two managers are created at startup (sandbox + production) so switching
    # environments does not require a round-trip to re-initialise.
    zenodo_sandbox <- ZenodoManager$new(
      url = "http://sandbox.zenodo.org/api",
      sandbox = TRUE,
      token = Sys.getenv("ZENSANDTOKEN"),
      logger = "DEBUG"
    )
    zenodo_production <- ZenodoManager$new(
      url = "https://zenodo.org/api",
      sandbox = FALSE,
      token = Sys.getenv("ZENTOKEN"),
      logger = "INFO"
    )

    ## current_zenodo reactive: returns the active ZenodoManager ----
    # Switches between sandbox and production based on the environment toggle.
    current_zenodo <- reactive({
      if (input$zenEnvironment) zenodo_sandbox else zenodo_production
    })

    ## author_count reactiveVal: tracks number of visible author rows ----
    # Declared early so it can be referenced in the author_iv rule closures below.
    author_count <- reactiveVal(1)

    ## InputValidator$new: author_iv ----
    # Child validator for dynamic author rows.
    # Rules are pre-registered for MAX_AUTHORS slots; each rule function checks
    # author_count() >= idx so that only active rows are validated.
    MAX_AUTHORS <- 20
    author_iv <- InputValidator$new()
    for (i in seq_len(MAX_AUTHORS)) {
      local({
        idx <- i
        author_iv$add_rule(paste0("zenFirstName_", idx), function(value) {
          if (author_count() >= idx && !isTruthy(value)) {
            "First name is required"
          }
        })
        author_iv$add_rule(paste0("zenLastName_", idx), function(value) {
          if (author_count() >= idx && !isTruthy(value)) "Last name is required"
        })
      })
    }

    ## InputValidator$new: iv ----
    # Parent validator for required form fields. author_iv is added as a child
    # so that iv$is_valid() covers both form fields and all active author rows.
    iv <- InputValidator$new()
    iv$add_rule("zenTitle", sv_required())
    iv$add_rule("zenDescription", sv_required())
    iv$add_rule("contactEmail", sv_required())

    ## iv$add_validator(author_iv) ----
    iv$add_validator(author_iv)

    ## iv$enable() ----
    iv$enable()

    # 2. Observers and Reactives ----

    ## observe: pre-fill title + description from upstream campaign data ----
    # upstream:  session$userData$reactiveValues$campaignData
    # downstream: input$zenTitle, input$zenDescription
    observe({
      tryCatch(
        {
          rv <- session$userData$reactiveValues
          campaign <- rv$campaignData

          if (is.null(campaign) || nrow(campaign) == 0) {
            return()
          }

          if (
            !nzchar(input$zenTitle %||% "") &&
              !is.na(campaign$CAMPAIGN_NAME[1]) &&
              nzchar(campaign$CAMPAIGN_NAME[1])
          ) {
            updateTextInput(
              session,
              "zenTitle",
              value = campaign$CAMPAIGN_NAME[1]
            )
          }

          # if campaign comment exists, add it
          if (
            !nzchar(input$zenDescription %||% "") &&
              !is.na(campaign$CAMPAIGN_COMMENT[1]) &&
              nzchar(campaign$CAMPAIGN_COMMENT[1] %||% "")
          ) {
            dataset_description <- campaign$CAMPAIGN_COMMENT[1]
          }

          # and if datasetDetails from CREED exists, add it on the end
          # if (
          #   !nzchar(input$zenDescription %||% "") &&
          #     !is.null(datasetDetails) &&
          #     nrow(datasetDetails) != 0
          # ) {
          #   dataset_description <- campaign$CAMPAIGN_COMMENT[1]
          # }

          updateTextAreaInput(
            session,
            "zenDescription",
            value = dataset_description
          )

          session$userData$reactiveValues$datasetDetails
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error pre-filling title/description: ",
              e$message,
              " (Code: mod_zenodo_prefill_title)"
            ),
            type = "error",
            duration = NULL
          )
        },
        warning = function(w) {
          showNotification(
            paste0("Warning: ", w$message, " (Code: mod_zenodo_prefill_title)"),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        session$userData$reactiveValues$campaignData,
        ignoreNULL = TRUE,
        label = "mod_zenodo_prefill_title"
      )

    ## observe: pre-fill contact email from ENTERED_BY ----
    # upstream:  session$userData$reactiveValues$ENTERED_BY
    # downstream: input$contactEmail
    observe({
      tryCatch(
        {
          entered_by <- session$userData$reactiveValues$ENTERED_BY
          if (
            length(entered_by) > 0 &&
              nzchar(entered_by) &&
              !nzchar(input$contactEmail)
          ) {
            updateTextInput(session, "contactEmail", value = entered_by)
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error pre-filling contact email: ",
              e$message,
              " (Code: mod_zenodo_prefill_contact)"
            ),
            type = "error",
            duration = NULL
          )
        },
        warning = function(w) {
          showNotification(
            paste0(
              "Warning: ",
              w$message,
              " (Code: mod_zenodo_prefill_contact)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        session$userData$reactiveValues$ENTERED_BY,
        ignoreNULL = TRUE,
        label = "mod_zenodo_prefill_contact"
      )

    ## observe: add an author row ----
    # upstream:  input$addAuthor (button)
    # downstream: author_count (incremented)
    observe({
      tryCatch(
        {
          author_count(author_count() + 1)
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error adding author row: ",
              e$message,
              " (Code: mod_zenodo_add_author)"
            ),
            type = "error",
            duration = NULL
          )
        }
      )
    }) |>
      bindEvent(
        input$addAuthor,
        ignoreInit = TRUE,
        label = "mod_zenodo_add_author"
      )

    ## observe: pre-fill author rows from referenceData$AUTHOR ("Last, First; ...") ----
    # upstream:  session$userData$reactiveValues$referenceData
    # downstream: author_count, zenFirstName_i / zenLastName_i inputs
    observe({
      tryCatch(
        {
          rv <- session$userData$reactiveValues
          ref <- rv$referenceData

          if (is.null(ref) || nrow(ref) == 0) {
            return()
          }
          if (is.na(ref$AUTHOR[1]) || !nzchar(ref$AUTHOR[1] %||% "")) {
            return()
          }
          # Only pre-fill if the first author's first name is still empty
          if (nzchar(input$zenFirstName_1 %||% "")) {
            return()
          }

          parsed <- str_split(ref$AUTHOR[1], ";")[[1]] |>
            str_trim() |>
            map(
              ~ {
                parts <- str_split(.x, ",")[[1]] |> str_trim()
                list(
                  last = if (length(parts) >= 1) parts[1] else "",
                  first = if (length(parts) >= 2) parts[2] else ""
                )
              }
            )

          author_count(length(parsed))

          # Wait for renderUI to process the new author_count before updating inputs
          # FIXME: Doesn't actually wait, so the names get silently written to the inputs before they exist (?)
          session$onFlushed(
            function() {
              walk2(
                parsed,
                seq_along(parsed),
                ~ {
                  browser()
                  updateTextInput(
                    session,
                    paste0("zenFirstName_", .y),
                    value = .x$first
                  )
                  updateTextInput(
                    session,
                    paste0("zenLastName_", .y),
                    value = .x$last
                  )
                }
              )
            },
            once = TRUE
          )
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error pre-filling authors: ",
              e$message,
              " (Code: mod_zenodo_prefill_authors)"
            ),
            type = "error",
            duration = NULL
          )
        },
        warning = function(w) {
          showNotification(
            paste0(
              "Warning: ",
              w$message,
              " (Code: mod_zenodo_prefill_authors)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        # I think currently gets triggered by username/email propogation
        (session$userData$reactiveValues$referenceDataValid &
          nrow(session$userData$reactiveValues$referenceData) == 1),
        ignoreNULL = TRUE,
        ignoreInit = TRUE,
        label = "mod_zenodo_prefill_authors"
      )

    ## output$authorFields: render one bordered row per author ----
    output$authorFields <- renderUI({
      n <- author_count()
      lapply(seq_len(n), function(i) {
        div(
          style = paste0(
            "border: 1px solid #dee2e6; border-radius: 6px; padding: 14px; margin-bottom: 12px;",
            if (n > 1) " position: relative;" else ""
          ),
          # Remove button shown on all rows except the first
          if (i > 1) {
            div(
              style = "position: absolute; top: 8px; right: 10px;",
              actionButton(
                ns(paste0("removeAuthor_", i)),
                NULL,
                label = bs_icon("x"),
                class = "btn-sm btn-outline-danger",
                style = "padding: 2px 8px; font-size: 0.75rem;",
                title = paste("Remove Author", i)
              )
            )
          },
          tags$small(
            style = "color: #6c757d; font-weight: 600; display: block; margin-bottom: 8px;",
            if (i == 1) "Primary Author" else paste("Author", i)
          ),
          layout_column_wrap(
            width = "300px",
            fill = FALSE,
            fillable = FALSE,
            style = "margin-bottom: 0px;",
            textInput(
              ns(paste0("zenFirstName_", i)),
              "First name",
              placeholder = "e.g., Jane"
            ),
            textInput(
              ns(paste0("zenLastName_", i)),
              "Last name",
              placeholder = "e.g., Smith"
            ),
            textInput(
              ns(paste0("zenAffiliation_", i)),
              "Affiliation/Institution",
              placeholder = "e.g., University of Example"
            ),
            textInput(
              ns(paste0("zenAuthorOrcid_", i)),
              tooltip(
                list("ORCID iD", bs_icon("info-circle-fill")),
                "A persistent digital identifier for researchers. Register free at orcid.org."
              ),
              placeholder = "e.g., 0000-0002-1825-0097"
            )
          )
        )
      })
    })

    ## observe: register remove-button observers for all non-first author rows ----
    # Re-runs whenever author_count changes so newly rendered remove buttons are wired up.
    # upstream:  author_count
    # downstream: author_count (decremented); shifts field values up when a row is removed
    observe({
      tryCatch(
        {
          n <- author_count()
          lapply(seq_len(n), function(i) {
            if (i == 1) {
              return()
            }
            observe({
              tryCatch(
                {
                  new_n <- author_count() - 1
                  if (new_n < 1) {
                    return()
                  }
                  # Shift every row above the removed index down by one slot
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
                error = function(e) {
                  showNotification(
                    paste0(
                      "Error removing author: ",
                      e$message,
                      " (Code: mod_zenodo_remove_author)"
                    ),
                    type = "error",
                    duration = NULL
                  )
                }
              )
            }) |>
              bindEvent(
                input[[paste0("removeAuthor_", i)]],
                ignoreInit = TRUE,
                once = TRUE,
                label = paste0("mod_zenodo_remove_author_", i)
              )
          })
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error registering remove-author observers: ",
              e$message,
              " (Code: mod_zenodo_remove_author_observers)"
            ),
            type = "error",
            duration = NULL
          )
        }
      )
    }) |>
      bindEvent(author_count(), label = "mod_zenodo_remove_author_observers")

    ## all_authors reactive: collect all active author rows into a structured list ----
    # upstream:  author_count, zenFirstName_i / zenLastName_i / zenAffiliation_i / zenAuthorOrcid_i
    # downstream: generateReadme, performUpload
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

    ## zenSessionState: reactiveValues tracking available datasets and export readiness ----
    zenSessionState <- reactiveValues(
      ready = FALSE,
      available_datasets = character(0),
      campaign_name = "Unknown_Campaign",
      dataset_dimensions = list()
    )

    ## observe: check session data on demand and populate zenSessionState ----
    # upstream:  input$get_session_data (button)
    # downstream: zenSessionState, output$session_data_summary
    observe({
      tryCatch(
        {
          rv <- session$userData$reactiveValues
          dataset_check <- check_available_datasets(rv)

          zenSessionState$available_datasets <- dataset_check$available_datasets
          zenSessionState$dataset_dimensions <- dataset_check$dataset_dimensions
          zenSessionState$ready <- dataset_check$export_ready

          campaign_name <- extract_campaign_name(rv$campaignData)
          if (!is.null(campaign_name)) {
            zenSessionState$campaign_name <- campaign_name
          }

          print_dev(glue(
            "mod_zenodo: {length(dataset_check$available_datasets)} datasets found: ",
            "{paste(dataset_check$available_datasets, collapse = ', ')}"
          ))
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error checking session data: ",
              e$message,
              " (Code: mod_zenodo_check_session_data)"
            ),
            type = "error",
            duration = NULL
          )
        },
        warning = function(w) {
          showNotification(
            paste0(
              "Warning: ",
              w$message,
              " (Code: mod_zenodo_check_session_data)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(input$get_session_data, label = "mod_zenodo_check_session_data")

    ## output$session_data_summary: badge + inline list of datasets ready for upload ----
    output$session_data_summary <- renderUI({
      if (zenSessionState$ready) {
        summaries <- map_chr(
          zenSessionState$available_datasets,
          ~ {
            dims <- zenSessionState$dataset_dimensions[[.x]]
            if (dims$type == "text") {
              glue("{.x}: {dims$chars} characters")
            } else {
              glue("{.x}: {dims$rows} \u00D7 {dims$cols}")
            }
          }
        )
        div(
          style = "margin-top: 12px;",
          div(
            bs_icon("check-circle"),
            glue(
              " {length(zenSessionState$available_datasets)} datasets ready for upload"
            ),
            class = "validation-status validation-complete",
            style = "display: inline-block; margin-bottom: 8px;"
          ),
          # Render summaries as code tags separated by commas
          tagList(
            imap(
              summaries,
              ~ tagList(
                tags$code(.x),
                if (.y < length(summaries)) ", "
              )
            )
          )
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          " Click 'Get Data from Modules' to auto-fill fields with relevant information.",
          class = "validation-status validation-warning",
          style = "display: inline-block; margin-top: 12px;"
        )
      }
    })

    ## generateReadme reactive: builds a markdown README string from current form inputs ----
    # upstream:  all form input$ fields, all_authors()
    # downstream: README preview modal, companion README file uploaded with the record
    generateReadme <- reactive({
      generate_zenodo_readme(
        title = input$zenTitle,
        description = input$zenDescription,
        authors = all_authors(),
        license_id = input$zenLicense,
        contact_name = input$contactName,
        contact_email = input$contactEmail
      )
    })

    ## observe: show README preview modal ----
    # upstream:  input$previewReadme (button)
    # Uses markdownToHTML(text = ...) to convert the README string to HTML directly,
    # avoiding the pre-wrap line-spacing issue that arose from treating rendered HTML
    # as plain text.
    observe({
      tryCatch(
        {
          showModal(modalDialog(
            title = tags$div(
              style = "display: flex; align-items: center; gap: 8px;",
              bs_icon("file-text"),
              "README Preview"
            ),
            size = "l",
            div(
              style = "background: #f8fafc; color: #1e293b; padding: 1rem; border: 1px solid #e5e7eb; border-radius: 0.5rem; max-height: 500px; overflow-y: auto; font-size: 0.85em;",
              HTML(markdownToHTML(
                text = generateReadme(),
                fragment.only = TRUE
              ))
            ),
            footer = modalButton("Close"),
            easyClose = TRUE
          ))
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error generating README preview: ",
              e$message,
              " (Code: mod_zenodo_preview_readme)"
            ),
            type = "error",
            duration = NULL
          )
        }
      )
    }) |>
      bindEvent(
        input$previewReadme,
        ignoreInit = TRUE,
        label = "mod_zenodo_preview_readme"
      )

    ## output$environmentWarning: informational banner reflecting the active environment ----
    output$environmentWarning <- renderUI({
      if (input$zenEnvironment) {
        div(
          style = "background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; padding: 10px; margin-top: 12px;",
          div(
            style = "display: flex; align-items: flex-start; gap: 8px;",
            bs_icon("info-circle", style = "color: #0c5460; margin-top: 2px;"),
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
            bs_icon(
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

    ## observe: update environment label text ----
    # upstream:  input$zenEnvironment (toggle)
    # downstream: envLabel span
    observe({
      tryCatch(
        {
          html(
            ns("envLabel"),
            if (input$zenEnvironment) {
              "Sandbox (Testing)"
            } else {
              "Production (Live)"
            }
          )
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error updating environment label: ",
              e$message,
              " (Code: mod_zenodo_env_label)"
            ),
            type = "error",
            duration = NULL
          )
        }
      )
    }) |>
      bindEvent(input$zenEnvironment, label = "mod_zenodo_env_label")

    ## observe: update environment badge colour and text ----
    # upstream:  input$zenEnvironment (toggle)
    # downstream: envBadge span (HTML + inline CSS via shinyjs)
    observe({
      tryCatch(
        {
          if (input$zenEnvironment) {
            html(ns("envBadge"), "TEST MODE")
            runjs(sprintf(
              "$('#%s').css({'background-color': '#ffc107', 'color': '#000'});",
              ns("envBadge")
            ))
          } else {
            html(ns("envBadge"), "LIVE")
            runjs(sprintf(
              "$('#%s').css({'background-color': '#dc3545', 'color': '#fff'});",
              ns("envBadge")
            ))
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error updating environment badge: ",
              e$message,
              " (Code: mod_zenodo_env_badge)"
            ),
            type = "error",
            duration = NULL
          )
        }
      )
    }) |>
      bindEvent(input$zenEnvironment, label = "mod_zenodo_env_badge")

    ## observe: validate form and show environment-aware confirmation modal on submit ----
    # upstream:  input$submitZen (task button)
    # downstream: showModal confirmation dialog
    # Author validation is now handled by author_iv (child of iv), so iv$is_valid()
    # covers both required fields and all active author rows.
    observe({
      tryCatch(
        {
          required_missing <- c(
            !iv$is_valid(),
            !zenSessionState$ready
          )

          if (any(required_missing)) {
            extra <- if (!zenSessionState$ready) {
              "(no session data \u2014 click 'Get Data from Modules' first)"
            } else {
              ""
            }
            showNotification(
              paste("Please fill in all required fields.", extra),
              type = "error",
              duration = 5
            )
            return()
          }

          # Always confirm before uploading — messaging differs by environment
          if (input$zenEnvironment) {
            modal_title <- tags$div(
              style = "display: flex; align-items: center; gap: 8px;",
              bs_icon("cloud-upload"),
              "Confirm Sandbox Upload"
            )
            modal_body <- tags$p(
              "Upload to the Zenodo ",
              tags$strong("sandbox (testing)"),
              " environment? Test uploads are not preserved and are safe to discard."
            )
            confirm_label <- "Yes, Upload to Sandbox"
            confirm_class <- "btn-primary"
          } else {
            modal_title <- tags$div(
              style = "display: flex; align-items: center; gap: 8px; color: #dc3545;",
              bs_icon("exclamation-triangle", style = "font-size: 1.2em;"),
              "Confirm Production Upload"
            )
            modal_body <- tags$p(
              style = "font-weight: 500;",
              "You are about to upload to the ",
              tags$strong("live Zenodo repository"),
              ". This will be ",
              tags$strong("permanent"),
              " and ",
              tags$strong("publicly accessible"),
              ". Use the \u2018Preview README\u2019 button to review the README before confirming."
            )
            confirm_label <- "Yes, Upload to Production"
            confirm_class <- "btn-danger"
          }

          showModal(modalDialog(
            title = modal_title,
            size = "m",
            modal_body,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                ns("confirmSubmit"),
                confirm_label,
                class = confirm_class,
                icon = bs_icon("check")
              )
            ),
            easyClose = FALSE
          ))
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error during submit validation: ",
              e$message,
              " (Code: mod_zenodo_submit)"
            ),
            type = "error",
            duration = NULL
          )
        }
      )
    }) |>
      bindEvent(input$submitZen, ignoreInit = TRUE, label = "mod_zenodo_submit")

    ## observe: close modal and trigger upload after user confirms ----
    # upstream:  input$confirmSubmit (modal action button)
    # downstream: performUpload()
    observe({
      tryCatch(
        {
          removeModal()
          performUpload()
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error starting upload: ",
              e$message,
              " (Code: mod_zenodo_confirm_submit)"
            ),
            type = "error",
            duration = NULL
          )
        }
      )
    }) |>
      bindEvent(
        input$confirmSubmit,
        ignoreInit = TRUE,
        label = "mod_zenodo_confirm_submit"
      )

    ## performUpload: build Zenodo record, upload files, and submit for community review ----
    # Called by the confirmSubmit observer.
    # Steps: build ZenodoRecord metadata → depositRecord → upload session ZIP →
    #        upload README.md → createReviewRequest → submitRecordForReview
    performUpload <- function() {
      tryCatch(
        {
          zenodo <- current_zenodo()
          env_name <- if (input$zenEnvironment) "Sandbox" else "Production"

          showNotification(
            paste0("Uploading to Zenodo ", env_name, "..."),
            type = "message",
            duration = NULL,
            id = "upload_notification"
          )

          # Build record metadata
          myrec <- ZenodoRecord$new()
          myrec$setTitle(input$zenTitle)
          myrec$setDescription(input$zenDescription)
          myrec$setResourceType(input$zenResourceType)

          for (auth in all_authors()) {
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

          # Deposit the record to create a draft on Zenodo
          myrec <- zenodo$depositRecord(myrec)

          # Upload session data as a ZIP archive
          session_zip <- tempfile(fileext = ".zip")
          on.exit(unlink(session_zip), add = TRUE)
          build_session_zip(session, zenSessionState, session_zip)
          zenodo$uploadFile(session_zip, myrec)

          # Upload a companion README.md alongside the data
          readme_temp <- tempfile(fileext = ".md")
          on.exit(unlink(readme_temp), add = TRUE)
          writeLines(generateReadme(), readme_temp)
          zenodo$uploadFile(readme_temp, myrec)

          # Submit record to the STOP community for curator review
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
                "Your data has been uploaded to Zenodo ",
                env_name,
                ".<br>",
                tail_msg
              )),
              type = "message",
              duration = 10
            )
          }
        },
        error = function(e) {
          removeNotification("upload_notification")
          showNotification(
            paste0("Upload failed: ", e$message, " (Code: mod_zenodo_upload)"),
            type = "error",
            duration = NULL
          )
        },
        warning = function(w) {
          showNotification(
            paste0(
              "Warning during upload: ",
              w$message,
              " (Code: mod_zenodo_upload)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }
  })
}

## To be copied in the UI
# mod_Zenodo_ui("zenodo_1")

## To be copied in the server
# mod_Zenodo_server("zenodo_1")
