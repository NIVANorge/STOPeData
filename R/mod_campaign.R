# Campaign Import Module ----
# A Shiny module for campaign data entry with validation using shinyvalidate

#' Campaign UI Function ----
#' @noRd
#'
#' @importFrom shiny NS tagList textInput dateInput selectInput textAreaInput actionButton
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel tooltip
#' @importFrom bsicons bs_icon
#' @importFrom tibble tibble
#' @import eDataDRF
mod_campaign_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main input card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_campaign.md"),
        ## Input fields layout ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### CAMPAIGN_NAME - Required string, 100 char ----
          textInput(
            inputId = ns("CAMPAIGN_NAME"),
            label = tooltip(
              list("Campaign Name", bs_icon("info-circle-fill")),
              "Text string used to identify the sampling campaign or project. Ensure a consistent Campaign string is used."
            ),
            placeholder = "e.g., 'Vannmiljo Mitigation Monitoring 2025'",
            width = "100%"
          ),

          ### CAMPAIGN_NAME_SHORT - Required string, 40 char ----
          textInput(
            inputId = ns("CAMPAIGN_NAME_SHORT"),
            label = tooltip(
              list("Campaign Name Short", bs_icon("info-circle-fill")),
              "Abbreviated campaign identifier (max 40 characters). Use for compact displays and references."
            ),
            placeholder = "e.g., 'Vm_Tilt_2025'",
            width = "100%"
          ),

          ### ORGANISATION - Required string, 50 char ----
          textInput(
            inputId = ns("ORGANISATION"),
            label = tooltip(
              list("Organisation", bs_icon("info-circle-fill")),
              "The principal organisation(s) responsible for collecting or creating the original data: authors' institution, report publishing institution, etc."
            ),
            placeholder = "Data collection organisation",
            width = "100%"
          ),

          ### CAMPAIGN_START_DATE - Required date ----
          dateInput(
            inputId = ns("CAMPAIGN_START_DATE"),
            label = tooltip(
              list("Campaign Start Date", bs_icon("info-circle-fill")),
              "The official or actual date of first sampling. Use ISO date format (YYYY-MM-DD)."
            ),
            value = as.Date(NA),
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          ### CAMPAIGN_END_DATE - Optional date ----
          dateInput(
            inputId = ns("CAMPAIGN_END_DATE"),
            label = tooltip(
              list("Campaign End Date", bs_icon("info-circle-fill")),
              "The latest date of sampling or analysis. Use ISO date format (YYYY-MM-DD)."
            ),
            value = as.Date(NA),
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          ### RELIABILITY_EVAL_SYS - Optional string, 50 char ----
          selectInput(
            inputId = ns("RELIABILITY_EVAL_SYS"),
            label = tooltip(
              list(
                "Reliability Evaluation System",
                bs_icon("info-circle-fill")
              ),
              "The system used to evaluate data quality."
            ),
            choices = c(
              "Not relevant",
              "Not reported",
              "CREED",
              "Other (add to comments)"
            ),
            width = "100%"
          ),

          ### RELIABILITY_SCORE - Optional int, 22 char ----
          textInput(
            inputId = ns("RELIABILITY_SCORE"),
            label = tooltip(
              list("Reliability Score", bs_icon("info-circle-fill")),
              "The score given (numeric or categorical) under the Reliability Evaluation System, if relevant."
            ),
            value = NA,
            placeholder = "Numeric or categorical",
            width = "100%"
          ),

          ### CONFIDENTIALITY_EXPIRY_DATE - Optional date ----
          dateInput(
            inputId = ns("CONFIDENTIALITY_EXPIRY_DATE"),
            label = tooltip(
              list("Confidentiality Expiry Date", bs_icon("info-circle-fill")),
              "The date at which the data leaves embargo or ceases to be confidential. Use ISO date format (YYYY-MM-DD)."
            ),
            value = NA,
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(), # suppress date NA warning

          ### ENTERED_BY - Required string, 50 char ----
          textInput(
            inputId = ns("ENTERED_BY"),
            label = tooltip(
              list("Entered By", bs_icon("info-circle-fill")),
              "Your name."
            ),
            placeholder = "Your name",
            width = "100%"
          ),

          ### ENTERED_DATE - Required date ----
          dateInput(
            inputId = ns("ENTERED_DATE"),
            label = tooltip(
              list("Entered Date", bs_icon("info-circle-fill")),
              "The date you are entering this data into the app. Use ISO date format (YYYY-MM-DD)."
            ),
            value = Sys.Date(),
            format = "yyyy-mm-dd",
            width = "100%"
          )
        ),

        ### CAMPAIGN_COMMENT - Full width text area ----
        textAreaInput(
          inputId = ns("CAMPAIGN_COMMENT"),
          label = tooltip(
            list("Campaign Comments", bs_icon("info-circle-fill")),
            "Use this space to enter any potentially relevant or noteworthy comments or remarks about the overall campaign."
          ),
          placeholder = "Campaign-level notes (optional)",
          width = "100%",
          rows = 3
        ),

        ## Validation status and raw data ----
        span(
          # prevent flex-grow validation element from growing vertically
          uiOutput(ns("validation_reporter"))
        ),
        accordion(
          id = ns("data_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Click to view raw validated data",
            icon = bs_icon("code"),
            verbatimTextOutput(ns("validated_data_display"))
          )
        ),

        ## Action buttons ----
        actionButton(
          inputId = ns("clear"),
          label = "Clear All Fields",
          class = "btn-danger",
          width = "300px"
        )
      )
    )
  )
}

#' Campaign Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText updateTextInput updateDateInput updateNumericInput updateTextAreaInput bindEvent
#' @importFrom glue glue
#' @importFrom tibble add_row
#' @import eDataDRF
mod_campaign_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## InputValidator$new: iv ----
    iv <- InputValidator$new()
    iv$add_rule("CAMPAIGN_NAME", sv_required())
    iv$add_rule("CAMPAIGN_NAME", function(value) {
      if (isTruthy(value) && nchar(value) > 150) {
        "Campaign Name must be 150 characters or less"
      }
    })

    iv$add_rule("CAMPAIGN_START_DATE", sv_required())

    iv$add_rule("ENTERED_BY", sv_required())

    iv$add_rule("ORGANISATION", sv_required())
    iv$add_rule("ORGANISATION", function(value) {
      if (isTruthy(value) && nchar(value) > 100) {
        "Organisation must be 100 characters or less"
      }
    })

    iv$add_rule("ENTERED_DATE", sv_required())

    # Conditional validation for reliability score
    iv$add_rule("RELIABILITY_SCORE", function(value) {
      if (
        isTruthy(input$RELIABILITY_EVAL_SYS) &&
          isRelevant(input$RELIABILITY_EVAL_SYS) &&
          !isTruthy(value)
      ) {
        "If an evaluation system is selected a score must also be entered."
      }
    })

    iv$add_rule("CAMPAIGN_COMMENT", function(value) {
      if (isTruthy(value) && nchar(value) > 1000) {
        "Campaign Comment must be 1000 characters or less"
      }
    })

    # Date validation for end date
    iv$add_rule("CAMPAIGN_END_DATE", function(value) {
      if (isTruthy(value) && isTruthy(input$CAMPAIGN_START_DATE)) {
        if (value < input$CAMPAIGN_START_DATE) {
          "Campaign End Date must be after Start Date"
        }
      }
    })

    # CAMPAIGN_NAME_SHORT is short
    iv$add_rule("CAMPAIGN_NAME_SHORT", sv_required())
    iv$add_rule("CAMPAIGN_NAME_SHORT", function(value) {
      if (isTruthy(value) && nchar(value) > 40) {
        "Campaign Name Short must be 40 characters or less"
      }
    })

    ## InputValidator$enable() ----
    iv$enable()

    # 2. Observers and Reactives ----

    ## observe: check validation status and send to session$userData ----
    # imports: fct_formats::initialise_campaign_tibble()
    # upstream: iv
    # downstream: session$userData$reactiveValues$campaignData, campaignDataValid
    observe({
      tryCatch(
        {
          # if (iv$is_valid()) {
          # TODO: Strictly speaking this shouldn't be conditional on validity, because
          # we claim that data is saved even when invalid. But CAMPAIGN is almost always valid
          # and I don't want to disable it just yet
          # Collect validated data
          validated_data <- tryCatch(
            {
              # uses standardised format from fct_formats, will fail if format/data types not respected
              initialise_campaign_tibble() |>
                add_row(
                  CAMPAIGN_NAME_SHORT = input$CAMPAIGN_NAME_SHORT,
                  CAMPAIGN_NAME = input$CAMPAIGN_NAME,
                  CAMPAIGN_START_DATE = input$CAMPAIGN_START_DATE %|truthy|%
                    as.Date(NA),
                  CAMPAIGN_END_DATE = input$CAMPAIGN_END_DATE %|truthy|%
                    as.Date(NA),
                  RELIABILITY_SCORE = input$RELIABILITY_SCORE %|truthy|% NA,
                  RELIABILITY_EVAL_SYS = input$RELIABILITY_EVAL_SYS %|truthy|%
                    NA,
                  CONFIDENTIALITY_EXPIRY_DATE = input$CONFIDENTIALITY_EXPIRY_DATE %|truthy|%
                    as.Date(NA),
                  ORGANISATION = input$ORGANISATION,
                  ENTERED_BY = input$ENTERED_BY,
                  ENTERED_DATE = input$ENTERED_DATE,
                  CAMPAIGN_COMMENT = input$CAMPAIGN_COMMENT %|truthy|% NA
                )
            },
            error = function(e) {
              stop(
                "Column mismatch in campaign data collection: ",
                e$message,
                call. = FALSE
              )
            }
          )

          # set module true at server level if validation passes
          if (isTRUE(iv$is_valid())) {
            session$userData$reactiveValues$campaignDataValid <- TRUE
          }

          # CHANGED: Store directly to userData
          session$userData$reactiveValues$campaignData <- validated_data

          # } else {
          # CHANGED: Set validation flag to FALSE
          # session$userData$reactiveValues$campaignDataValid <- FALSE
          # }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error validating campaign data: ",
              e$message,
              " (Code: mod_campaign_validate_data)"
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
              " (Code: mod_campaign_validate_data)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_campaign_validate_data",
        input$CAMPAIGN_NAME,
        input$CAMPAIGN_NAME_SHORT,
        input$CAMPAIGN_START_DATE,
        input$CAMPAIGN_END_DATE,
        input$RELIABILITY_SCORE,
        input$RELIABILITY_EVAL_SYS,
        input$CONFIDENTIALITY_EXPIRY_DATE,
        input$ORGANISATION,
        input$ENTERED_BY,
        input$ENTERED_DATE,
        input$CAMPAIGN_COMMENT
      )

    ## observe ~ bindEvent: Clear fields button ----
    # upstream: user clicks input$clear
    # downstream: all input fields
    observe({
      tryCatch(
        {
          # Reset all inputs to default values
          updateTextInput(session, "CAMPAIGN_NAME_SHORT", value = "")
          updateTextInput(session, "CAMPAIGN_NAME", value = "")
          updateDateInput(session, "CAMPAIGN_START_DATE", value = as.Date(NA))
          updateDateInput(session, "CAMPAIGN_END_DATE", value = as.Date(NA))
          updateNumericInput(session, "RELIABILITY_SCORE", value = NA)
          updateTextInput(session, "RELIABILITY_EVAL_SYS", value = "")
          updateDateInput(
            session,
            "CONFIDENTIALITY_EXPIRY_DATE",
            value = as.Date(NA)
          )
          updateTextInput(session, "ORGANISATION", value = "")
          updateTextInput(session, "ENTERED_BY", value = "")
          updateDateInput(session, "ENTERED_DATE", value = Sys.Date())
          updateTextAreaInput(session, "CAMPAIGN_COMMENT", value = "")

          # CHANGED: Clear validation state in userData
          session$userData$reactiveValues$campaignData <- initialise_campaign_tibble()
          session$userData$reactiveValues$campaignDataValid <- FALSE
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error clearing fields: ",
              e$message,
              " (Code: mod_campaign_clear_fields)"
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
              " (Code: mod_campaign_clear_fields)"
            ),
            type = "warning",
            duration = 10
          )
        }
      ) |>
        suppressWarnings()
    }) |>
      bindEvent(
        label = "mod_campaign_clear_fields",
        input$clear
      )

    ## observe ~ bindEvent: Set session username from ENTERED_BY ----
    observe({
      tryCatch(
        {
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
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error saving username: ",
              e$message,
              " (Code: mod_campaign_save_username)"
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
              " (Code: mod_campaign_save_username)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_campaign_save_username",
        input$ENTERED_BY,
        ignoreInit = TRUE
      )

    ## observe: update ENTERED_BY field with user_id ----
    # upstream: session$userData$reactiveValues$ENTERED_BY
    # downstream: input$ENTERED_BY
    observe({
      tryCatch(
        {
          updateTextInput(
            session,
            "ENTERED_BY",
            value = session$userData$reactiveValues$ENTERED_BY
          )
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error updating ENTERED_BY field: ",
              e$message,
              " (Code: mod_campaign_update_entered_by)"
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
              " (Code: mod_campaign_update_entered_by)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_campaign_update_entered_by",
        session$userData$reactiveValues$ENTERED_BY
      )

    ## observe: Populate from LLM data when available ----
    # upstream: session$userData$reactiveValues$campaignDataLLM
    # downstream: input fields
    observe({
      tryCatch(
        {
          llm_data <- session$userData$reactiveValues$campaignDataLLM
          if (
            !is.null(llm_data) &&
              session$userData$reactiveValues$llmExtractionComplete
          ) {
            populate_campaign_from_llm(session, llm_data)

            # showNotification(
            #   "Campaign form populated.",
            #   type = "message"
            # )
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error populating from LLM data: ",
              e$message,
              " (Code: mod_campaign_populate_llm)"
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
              " (Code: mod_campaign_populate_llm)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_campaign_populate_llm",
        session$userData$reactiveValues$campaignDataLLM,
        session$userData$reactiveValues$llmExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    ## observer: receive data from session$userData$reactiveValues$campaignData (import) ----
    ## and update module inputs
    observe({
      tryCatch(
        {
          # CHANGED: Data is already in userData, just need to populate the form
          campaign_data <- session$userData$reactiveValues$campaignData |>
            as.list()
          names(campaign_data) <- tolower(names(campaign_data))
          # import data is SCREAMING_NAME but module expects snake_case, so we need to convert the list names

          populate_campaign_from_llm(
            session,
            campaign_data
          )
          print_dev("Populated campaign form from saved data")
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error loading saved data: ",
              e$message,
              " (Code: mod_campaign_load_saved_data)"
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
              " (Code: mod_campaign_load_saved_data)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_campaign_load_saved_data",
        session$userData$reactiveValues$saveExtractionComplete,
        session$userData$reactiveValues$saveExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    # 3. Outputs ----

    ## output: validation_reporter ----
    # upstream: session$userData$reactiveValues$campaignDataValid
    # downstream: UI update
    output$validation_reporter <- renderUI({
      tryCatch(
        {
          # CHANGED: Reference userData validation status
          if (session$userData$reactiveValues$campaignDataValid) {
            div(
              bs_icon("clipboard2-check"),
              "All data validated successfully.",
              class = "validation-status validation-complete"
            )
          } else {
            div(
              bs_icon("exclamation-triangle"),
              "Please ensure all required fields are filled, and all entered data is properly formatted.",
              class = "validation-status validation-warning"
            )
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error rendering validation status: ",
              e$message,
              " (Code: mod_campaign_validation_reporter)"
            ),
            type = "error",
            duration = NULL
          )
          return(NULL)
        },
        warning = function(w) {
          showNotification(
            paste0(
              "Warning: ",
              w$message,
              " (Code: mod_campaign_validation_reporter)"
            ),
            type = "warning",
            duration = 10
          )
          return(NULL)
        }
      )
    })

    ## output: validated_data_display ----
    # upstream: session$userData$reactiveValues$campaignData (when valid)
    # downstream: UI update
    output$validated_data_display <- renderText({
      tryCatch(
        {
          # CHANGED: Show data only when valid, reference userData
          if (
            session$userData$reactiveValues$campaignDataValid &&
              nrow(session$userData$reactiveValues$campaignData) > 0
          ) {
            printreactiveValues(session$userData$reactiveValues$campaignData)
          } else {
            "# Data object will be created when valid data is entered."
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error displaying validated data: ",
              e$message,
              " (Code: mod_campaign_validated_data_display)"
            ),
            type = "error",
            duration = NULL
          )
          return("# Error displaying data - see notification for details")
        },
        warning = function(w) {
          showNotification(
            paste0(
              "Warning: ",
              w$message,
              " (Code: mod_campaign_validated_data_display)"
            ),
            type = "warning",
            duration = 10
          )
          return(NULL)
        }
      )
    })
  })
}

## To be copied in the UI ----
# mod_campaign_ui("campaign_1")

## To be copied in the server ----
# campaign_data <- mod_campaign_server("campaign_1")
