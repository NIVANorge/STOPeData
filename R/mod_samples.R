# Samples Import Module ----
# A Shiny module for sample combinations of sites, parameters, and compartments

#' Samples UI Function ----
#' @noRd
#'
#' @importFrom shiny NS tagList dateInput actionButton
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinyWidgets airDatepickerInput pickerInput
#' @importFrom golem get_golem_wd
#' @import eDataDRF
mod_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs ----
    useShinyjs(),

    # Main content card ----
    card(
      fill = TRUE,
      full_screen = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_samples.md"),

        ## Sample combination form ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ## Sites selection ----
          div(
            pickerInput(
              inputId = ns("sites_select"),
              label = tooltip(
                list("Sites", bs_icon("info-circle-fill")),
                "Select one or more sampling sites"
              ),
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "No sites available - add sites first",
                `actions-box` = TRUE
              ),
              width = "100%"
            )
          ),

          ## Parameters selection ----
          div(
            pickerInput(
              inputId = ns("parameters_select"),
              label = tooltip(
                list("Parameters", bs_icon("info-circle-fill")),
                "Select one or more parameters to measure"
              ),
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "No parameters available - add parameters first",
                `actions-box` = TRUE
              ),
              width = "100%"
            )
          ),

          ## Compartments selection ----
          div(
            pickerInput(
              inputId = ns("compartments_select"),
              label = tooltip(
                list("Compartments", bs_icon("info-circle-fill")),
                "Select one or more environmental compartment combinations"
              ),
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "No compartments available - add compartments first",
                `actions-box` = TRUE
              ),
              width = "100%"
            )
          ),

          ## Sampling dates ----
          div(
            airDatepickerInput(
              inputId = ns("sampling_date"),
              label = tooltip(
                list("Sampling Dates", bs_icon("info-circle-fill")),
                "Dates when samples were collected. The selector here is a bit unreliable, so please deselect using the calendar picker or Remove All button, not Backspace"
              ),
              dateFormat = "yyyy-MM-dd",
              clearButton = TRUE,
              width = "100%",
              multiple = TRUE,
              todayButton = TRUE,
              update_on = "change",
              addon = "none"
            )
          ),

          ## Subsampling index ----
          textAreaInput(
            inputId = ns("subsample"),
            label = tooltip(
              list("Subsample Indices", bs_icon("info-circle-fill")),
              "If samples are split (replicates or e.g. core depths), enter values separated by commas."
            ),
            placeholder = "Comma-separated subsample identifiers (e.g., '1, 2, 3' or '1cm, 2cm, 3cm')",
            width = "100%",
            value = 1,
            rows = 1
          )
        ),

        ## Generate combinations button and validation status ----
        div(
          style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 15px 0;",

          input_task_button(
            id = ns("generate_combinations"),
            label = "Generate Sample Combinations",
            icon = icon("magic"),
            class = "btn-success",
            width = "250px"
          ) |>
            disabled(),

          ### Preview combination count ----
          uiOutput(ns("combination_preview"))
        ),

        ## Validation info ----
        uiOutput(ns("validation_reporter")),

        ## Raw data accordion ----
        accordion(
          id = ns("data_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Click to view raw validated data",
            icon = bs_icon("code"),
            verbatimTextOutput(ns("validated_data_display"))
          )
        )
      )
    ),

    ## Samples table card ----
    card(
      full_screen = TRUE,
      div(
        rHandsontableOutput(ns("samples_table")),
        style = "margin-bottom: 10px;"
      )
    )
  )
}

#' Samples Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_context_menu hot_col
#' @importFrom shinyjs enable disable disabled
#' @importFrom shinyWidgets updateAirDateInput updatePickerInput
#' @importFrom purrr is_empty
#' @importFrom glue glue
#' @importFrom tibble as_tibble add_row
#' @import eDataDRF

mod_samples_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    # CHANGED: Keep only UI-specific caches and transient state here
    moduleState <- reactiveValues(
      available_sites = NULL,
      available_parameters = NULL,
      available_compartments = NULL
    )

    ## InputValidator for table-level validation ----
    iv <- InputValidator$new()
    iv$add_rule("samples_table_validation", function(value) {
      if (nrow(session$userData$reactiveValues$samplesData) == 0) {
        "At least one sample combination must be generated"
      } else {
        # Check required fields
        required_fields <- c(
          "SITE_CODE",
          "PARAMETER_NAME",
          "ENVIRON_COMPARTMENT",
          "ENVIRON_COMPARTMENT_SUB",
          "MEASURED_CATEGORY",
          "SUBSAMPLE",
          "SAMPLE_ID"
        )

        for (i in 1:nrow(session$userData$reactiveValues$samplesData)) {
          for (field in required_fields) {
            value <- session$userData$reactiveValues$samplesData[i, field]
            if (is.na(value) || value == "" || is_empty(value)) {
              return(paste("Row", i, "is missing required field:", field))
            }
          }
        }
        NULL # All validations passed
      }
    })
    iv$enable()

    # 2. Observers and Reactives ----

    ## observe: Update sites selectize choices ----
    # upstream: session$userData$reactiveValues$sitesData
    # downstream: sites selectize input choices and moduleState$available_sites
    observe({
      tryCatch(
        {
          if (!is.null(session$userData$reactiveValues$sitesData)) {
            update_sites_selectize(
              session,
              session$userData$reactiveValues$sitesData
            )
            moduleState$available_sites <- session$userData$reactiveValues$sitesData
          } else {
            update_sites_selectize(session, dummy_sites)
            moduleState$available_sites <- dummy_sites
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error updating sites: ",
              e$message,
              " (Code: mod_samples_update_sites_selectize)"
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
              " (Code: mod_samples_update_sites_selectize)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_update_sites_selectize",
        session$userData$reactiveValues$sitesData,
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )

    ## observe: Update parameters selectize choices ----
    # upstream: session$userData$reactiveValues$parametersData
    # downstream: parameters selectize input choices and moduleState$available_parameters
    observe({
      tryCatch(
        {
          if (!is.null(session$userData$reactiveValues$parametersData)) {
            update_parameters_selectize(
              session,
              session$userData$reactiveValues$parametersData
            )
            moduleState$available_parameters <- session$userData$reactiveValues$parametersData
          } else {
            update_parameters_selectize(session, dummy_parameters)
            moduleState$available_parameters <- dummy_parameters
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error updating parameters: ",
              e$message,
              " (Code: mod_samples_update_parameters_selectize)"
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
              " (Code: mod_samples_update_parameters_selectize)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_update_parameters_selectize",
        session$userData$reactiveValues$parametersData,
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      )

    ## observe: Update compartments selectize choices ----
    # upstream: session$userData$reactiveValues$compartmentsData
    # downstream: compartments selectize input choices and moduleState$available_compartments
    observe({
      tryCatch(
        {
          if (!is.null(session$userData$reactiveValues$compartmentsData)) {
            update_compartments_selectize(
              session,
              session$userData$reactiveValues$compartmentsData
            )
            moduleState$available_compartments <- session$userData$reactiveValues$compartmentsData
          } else {
            update_compartments_selectize(session, dummy_compartments)
            moduleState$available_compartments <- dummy_compartments
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error updating compartments: ",
              e$message,
              " (Code: mod_samples_update_compartments_selectize)"
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
              " (Code: mod_samples_update_compartments_selectize)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_update_compartments_selectize",
        session$userData$reactiveValues$compartmentsData,
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      )

    ## observe: Load from LLM data when available ----
    # upstream: session$userData$reactiveValues$samplesDataLLM
    # downstream: input$sampling_date
    observe({
      tryCatch(
        {
          # just dates right now
          llm_samples <- session$userData$reactiveValues$samplesDataLLM
          if (
            !is.null(llm_samples) &&
              length(llm_samples) > 0 &&
              session$userData$reactiveValues$llmExtractionComplete
          ) {
            updateAirDateInput(
              session,
              inputId = "sampling_date",
              value = llm_samples
            )
          } else {}
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error loading LLM data: ",
              e$message,
              " (Code: mod_samples_populate_llm)"
            ),
            type = "error",
            duration = NULL
          )
        },
        warning = function(w) {
          showNotification(
            paste0("Warning: ", w$message, " (Code: mod_samples_populate_llm)"),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_populate_llm",
        # session$userData$reactiveValues$llmExtractionComplete,
        session$userData$reactiveValues$llmExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    ## observe ~bindEvent: Enable generate button when options valid ----
    observe({
      tryCatch(
        {
          if (
            all(
              length(input$sites_select) > 0,
              length(input$compartments_select) > 0,
              length(input$parameters_select) > 0,
              length(input$sampling_date) > 0,
              length(input$subsample) > 0
            )
          ) {
            enable("generate_combinations")
          } else {
            disable("generate_combinations")
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error toggling generate button: ",
              e$message,
              " (Code: mod_samples_toggle_generate_button)"
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
              " (Code: mod_samples_toggle_generate_button)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_toggle_generate_button",
        input$sites_select,
        input$compartments_select,
        input$parameters_select,
        input$sampling_date,
        input$subsample
      )

    ## observe ~bindEvent(generate_combinations): Generate sample combinations ----
    # upstream: user clicks input$generate_combinations
    # downstream: session$userData$reactiveValues$samplesData
    observe({
      tryCatch(
        {
          sites <- input$sites_select
          parameters <- input$parameters_select
          compartments <- input$compartments_select
          dates <- input$sampling_date
          subsamples <- input$subsample |> as.character()

          if (
            length(sites) == 0 ||
              length(parameters) == 0 ||
              length(compartments) == 0 ||
              length(dates) == 0
          ) {
            showNotification(
              "Please select at least one site, parameter, compartment, and date",
              type = "warning"
            )
            return()
          }

          # Create combinations with duplicate checking
          result <- create_sample_combinations(
            sites,
            parameters,
            compartments,
            dates,
            subsamples,
            session$userData$reactiveValues$samplesData, # Pass userData for duplicate checking
            moduleState$available_compartments,
            moduleState$available_sites,
            moduleState$available_parameters
          )
          new_combinations <- result$combinations
          skipped_count <- result$skipped

          if (nrow(new_combinations) > 0) {
            session$userData$reactiveValues$samplesData <- rbind(
              session$userData$reactiveValues$samplesData,
              new_combinations
            )
          }

          # Show notification with details
          message <- if (skipped_count > 0) {
            glue(
              "Generated {nrow(new_combinations)} new sample combinations, {skipped_count} duplicates skipped"
            )
          } else {
            glue("Generated {nrow(new_combinations)} sample combinations")
          }
          showNotification(message, type = "message")
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error generating combinations: ",
              e$message,
              " (Code: mod_samples_generate_combinations)"
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
              " (Code: mod_samples_generate_combinations)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_generate_combinations",
        input$generate_combinations
      )

    ## observe: Handle table changes ----
    # upstream: input$samples_table changes
    # downstream: session$userData$reactiveValues$samplesData
    observe({
      tryCatch(
        {
          if (!is.null(input$samples_table)) {
            # TODO: #42 Error handling table changes: no method or default for coercing “character” to “NA” (Code: mod_samples_handle_table_changes)
            updated_data <- hot_to_r(input$samples_table)
            session$userData$reactiveValues$samplesData <- updated_data
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error handling table changes: ",
              e$message,
              " (Code: mod_samples_handle_table_changes)"
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
              " (Code: mod_samples_handle_table_changes)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_handle_table_changes",
        input$samples_table
      )

    ## observe: Check overall validation, send data to session$userData ----
    # upstream: session$userData$reactiveValues$samplesData, iv
    # downstream: session$userData$reactiveValues$samplesDataValid
    observe({
      tryCatch(
        {
          validation_result <- iv$is_valid()

          # Update validation status in userData
          if (
            validation_result &&
              nrow(session$userData$reactiveValues$samplesData) > 0
          ) {
            session$userData$reactiveValues$samplesDataValid <- TRUE
          } else {
            session$userData$reactiveValues$samplesDataValid <- FALSE
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error validating data: ",
              e$message,
              " (Code: mod_samples_validate_data)"
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
              " (Code: mod_samples_validate_data)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_validate_data",
        input$samples_table,
        session$userData$reactiveValues$samplesData,
        session$userData$reactiveValues$llmExtractionComplete
      )

    ## observer: receive data from session$userData$reactiveValues$samplesData (import) ----
    ## and update module data
    # ensure SUBSAMPLE is character
    observe({
      tryCatch(
        {
          # Ensure SUBSAMPLE is character type (numbered subsamples get converted to numeric otherwise)
          if (nrow(session$userData$reactiveValues$samplesData) > 0) {
            session$userData$reactiveValues$samplesData <- session$userData$reactiveValues$samplesData |>
              mutate(SUBSAMPLE = as.character(SUBSAMPLE))
          }
          print_dev("Loaded saved data into samples userData.")
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error loading saved data: ",
              e$message,
              " (Code: mod_samples_load_saved_data)"
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
              " (Code: mod_samples_load_saved_data)"
            ),
            type = "warning",
            duration = 10
          )
        }
      )
    }) |>
      bindEvent(
        label = "mod_samples_load_saved_data",
        session$userData$reactiveValues$saveExtractionComplete,
        session$userData$reactiveValues$saveExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    # 3. Outputs ----

    ## output: Update combination preview ----
    # upstream: input selections
    # downstream: combination_preview output
    output$combination_preview <- renderUI({
      tryCatch(
        {
          sites_count <- length(input$sites_select %||% character(0))
          params_count <- length(input$parameters_select %||% character(0))
          comps_count <- length(input$compartments_select %||% character(0))
          dates_count <- length(input$sampling_date %||% character(0))
          replicates_count <- length(parse_subsamples(input$subsample)) %||% 1

          update_combination_preview(
            sites_count,
            params_count,
            comps_count,
            dates_count,
            replicates_count
          )
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error updating combination preview: ",
              e$message,
              " (Code: mod_samples_combination_preview)"
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
              " (Code: mod_samples_combination_preview)"
            ),
            type = "warning",
            duration = 10
          )
          return(NULL)
        }
      )
    })

    ## output: samples_table ----
    # upstream: session$userData$reactiveValues$samplesData
    # downstream: UI table display
    output$samples_table <- renderRHandsontable({
      tryCatch(
        {
          if (nrow(session$userData$reactiveValues$samplesData) == 0) {
            # Show empty table structure
            rhandsontable(
              initialise_samples_tibble(),
              stretchH = "all",
              height = "inherit",
              selectCallback = TRUE,
              width = NULL
            ) |>
              hot_col("SAMPLE_ID", readOnly = TRUE) |>
              hot_col("SUBSAMPLE_ID", readOnly = TRUE) |>
              hot_context_menu(
                allowRowEdit = TRUE,
                allowColEdit = FALSE,
                customOpts = list(
                  "row_above" = NULL,
                  "row_below" = NULL,
                  "remove_row" = list(
                    name = "Remove selected rows"
                  )
                )
              )
          } else {
            rhandsontable(
              session$userData$reactiveValues$samplesData,
              stretchH = "all",
              height = "inherit",
              selectCallback = TRUE,
              width = NULL
            ) |>
              hot_col("SITE_CODE", readOnly = TRUE) |>
              hot_context_menu(
                allowRowEdit = TRUE,
                allowColEdit = FALSE,
                customOpts = list(
                  "row_above" = NULL,
                  "row_below" = NULL,
                  "remove_row" = list(
                    name = "Remove selected rows"
                  )
                )
              )
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error rendering samples table: ",
              e$message,
              " (Code: mod_samples_render_table)"
            ),
            type = "error",
            duration = NULL
          )
          return(NULL)
        },
        warning = function(w) {
          showNotification(
            paste0("Warning: ", w$message, " (Code: mod_samples_render_table)"),
            type = "warning",
            duration = 10
          )
          return(NULL)
        }
      )
    })

    ## output: validation_reporter ----
    # upstream: session$userData$reactiveValues$samplesDataValid, mod_llm output
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      tryCatch(
        {
          llm_indicator <- if (
            session$userData$reactiveValues$llmExtractionComplete
          ) {
            div(
              bs_icon("cpu"),
              "Some data populated from LLM extraction - please review for accuracy",
              class = "validation-status validation-llm",
              style = "margin-bottom: 10px;"
            )
          } else {
            NULL
          }

          validation_status <- if (
            session$userData$reactiveValues$samplesDataValid
          ) {
            div(
              bs_icon("clipboard2-check"),
              glue(
                "All sample data validated successfully. {nrow(session$userData$reactiveValues$samplesData)} sample(s) ready."
              ),
              class = "validation-status validation-complete"
            )
          } else {
            div(
              bs_icon("exclamation-triangle"),
              "Generate at least one valid sample combination to proceed.",
              class = "validation-status validation-warning"
            )
          }

          div(llm_indicator, validation_status, class = "validation-container")
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error rendering validation status: ",
              e$message,
              " (Code: mod_samples_validation_reporter)"
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
              " (Code: mod_samples_validation_reporter)"
            ),
            type = "warning",
            duration = 10
          )
          return(NULL)
        }
      )
    })

    ## output: validated_data_display ----
    # upstream: session$userData$reactiveValues$samplesData (when valid)
    # downstream: UI data display
    output$validated_data_display <- renderText({
      tryCatch(
        {
          if (
            session$userData$reactiveValues$samplesDataValid &&
              nrow(session$userData$reactiveValues$samplesData) > 0
          ) {
            # Format first few samples as examples
            sample_count <- nrow(session$userData$reactiveValues$samplesData)
            display_count <- min(5, sample_count)

            sample_entries <- lapply(1:display_count, function(i) {
              sample <- session$userData$reactiveValues$samplesData[i, ]
              sample_lines <- sapply(names(sample), function(name) {
                value <- sample[[name]]
                if (is.na(value) || is.null(value) || value == "") {
                  paste0("  ", name, " = NA")
                } else if (is.character(value)) {
                  paste0("  ", name, " = '", value, "'")
                } else {
                  paste0("  ", name, " = ", as.character(value))
                }
              })
              paste0("Sample ", i, ":\n", paste(sample_lines, collapse = "\n"))
            })

            result <- paste(sample_entries, collapse = "\n\n")
            if (sample_count > display_count) {
              result <- paste0(
                result,
                "\n\n# ... and ",
                sample_count - display_count,
                " more samples"
              )
            }

            return(result)
          } else {
            "# Sample combinations will appear here when generated"
          }
        },
        error = function(e) {
          showNotification(
            paste0(
              "Error displaying validated data: ",
              e$message,
              " (Code: mod_samples_validated_data_display)"
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
              " (Code: mod_samples_validated_data_display)"
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
# mod_samples_ui("samples_1")

## To be copied in the server ----
# samples_data <- mod_samples_server("samples_1")
