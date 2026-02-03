# CREED Quality Assessment Module ----
# A Shiny module for CREED-based dataset quality assessment

#' CREED UI Function ----
#' @noRd
#'
#' @importFrom shiny NS tagList textInput textAreaInput actionButton checkboxInput renderText markdown HTML
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs disabled disable enable
mod_CREED_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main CREED card ----
    card(
      fill = TRUE,
      full_screen = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(
          title = "CREED",
          content_file = "inst/app/www/md/intro_CREED.md",
          div(
            "Required criteria (",
            tooltip(
              bs_icon("award-fill", class = "CREED-required"),
              "Required for Silver level scoring"
            ),
            ") are needed for Silver level scoring. Recommended criteria (",
            tooltip(
              bs_icon("award-fill", class = "CREED-recommended"),
              "Additional requirements for Gold level scoring"
            ),
            ") are additional requirements for Gold level scoring."
          )
        ),

        div(
          div(
            style = "margin: 10px 10px 0 10px; display: flex; align-items: center; gap: 15px;",
            input_task_button(
              id = ns("get_data"),
              label = list(
                "Get Data from Modules",
                bs_icon("arrow-down-circle-fill")
              ),
              class = "btn-primary"
            ),
            uiOutput(ns("validation_reporter")),
          ),
          br(),
          div(
            class = "alert alert-primary",
            p(
              bs_icon("arrow-down-circle-fill", class = "text-primary"),
              strong(" Auto-population: "),
              "Fields marked with this icon are auto-populated from data entered in 
          earlier modules. This data can be overwritten as needed, but note that
          if you populate fields from data again, your changes will not be 
          saved."
            )
          )
        ),

        ## Purpose Statement ---
        h5("1. Purpose Statement"),
        markdown(
          "- The CREED grading process begins with laying out the purpose of 
            your overall chemical/ecological impact or risk assessment, which 
            shapes the criteria used. 
            - As it is usual to include multiple datasets
            in such an assessment, we recommend reusing a single Purpose Statement
            per assessment.
            "
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Purpose Statement",
            mod_CREED_purpose_ui(NS(id, "CREED_purpose"))
          )
        ),

        ## Dataset Details  ----
        h5("2. Dataset Details - Key Attributes"),
        markdown(
          "- This section provides a summary of basic details for the dataset. 
          - Review the auto-populated fields below and add any missing information."
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Dataset Details",
            mod_CREED_details_ui(NS(id, "CREED_details"))
          )
        ),

        ## Gateway Criteria ----
        h5("3. Gateway Criteria"),
        markdown(
          "- CREED's gateway criteria are designed to allow for the easy 
          rejection of a study without requiring methodical examination.
          - Most studies processed using this tool can be expected 
          to pass these criteria without issue. 
          
          - Nevertheless they are included for the sake of completeness.
          - Each criterion is auto-evaluated based on your entered data, but can be 
          manually overridden."
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Gateway Criteria",
            mod_CREED_gateway_ui(NS(id, "CREED_gateway"))
          )
        ),

        ## Reliability Criteria ---
        h5("4. Reliability Criteria"),
        markdown(
          "Assess how reliable the dataset is for answering your assessment 
          questions."
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Reliability Criteria",
            mod_CREED_reliability_ui(NS(id, "CREED_reliability"))
          )
        ),

        ## Relevance Criteria ---

        h5("5. Relevance Criteria"),
        markdown(
          "Assess how relevant the dataset is to the purpose as described in your
          Purpose Statement."
        ),

        accordion(
          open = FALSE,
          accordion_panel(
            title = "Relevance Criteria",
            mod_CREED_relevance_ui(NS(id, "CREED_relevance"))
          )
        ),

        ## Status display ----
        div(
          style = "margin-top: 15px;",
          uiOutput(ns("status_reporter"))
        ),
        input_task_button(
          id = ns("calculate_scores"),
          label = list(
            "Calculate CREED Scores",
            bs_icon("award-fill")
          ),
          class = "btn-success"
        ),

        uiOutput(ns("creed_scores_display")),
      )
    )
  )
}

#' CREED Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI showNotification updateTextAreaInput updateCheckboxInput bindEvent
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @importFrom shinyjs enable disable
#' @importFrom dplyr add_row
mod_CREED_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      ready_for_assessment = FALSE,
      assessment_saved = FALSE,
      creed_scores_pretty = tibble(NULL)
    )

    # Call sub-module servers ----
    mod_CREED_purpose_server("CREED_purpose")
    mod_CREED_details_server("CREED_details")
    mod_CREED_gateway_server("CREED_gateway")
    mod_CREED_reliability_server("CREED_reliability")
    mod_CREED_relevance_server("CREED_relevance")

    # 2. Helper functions ----

    # 3. Observers and Reactives ----

    ## observe enable CREED assessment once measurements are entered
    # upstream: session$userData$reactiveValues$measurementsDataValid
    # downstream: alll CREED stuff
    observe(
      {
        tryCatch(
          {
            if (isTRUE(session$userData$reactiveValues$measurementsDataValid)) {
              enable("get_data")
            } else {
              disable("get_data")
            } # we could technically do this with toggle() but I trust this implementation slightly more?
          },
          error = function(e) {
            showNotification(
              paste("CREED validation of previous modules failed:", e$message),
              type = "error"
            )
          }
        )
      }
    ) |>
      bindEvent(
        session$userData$reactiveValues$measurementsDataValid == TRUE,
        ignoreInit = FALSE
      )

    ## observe ~bindEvent(get_data): Get data from session ----
    # upstream: user clicks input$get_data
    # downstream: we get the by triggering a bunch of observers downstream
    observe(
      {
        tryCatch(
          {
            session$userData$reactiveValues$creedGetData <- session$userData$reactiveValues$creedGetData +
              1
          },
          error = function(e) {
            showNotification(
              paste("CREED Autopopulation failed:", e$message),
              type = "error"
            )
          }
        )
        # this triggers all the autopopulation observers in the individual CREED modules
      }
    ) |>
      bindEvent(input$get_data)

    ## observe ~bindEvent(input$calculate_scores): Get data from session ----
    # upstream: user clicks input$get_data
    # downstream: we get the by triggering a bunch of observers downstream
    observe(
      {
        tryCatch(
          {
            session$userData$reactiveValues$creedCalculateScores <- session$userData$reactiveValues$creedCalculateScores +
              1
          },
          error = function(e) {
            showNotification(
              paste("CREED Score Calculation failed:", e$message),
              type = "error"
            )
          }
        )
        # this triggers score calculation
      }
    ) |>
      bindEvent(input$calculate_scores)

    ## observe: Calculate CREED silver and gold scores ----
    # upstream: session$userData$reactiveValues$creedRelevance, creedReliability
    # downstream: session$userData$reactiveValues$creedScores
    observe({
      tryCatch(
        {
          if (
            nrow(session$userData$reactiveValues$creedReliability) > 1 &&
              nrow(session$userData$reactiveValues$creedRelevance) > 1
          ) {
            reliability_data <- session$userData$reactiveValues$creedReliability
            relevance_data <- session$userData$reactiveValues$creedRelevance

            # Add numeric score column
            reliability_data <- reliability_data |>
              mutate(numeric_score = as.integer(score))

            relevance_data <- relevance_data |>
              mutate(numeric_score = as.integer(score))

            # Calculate Silver levels (Required criteria)
            reliability_silver <- reliability_data |>
              filter(required_recommended == "Required") |>
              pull(numeric_score) |>
              max(na.rm = TRUE)

            relevance_silver <- relevance_data |>
              filter(required_recommended == "Required") |>
              pull(numeric_score) |>
              max(na.rm = TRUE)

            # Calculate Gold levels (Recommended criteria)
            reliability_gold <- reliability_data |>
              filter(required_recommended == "Recommended") |>
              pull(numeric_score) |>
              max(na.rm = TRUE)

            relevance_gold <- relevance_data |>
              filter(required_recommended == "Recommended") |>
              pull(numeric_score) |>
              max(na.rm = TRUE)

            # Map scores back to categories
            reliability_categories <- c(
              "1" = "Reliable without restrictions",
              "2" = "Reliable with restrictions",
              "3" = "Not assignable",
              "4" = "Not usable"
            )

            relevance_categories <- c(
              "1" = "Relevant without restrictions",
              "2" = "Relevant with restrictions",
              "3" = "Not assignable",
              "4" = "Not usable"
            )

            # Create results tibble
            moduleState$creed_scores_pretty <- tibble(
              level = c("Silver", "Gold"),
              reliability_score = c(reliability_silver, reliability_gold),
              reliability_category = reliability_categories[as.character(c(
                reliability_silver,
                reliability_gold
              ))],
              relevance_score = c(relevance_silver, relevance_gold),
              relevance_category = relevance_categories[as.character(c(
                relevance_silver,
                relevance_gold
              ))]
            )

            # Store results
            ref_id <- if (
              nrow(session$userData$reactiveValues$referenceData) > 0
            ) {
              session$userData$reactiveValues$referenceData$REFERENCE_ID
            } else {
              "Reference ID Not Found"
            }

            session$userData$reactiveValues$creedScores <- initialise_CREED_scores_tibble() |>
              add_row(
                REFERENCE_ID = ref_id,
                SILVER_RELIABILITY = reliability_categories[as.character(c(
                  reliability_silver
                ))],
                SILVER_RELEVANCE = relevance_categories[as.character(c(
                  relevance_silver
                ))],
                GOLD_RELIABILITY = reliability_categories[as.character(c(
                  reliability_gold
                ))],
                GOLD_RELEVANCE = relevance_categories[as.character(c(
                  relevance_gold
                ))]
              )

            showNotification(
              "CREED scores calculated successfully",
              type = "message"
            )
          }
        },
        error = function(e) {
          showNotification(
            paste("CREED Score Calculation failed:", e$message),
            type = "error"
          )
        }
      )
    }) |>
      bindEvent(
        # only run once we have the data upstream
        (nrow(session$userData$reactiveValues$creedReliability) > 1 &&
          nrow(session$userData$reactiveValues$creedRelevance) > 1),
        ignoreInit = TRUE
      )

    # 4. Outputs ----

    ## output: validation_reporter ----
    # upstream: moduleState$is_valid, mod_llm output
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      validation_status <- if (
        session$userData$reactiveValues$measurementsDataValid
      ) {
        div(
          bs_icon("clipboard2-check"),
          "Data module validated successfully.",
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Please complete the data module before starting CREED assessment.",
          class = "validation-status validation-warning"
        )
      }

      div(validation_status, class = "validation-container")
    }) |>
      bindEvent(
        session$userData$reactiveValues$measurementsDataValid,
        ignoreInit = FALSE
      )

    ## output: status_reporter ----
    # upstream: moduleState$assessment_saved
    # downstream: UI status display
    # FIXME: We don't actually track assessment_saved anymore, so this reporter will never change.
    output$status_reporter <- renderUI({
      if (moduleState$assessment_saved) {
        div(
          bs_icon("check-circle"),
          "CREED assessment saved successfully. Dataset details are ready for quality evaluation.",
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("info-circle"),
          "Review dataset details above and click 'Calculate Score' when ready.",
          class = "validation-status validation-info"
        )
      }
    })

    ## output: Display CREED scores ----
    # upstream: session$userData$reactiveValues$creedScores
    # downstream: UI display
    output$creed_scores_display <- renderUI({
      scores <- if (nrow(moduleState$creed_scores_pretty) > 0) {
        moduleState$creed_scores_pretty
      } else {
        initialise_CREED_scores_tibble()
      }

      tagList(
        h5("CREED Assessment Results"),
        tags$table(
          class = "table table-sm",
          tags$thead(
            tags$tr(
              tags$th("Level"),
              tags$th("Reliability"),
              tags$th("Relevance")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(
                bs_icon("award-fill", class = "CREED-required"),
                strong("Silver")
              ),
              tags$td(scores$reliability_category[1]),
              tags$td(scores$relevance_category[1])
            ),
            tags$tr(
              tags$td(
                bs_icon("award-fill", class = "CREED-recommended"),
                strong("Gold")
              ),
              tags$td(scores$reliability_category[2]),
              tags$td(scores$relevance_category[2])
            )
          )
        )
      )
    })
  })
}

## To be copied in the UI ----
# mod_creed_ui("creed_1")

## To be copied in the server ----
# mod_creed_server("creed_1")
