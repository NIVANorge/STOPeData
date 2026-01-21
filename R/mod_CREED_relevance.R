# CREED Relevance Assessment Module ----
# A Shiny module for CREED relevance criteria evaluation

#' CREED_relevance UI Function
#'
#' @description A shiny Module for evaluating CREED relevance criteria.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput textAreaInput actionButton checkboxInput renderText selectInput h4 p strong em div
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel input_task_button
#' @importFrom bsicons bs_icon
mod_CREED_relevance_ui <- function(id) {
  ns <- NS(id)

  tagList(
    input_task_button(
      id = ns("populate_from_data"),
      label = "Populate section from data",
      icon = bs_icon("arrow-down-circle")
    ),
    ## Media criteria ----

    # RV1: Was the sampling medium/matrix appropriate ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-required"),
              "RV1: Was the sampling medium/matrix appropriate? (Required)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Was the sampling medium/matrix appropriate for the given purpose?"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV1_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV1_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV1"),
        create_limitations_input(ns, "RV1")
      )
    ),

    ### RV2: Collection Method/Sample Type ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-recommended"),
              "RV2: Collection Method/Sample Type (Recommended)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Was the sample collection method adequate for the given purpose?"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV2_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV2_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV2"),
        create_limitations_input(ns, "RV2")
      )
    ),

    ## Spatial criteria ----

    ### RV3: Study Area ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-required"),
              "RV3: Study Area (Required)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Were the study area and number of locations sampled suitable for the given purpose?"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV3_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV3_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV3"),
        create_limitations_input(ns, "RV3")
      )
    ),

    ### RV4: Site Type ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-recommended"),
              "RV4: Site Type (Recommended)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Was the rationale for selection of sampling locations provided and was it suitable for the given purpose?"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV4_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV4_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV4"),
        create_limitations_input(ns, "RV4")
      )
    ),

    ## Temporal criteria ----

    ### RV5: Sampling Timespan ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-required"),
              "RV5: Sampling Timespan (Required)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Were the samples collected over a time scale that was appropriate for the given purpose?"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV5_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV5_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV5"),
        create_limitations_input(ns, "RV5")
      )
    ),

    ### RV6: Sampling Frequency ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-required"),
              "RV6: Sampling Frequency (Required)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Over the timespan, was the sampling frequency appropriate for the given purpose?"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV6_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV6_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV6"),
        create_limitations_input(ns, "RV6")
      )
    ),

    ### RV7: Temporal Conditions ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-recommended"),
              "RV7: Temporal Conditions (Recommended)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Were conditions during sampling events documented and relevant for the given purpose?"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV7_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV7_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV7"),
        create_limitations_input(ns, "RV7")
      )
    ),

    ## Analytical criteria ----

    ### RV8: Analyte ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-required"),
              "RV8: Analyte (Required)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Was/were the reported analyte(s) appropriate for the given purpose?"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV8_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV8_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV8"),
        create_limitations_input(ns, "RV8")
      )
    ),

    ### RV9: Sensitivity/LOD/LOQ ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-required"),
              "RV9: Sensitivity/LOD/LOQ (Required)"
            ))
          ),
          p(
            strong("Criterion: "),
            "The method was sensitive enough for the given purpose"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV9_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV9_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV9"),
        create_limitations_input(ns, "RV9")
      )
    ),

    ## Data Handling and Statistics criteria ----

    ### RV10: Summary Statistics Type ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-recommended"),
              "RV10: Summary Statistics Type (Recommended)"
            ))
          ),
          p(
            strong("Criterion: "),
            "The summary statistics provided were appropriate for the given purpose"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV10_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV10_thresholds")),

      # Two-column layout for data and justification ----
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV10"),
        create_limitations_input(ns, "RV10")
      )
    ),

    ## Supporting Parameters criteria ----

    ### RV11: Supporting Parameters ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

      # Header with title and dropdown ----
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 1; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-required"),
              "RV11: Supporting Parameters (Required)"
            ))
          ),
          p(
            strong("Criterion: "),
            "All supporting parameters that were needed to achieve the given purpose were provided"
          )
        ),
        div(
          style = "min-width: 150px;",
          selectInput(
            inputId = ns("RV11_score"),
            label = "Score:",
            choices = names(CREED_choices_vocabulary()),
            width = "150px"
          )
        )
      ),

      # Dynamic threshold boxes ----
      uiOutput(ns("RV11_thresholds")),
      layout_column_wrap(
        width = "400px",
        create_relevant_data_input(ns, "RV11"),
        create_limitations_input(ns, "RV11")
      )
    )
  )
  # input_task_button(
  #   id = ns("calc_scores"),
  #   label = "Calculate Reliability Score"
  # )
}

#' CREED_relevance Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer
mod_CREED_relevance_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Server functionality to be implemented ----
    # - Auto-population of relevant data fields
    # - Score calculation and validation
    # - Data storage in session$userData
    # - Completion status tracking

    # Render threshold boxes for all RV criteria ----
    criterion_ids <- c(
      "RV1",
      "RV2",
      "RV3",
      "RV4",
      "RV5",
      "RV6",
      "RV7",
      "RV8",
      "RV9",
      "RV10",
      "RV11"
    )

    lapply(criterion_ids, function(criterion_id) {
      output_name <- paste0(criterion_id, "_thresholds")

      output[[output_name]] <- renderUI({
        if (is.null(session$userData$reactiveValues$creedPurpose)) {
          div("Awaiting purpose data.", class = "text-muted")
        } else {
          create_threshold_boxes(
            criterion_id = criterion_id,
            creed_purpose = session$userData$reactiveValues$creedPurpose
          )
        }
      })
    })
    # -----------------

    ## observe: Auto-populate relevant data fields ----
    # upstream: input$populate_from_data OR session$userData$reactiveValues$creedGetData
    # downstream: All RV*_relevant_data inputs
    observe({
      req(session$userData$reactiveValues)

      tryCatch(
        {
          # Get auto-populated field values
          field_updates <- autopop_relevance_fields(
            session$userData$reactiveValues
          )

          # Update each field
          for (field_name in names(field_updates)) {
            updateTextAreaInput(
              session,
              field_name,
              value = field_updates[[field_name]]
            )
          }

          showNotification(
            "Relevance data fields populated from dataset",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Auto-populate failed:", e$message),
            type = "error"
          )
        }
      )
    }) |>
      bindEvent(
        input$populate_from_data,
        session$userData$reactiveValues$creedGetData,
        ignoreInit = TRUE
      )

    ## observe: Collect relevance data ----
    # upstream: input$calc_scores, session$userData$reactiveValues$creedCalculateScores
    # downstream: session$userData$reactiveValues$creedRelevance
    observe({
      # Collect and store data ----
      session$userData$reactiveValues$creedRelevance <- collect_CREED_data(
        # defined in mod_CREED_fct_helpers.R
        criteria_config = CREED_relevance_criteria_config(),
        input = input
      )
    }) |>
      bindEvent(
        input$calc_scores,
        session$userData$reactiveValues$creedCalculateScores,
        ignoreInit = TRUE
      )
  })
}
