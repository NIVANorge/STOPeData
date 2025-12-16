# CREED Gateway Module ----
# A Shiny module for CREED gateway criteria assessment with auto-population from study data

#' CREED Gateway UI Function ----
#'
#' @description A shiny Module for assessing CREED gateway criteria with automatic population from study metadata.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList checkboxInput icon div strong textOutput updateCheckboxInput moduleServer observe renderText
#' @importFrom bslib input_task_button
#' @importFrom bsicons bs_icon
#' @import eDataDRF
#' @export
mod_CREED_gateway_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Auto-populate button ----
    input_task_button(
      id = ns("populate_from_data"),
      label = "Populate section from data",
      icon = bs_icon("arrow-down-circle")
    ),

    # Gateway Criterion 1: Sampling Medium/Matrix ----
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "margin-right: 15px;",
        strong("1. Does the study specify which medium/matrix is sampled?"),
        bs_icon("arrow-down-circle-fill", class = "text-primary"),

        div(
          style = "background-color: var(--bg-lighter); padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_medium_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_medium_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 2: Analyte ----
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "margin-right: 15px;",
        strong("2. Does the study specify which unique analyte is measured?"),
        bs_icon("arrow-down-circle-fill", class = "text-primary"),

        div(
          style = "background-color: var(--bg-lighter); padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_analyte_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_analyte_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 3: Spatial Location ----
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "margin-right: 15px;",
        strong(
          "3. Does the study specify where samples were collected?"
        ),
        bs_icon("arrow-down-circle-fill", class = "text-primary"),

        div(
          "At a minimum, there is enough information for the given purpose (e.g., country)."
        ),
        div(
          style = "background-color: var(--bg-lighter); padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_location_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_location_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 4: Year ----
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "margin-right: 15px;",
        strong(
          "4. Does the study indicate when samples were collected?"
        ),
        bs_icon("arrow-down-circle-fill", class = "text-primary"),

        div(
          "At a minimum, there is enough information for the given purpose (e.g., sampling year)."
        ),
        div(
          style = "background-color: var(--bg-lighter); padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_year_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_year_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 5: Units ----
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "margin-right: 15px;",
        strong("5. Does the study specify units of measurement?"),
        bs_icon("arrow-down-circle-fill", class = "text-primary"),

        div(
          style = "background-color: var(--bg-lighter); padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_units_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_units_answer"),
        label = "Yes",
        value = FALSE
      )
    ),

    # Gateway Criterion 6: Data Source/Citation ----
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin: 15px 0; padding: 10px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "margin-right: 15px;",
        strong(
          "6. Does the study cite the source of data and/or is a suitable bibliographic reference available for the study?"
        ),
        bs_icon("arrow-down-circle-fill", class = "text-primary"),

        div(
          style = "background-color: var(--bg-lighter); padding: 6px; margin-top: 6px; border-radius: 4px; font-size: 0.9em;",
          strong("Relevant data: "),
          textOutput(ns("gateway_citation_summary"), inline = TRUE)
        )
      ),
      checkboxInput(
        inputId = ns("gateway_citation_answer"),
        label = "Yes",
        value = FALSE
      )
    )
  )
}

#' CREED Gateway Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer observe updateCheckboxInput renderText bindEvent
#' @importFrom tibble as_tibble
#' @export
mod_CREED_gateway_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Event handlers ----

    ## observe: Populate gateway criteria from existing data ----
    # upstream: input$populate_from_data (button click)
    # downstream: session$userData$reactiveValues$creedData$gateway_criteria, checkbox inputs
    observe({
      auto_populate_gateway_criteria()
    }) |>
      bindEvent(
        input$populate_from_data,
        # TODO: For some reason check_gateway_availability() and summary() don't work properly at this point - the table hasn't be initialised
        session$userData$reactiveValues$creedGetData,
        ignoreInit = TRUE
      )

    # 2. Helper functions ----

    ## Auto-populate gateway criteria ----
    auto_populate_gateway_criteria <- function() {
      # check to see if either samplesData or samplesDataWithBiota actualy exist
      samplesData <- session$userData$reactiveValues$samplesData
      samplesDataWithBiota <- session$userData$reactiveValues$samplesDataWithBiota

      samples_data <- if (
        exists("samplesDataWithBiota") &
          isTruthy(samplesDataWithBiota) &
          nrow(samplesDataWithBiota) > 1
      ) {
        samplesDataWithBiota
      } else if (
        exists("samplesData") & isTruthy(samplesData) & nrow(samplesData)
      ) {
        samplesData
      } else {
        tibble(NULL)
        print_dev(
          "auto_populate_gateway_criteria(): samplesDataWithBiota & samplesData empty, returning tibble(NULL)"
        )
        x
      }

      # Build module_data list from session userData
      module_data <- list(
        campaign = session$userData$reactiveValues$campaignData,
        references = session$userData$reactiveValues$referenceData,
        sites = session$userData$reactiveValues$sitesData,
        parameters = session$userData$reactiveValues$parametersData,
        compartments = session$userData$reactiveValues$compartmentsData,
        samples = samples_data,
        methods = session$userData$reactiveValues$methodsData,
        measurements = session$userData$reactiveValues$measurementsData
      )

      # Get gateway availability (TRUE/FALSE)
      availability <- check_gateway_availability(module_data)

      # Get gateway summaries (text data)
      summaries <- get_gateway_summaries(module_data)

      # Update checkboxes
      updateCheckboxInput(
        session,
        "gateway_medium_answer",
        value = availability$medium
      )
      updateCheckboxInput(
        session,
        "gateway_analyte_answer",
        value = availability$analyte
      )
      updateCheckboxInput(
        session,
        "gateway_location_answer",
        value = availability$location
      )
      updateCheckboxInput(
        session,
        "gateway_year_answer",
        value = availability$year
      )
      updateCheckboxInput(
        session,
        "gateway_units_answer",
        value = availability$units
      )
      updateCheckboxInput(
        session,
        "gateway_citation_answer",
        value = availability$citation
      )

      # Store gateway data in session userData (it expects a tibble format)
      session$userData$reactiveValues$creedData$gateway_criteria <- summaries |>
        as_tibble()
    }

    # 3. Outputs ----

    ## output: gateway_medium_summary ----
    # upstream: session$userData$reactiveValues$creedData$gateway_criteria
    # downstream: UI summary display
    output$gateway_medium_summary <- renderText({
      session$userData$reactiveValues$creedData$gateway_criteria$medium %||%
        "Relevant data not found"
    })

    ## output: gateway_analyte_summary ----
    # upstream: session$userData$reactiveValues$creedData$gateway_criteria
    # downstream: UI summary display
    output$gateway_analyte_summary <- renderText({
      session$userData$reactiveValues$creedData$gateway_criteria$analyte %||%
        "Relevant data not found"
    })

    ## output: gateway_location_summary ----
    # upstream: session$userData$reactiveValues$creedData$gateway_criteria
    # downstream: UI summary display
    output$gateway_location_summary <- renderText({
      session$userData$reactiveValues$creedData$gateway_criteria$location %||%
        "Relevant data not found"
    })

    ## output: gateway_year_summary ----
    # upstream: session$userData$reactiveValues$creedData$gateway_criteria
    # downstream: UI summary display
    output$gateway_year_summary <- renderText({
      session$userData$reactiveValues$creedData$gateway_criteria$year %||%
        "Relevant data not found"
    })

    ## output: gateway_units_summary ----
    # upstream: session$userData$reactiveValues$creedData$gateway_criteria
    # downstream: UI summary display
    output$gateway_units_summary <- renderText({
      session$userData$reactiveValues$creedData$gateway_criteria$units %||%
        "Relevant data not found"
    })

    ## output: gateway_citation_summary ----
    # upstream: session$userData$reactiveValues$creedData$gateway_criteria
    # downstream: UI summary display
    output$gateway_citation_summary <- renderText({
      session$userData$reactiveValues$creedData$gateway_criteria$citation %||%
        "Relevant data not found"
    })
  })
}

## To be copied in the UI ----
# mod_CREED_gateway_ui("CREED_gateway_1")

## To be copied in the server ----
# mod_CREED_gateway_server("CREED_gateway_1")
