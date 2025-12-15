# Compartments Import Module ----
# A Shiny module for environmental compartment combinations with form-then-table approach

#' Compartments UI Function ----
#'
#' @description A shiny Module for environmental compartment combinations data entry.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput actionButton
#' @importFrom bslib card  card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @export
mod_compartments_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs ----
    useShinyjs(),

    # Main content card ----
    card(
      full_screen = TRUE,
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_compartments.md"),

        ## Compartment selection form ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,
          style = "margin-bottom: -20px;",

          selectInput(
            inputId = ns("environ_compartment_select"),
            label = tooltip(
              list("Environmental Compartment", bs_icon("info-circle-fill")),
              "Which sphere does the sample come from?"
            ),
            choices = environ_compartments_vocabulary(),
            width = "100%",
            selected = "Aquatic"
          ),

          selectInput(
            inputId = ns("environ_compartment_sub_select"),
            label = tooltip(
              list(
                "Environmental Sub-Compartment",
                bs_icon("info-circle-fill")
              ),
              "Specific subset within the environmental compartment"
            ),
            choices = environ_compartments_sub_vocabulary()$Aquatic,
            width = "100%"
          ),

          selectInput(
            inputId = ns("measured_category_select"),
            label = tooltip(
              list("Measured Category", bs_icon("info-circle-fill")),
              "Type of exposure measurement"
            ),
            choices = measured_categories_vocabulary(),
            selected = "External",
            width = "100%"
          )
        ),

        ## Add combination button and validation status ----
        div(
          style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 15px 0;",

          input_task_button(
            id = ns("add_combination"),
            label = "Add Combination",
            icon = icon("plus-circle"),
            class = "btn-success",
            width = "200px"
          ),

          ### Validation status ----
          uiOutput(ns("validation_reporter"))
        ),

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

    ## Compartments table card ----
    card(
      full_screen = TRUE,
      div(
        rHandsontableOutput(ns("compartments_table")),
        style = "margin-bottom: 10px;"
      )
    )
  )
}

#' Compartments Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom tibble tibble
#' @importFrom shinyjs enable disable
#' @importFrom purrr is_empty
#' @importFrom glue glue
#' @importFrom dplyr add_row
#' @export
mod_compartments_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    # CHANGED: Keep only UI-specific transient state here
    moduleState <- reactiveValues(
      validation_message = ""
    )

    ## InputValidator for table-level validation ----
    iv <- InputValidator$new()
    # Rule 1: Check if there are no rows
    iv$add_rule("compartments_data", function(value) {
      # CHANGED: Reference userData instead of moduleState
      if (nrow(session$userData$reactiveValues$compartmentsData) == 0) {
        moduleState$validation_message <<- "At least one compartment combination must be added."
        return("At least one compartment combination must be added.")
      }
    })

    # Rule 2: Check for missing required fields in any row
    iv$add_rule("compartments_data", function(value) {
      # CHANGED: Reference userData instead of moduleState
      if (nrow(session$userData$reactiveValues$compartmentsData) > 0) {
        required_fields <- c(
          "ENVIRON_COMPARTMENT",
          "ENVIRON_COMPARTMENT_SUB",
          "MEASURED_CATEGORY"
        )

        for (i in 1:nrow(session$userData$reactiveValues$compartmentsData)) {
          for (field in required_fields) {
            field_value <- session$userData$reactiveValues$compartmentsData[
              i,
              field
            ]
            if (
              is.na(field_value) || field_value == "" || is_empty(field_value)
            ) {
              message <- glue("Row {i} is missing required field: {field}")
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 3: Check for invalid compartment/sub-compartment combinations
    iv$add_rule("compartments_data", function(value) {
      # CHANGED: Reference userData instead of moduleState
      if (nrow(session$userData$reactiveValues$compartmentsData) > 0) {
        for (i in 1:nrow(session$userData$reactiveValues$compartmentsData)) {
          compartment <- session$userData$reactiveValues$compartmentsData[[
            i,
            "ENVIRON_COMPARTMENT"
          ]]
          sub_compartment <- session$userData$reactiveValues$compartmentsData[[
            i,
            "ENVIRON_COMPARTMENT_SUB"
          ]]

          if (compartment %in% names(environ_compartments_sub_vocabulary())) {
            valid_subs <- environ_compartments_sub_vocabulary()[[compartment]]
            if (!sub_compartment %in% valid_subs) {
              message <- glue(
                "Row {i} has invalid sub-compartment {sub_compartment} for compartment {compartment}"
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })
    iv$enable()

    # 2. Observers and Reactives ----

    ## observe: Update sub-compartment dropdown when compartment changes ----
    # upstream: input$environ_compartment_select
    # downstream: input$environ_compartment_sub_select choices
    observe({
      compartment <- input$environ_compartment_select
      if (isTruthy(compartment) && compartment != "") {
        available_subs <- environ_compartments_sub_vocabulary()[[compartment]]
        if (!is.null(available_subs)) {
          choices <- c("Select sub-compartment..." = "", available_subs)
          updateSelectInput(
            session,
            "environ_compartment_sub_select",
            choices = choices
          )
        } else {
          updateSelectInput(
            session,
            "environ_compartment_sub_select",
            choices = c("No sub-compartments available" = "")
          )
        }
      }
    }) |>
      bindEvent(input$environ_compartment_select)

    ## observe: Add combination from form ----
    # upstream: user clicks input$add_combination
    # downstream: session$userData$reactiveValues$compartmentsData
    observe({
      compartment <- input$environ_compartment_select
      sub_compartment <- input$environ_compartment_sub_select
      category <- input$measured_category_select

      if (
        isTruthy(compartment) &&
          compartment != "" &&
          isTruthy(sub_compartment) &&
          sub_compartment != "" &&
          isTruthy(category) &&
          category != ""
      ) {
        # CHANGED: Removed duplicate check - duplicates are now allowed

        # Create new combination
        new_combination <- initialise_compartments_tibble() |>
          add_row(
            ENVIRON_COMPARTMENT = compartment,
            ENVIRON_COMPARTMENT_SUB = sub_compartment,
            MEASURED_CATEGORY = category
          )

        # CHANGED: Add to userData instead of moduleState
        session$userData$reactiveValues$compartmentsData <- rbind(
          session$userData$reactiveValues$compartmentsData,
          new_combination
        )

        showNotification(
          glue(
            "Added combination: {compartment} → {sub_compartment} → {category}"
          ),
          type = "message"
        )
      } else {
        showNotification(
          "Please select all required fields before adding",
          type = "warning"
        )
      }
    }) |>
      bindEvent(input$add_combination)

    ## observe: Handle table changes ----
    # upstream: input$compartments_table changes
    # downstream: session$userData$reactiveValues$compartmentsData
    observe({
      if (!is.null(input$compartments_table)) {
        updated_data <- hot_to_r(input$compartments_table)
        # CHANGED: Update userData instead of moduleState
        session$userData$reactiveValues$compartmentsData <- updated_data
      }
    }) |>
      bindEvent(input$compartments_table)

    ## observe: Check overall validation status ----
    # upstream: session$userData$reactiveValues$compartmentsData, iv
    # downstream: session$userData$reactiveValues$compartmentsDataValid
    observe({
      validation_result <- iv$is_valid()

      # CHANGED: Update validation status in userData
      if (
        validation_result &&
          nrow(session$userData$reactiveValues$compartmentsData) > 0
      ) {
        session$userData$reactiveValues$compartmentsDataValid <- TRUE
      } else {
        session$userData$reactiveValues$compartmentsDataValid <- FALSE
      }
    }) |>
      bindEvent(
        input$compartments_table,
        session$userData$reactiveValues$compartmentsData,
        session$userData$reactiveValues$llmExtractionComplete
      )

    ## observe: Load from LLM data when available ----
    # upstream: session$userData$reactiveValues$compartmentsDataLLM
    # downstream: session$userData$reactiveValues$compartmentsData
    observe({
      llm_compartments <- session$userData$reactiveValues$compartmentsDataLLM
      if (
        !is.null(llm_compartments) &&
          nrow(llm_compartments) > 0 &&
          session$userData$reactiveValues$llmExtractionComplete
      ) {
        # CHANGED: Replace userData with LLM data
        session$userData$reactiveValues$compartmentsData <- llm_compartments

        # showNotification(
        #   glue("Populated {nrow(llm_compartments)} compartments."),
        #   type = "message"
        # )
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$compartmentsDataLLM,
        session$userData$reactiveValues$llmExtractionComplete,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    ## observer: receive data from session$userData$reactiveValues$compartmentsData (import) ----
    ## and update module data
    # CHANGED: Data is already in userData, just log the event
    observe({
      print_dev("Loaded saved data into compartments userData.")
    }) |>
      bindEvent(
        session$userData$reactiveValues$saveExtractionComplete,
        session$userData$reactiveValues$saveExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    # 3. Outputs ----

    ## output: compartments_table ----
    # upstream: session$userData$reactiveValues$compartmentsData
    # downstream: UI table display
    output$compartments_table <- renderRHandsontable({
      # CHANGED: Reference userData instead of moduleState
      if (nrow(session$userData$reactiveValues$compartmentsData) == 0) {
        # Show empty table structure
        rhandsontable(
          initialise_compartments_tibble(),
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
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
          session$userData$reactiveValues$compartmentsData,
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
          hot_col(
            "ENVIRON_COMPARTMENT",
            readOnly = TRUE
          ) |>
          hot_col(
            "ENVIRON_COMPARTMENT_SUB",
            readOnly = TRUE
          ) |>
          hot_col(
            "MEASURED_CATEGORY",
            readOnly = TRUE
          ) |>
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
    })

    ## output: validation_reporter ----
    # upstream: session$userData$reactiveValues$compartmentsDataValid, mod_llm output
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
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

      # CHANGED: Reference userData validation status instead of moduleState
      validation_status <- if (
        session$userData$reactiveValues$compartmentsDataValid
      ) {
        div(
          bs_icon("clipboard2-check"),
          glue(
            "All compartment data validated successfully. {nrow(session$userData$reactiveValues$compartmentsData)} compartment combination(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          moduleState$validation_message,
          class = "validation-status validation-warning"
        )
      }

      div(llm_indicator, validation_status, class = "validation-container")
    })

    ## output: validated_data_display ----
    # upstream: session$userData$reactiveValues$compartmentsData (when valid)
    # downstream: UI data display
    output$validated_data_display <- renderText({
      # CHANGED: Show data only when valid, reference userData
      if (
        session$userData$reactiveValues$compartmentsDataValid &&
          nrow(session$userData$reactiveValues$compartmentsData) > 0
      ) {
        # Format each combination as a separate entry
        combo_entries <- lapply(
          1:nrow(session$userData$reactiveValues$compartmentsData),
          function(i) {
            combo <- session$userData$reactiveValues$compartmentsData[i, ]
            combo_lines <- sapply(names(combo), function(name) {
              value <- combo[[name]]
              if (is.na(value) || is.null(value) || value == "") {
                paste0("  ", name, " = NA")
              } else if (is.character(value)) {
                paste0("  ", name, " = '", value, "'")
              } else {
                paste0("  ", name, " = ", as.character(value))
              }
            })
            paste0(
              "Combination ",
              i,
              ":\n",
              paste(combo_lines, collapse = "\n")
            )
          }
        )

        paste(combo_entries, collapse = "\n\n")
      } else {
        "# Compartment combinations will appear here when valid combinations are added"
      }
    })
  })
}

## To be copied in the UI ----
# mod_compartments_ui("compartments_1")

## To be copied in the server ----
# compartments_data <- mod_compartments_server("compartments_1")
