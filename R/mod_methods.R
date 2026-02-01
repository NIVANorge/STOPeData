# Methods Import Module ----
# A Shiny module for analytical and Sampling Protocols with categorized protocol selection

#' Methods UI Function ----
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput actionButton
#' @importFrom bslib card  card_body layout_column_wrap accordion accordion_panel tooltip input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @import eDataDRF
mod_methods_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main content card ----
    card(
      full_screen = TRUE,
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_methods.md"),

        ## Method selection form ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,
          margin = "-10px",

          selectInput(
            inputId = ns("protocol_category_select"),
            label = tooltip(
              list("Protocol Category", bs_icon("info-circle-fill")),
              "Type of protocol or method being used"
            ),
            choices = c(
              "Sampling Protocol" = "Sampling Protocol",
              "Fractionation Protocol" = "Fractionation Protocol",
              "Extraction Protocol" = "Extraction Protocol",
              "Analytical Protocol" = "Analytical Protocol"
            ),
            selected = "Sampling Protocol",
            width = "100%"
          ),

          selectInput(
            inputId = ns("protocol_name_select"),
            label = tooltip(
              list("Protocol Name", bs_icon("info-circle-fill")),
              "Specific method or protocol within the category"
            ),
            choices = c("Select category first..." = ""),
            width = "100%",
            multiple = TRUE
          )
        ),

        ## Add method button and validation status ----
        div(
          style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",

          input_task_button(
            id = ns("add_method"),
            label = "Add Method",
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

    ## Methods table card ----
    card(
      full_screen = TRUE,
      div(
        rHandsontableOutput(ns("methods_table")),
        style = "margin: 20px; word-wrap: break-word;"
      )
    )
  )
}

#' Methods Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateSelectInput
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom shinyjs enable disable
#' @importFrom arrow read_parquet
#' @importFrom tibble tibble deframe
#' @importFrom dplyr filter select ungroup row_number relocate
#' @importFrom purrr is_empty
#' @import eDataDRF
mod_methods_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    # CHANGED: Keep only UI-specific transient state here
    moduleState <- reactiveValues()

    ## Protocol options by category ----
    protocol_options <- protocol_options_vocabulary()
    protocol_categories <- protocol_categories_vocabulary()
    protocol_names <- unique(protocol_options$Short_Name)

    ## InputValidator for table-level validation ----
    # ! FORMAT-BASED
    # CHANGED: Reference userData instead of moduleState
    # CHANGED: Separate rule for each validation check
    iv <- InputValidator$new()

    ## Rule: At least one method required ----
    iv$add_rule("methods_table_min_rows", function(value) {
      if (nrow(session$userData$reactiveValues$methodsData) == 0) {
        "At least one method must be added"
      }
    })

    ## Rule: Required fields must be present ----
    iv$add_rule("methods_table_required_fields", function(value) {
      if (nrow(session$userData$reactiveValues$methodsData) == 0) {
        return(NULL)
      }

      required_fields <- c("PROTOCOL_CATEGORY", "PROTOCOL_NAME")

      for (i in 1:nrow(session$userData$reactiveValues$methodsData)) {
        for (field in required_fields) {
          value <- session$userData$reactiveValues$methodsData[i, field]
          if (is_empty(value) || value == "") {
            return(paste("Row", i, "is missing required field:", field))
          }
        }
      }
      NULL
    })

    ## Rule: Protocol must be valid for category ----
    iv$add_rule("methods_table_protocol_category_match", function(value) {
      if (nrow(session$userData$reactiveValues$methodsData) == 0) {
        return(NULL)
      }

      for (i in 1:nrow(session$userData$reactiveValues$methodsData)) {
        category <- session$userData$reactiveValues$methodsData[
          i,
          "PROTOCOL_CATEGORY"
        ]
        protocol <- session$userData$reactiveValues$methodsData[
          i,
          "PROTOCOL_NAME"
        ]

        # Check if protocol is valid for the category
        if (category %in% names(protocol_options)) {
          valid_protocols <- protocol_options[[category]]
          if (!protocol %in% valid_protocols) {
            return(paste(
              "Row",
              i,
              "has invalid protocol",
              protocol,
              "for category",
              category
            ))
          }
        }
      }
      NULL
    })

    iv$enable()

    # 2. Helper functions ----

    ## function: create_method_entry() ----
    create_method_entry <- function(category, protocol) {
      # Get campaign name
      campaign_name_short <- get_session_data_safe(
        session,
        "campaignData",
        "CAMPAIGN_NAME_SHORT",
        "UnknownCampaign"
      )

      # Generate protocol ID
      protocol_id <- generate_protocol_id(
        category,
        protocol,
        1,
        campaign_name_short
      )

      tibble(
        PROTOCOL_ID = protocol_id,
        CAMPAIGN_NAME = campaign_name_short,
        PROTOCOL_CATEGORY = category,
        PROTOCOL_NAME = protocol,
        PROTOCOL_COMMENT = NA_character_
      )
    }

    # 3. Observers and Reactives ----

    ## observe: Update protocol name dropdown when category changes ----
    # upstream: input$protocol_category_select
    # downstream: input$protocol_name_select choices
    observe({
      category <- input$protocol_category_select

      if (isTruthy(category)) {
        available_protocols <- protocol_options |>
          filter(Protocol_Type == category) |>
          select(Long_Name, Short_Name) |>
          deframe()

        if (!is.null(available_protocols)) {
          choices <- c("Select protocol..." = "", available_protocols)
          updateSelectInput(
            session,
            "protocol_name_select",
            choices = choices
          )
        } else {
          updateSelectInput(
            session,
            "protocol_name_select",
            choices = c("No protocols available" = "")
          )
        }
      } else {
        updateSelectInput(
          session,
          "protocol_name_select",
          choices = c("Select category first..." = "")
        )
      }
    }) |>
      bindEvent(
        input$protocol_category_select,
        ignoreInit = FALSE
      )

    ## observe ~ bindEvent: Add selected method to table ----
    # upstream: input$add_method, input$protocol_category_select, input$protocol_name_select
    # downstream: session$userData$reactiveValues$methodsData
    observe({
      category <- input$protocol_category_select
      protocols <- input$protocol_name_select

      if (isTruthy(category) && isTruthy(protocols)) {
        # Create entries for all selected protocols
        new_entries <- lapply(protocols, function(protocol) {
          create_method_entry(category, protocol)
        }) |>
          bind_rows()

        # CHANGED: Update userData instead of moduleState
        session$userData$reactiveValues$methodsData <- bind_rows(
          session$userData$reactiveValues$methodsData,
          new_entries
        )
      }
    }) |>
      bindEvent(input$add_method)

    ## observe ~ bindEvent: Update methods data from table edits ----
    # upstream: input$methods_table (HOT edits)
    # downstream: session$userData$reactiveValues$methodsData
    observe({
      # CHANGED: Update userData instead of moduleState
      if (
        isTruthy(input$methods_table) &&
          nrow(session$userData$reactiveValues$methodsData) > 0
      ) {
        tryCatch(
          {
            updated_data <- hot_to_r(input$methods_table)

            # Check if any rows were actually removed (row count decreased)
            if (
              nrow(updated_data) <
                nrow(session$userData$reactiveValues$methodsData)
            ) {
              # Get current HOT selection - this might contain row indices
              # that were marked for deletion
              session$userData$reactiveValues$methodsData <- updated_data
            } else {
              # Normal update - preserve all rows
              # Update only the editable fields, keep generated fields
              for (i in 1:nrow(updated_data)) {
                if (i <= nrow(session$userData$reactiveValues$methodsData)) {
                  session$userData$reactiveValues$methodsData[
                    i,
                    "PROTOCOL_CATEGORY"
                  ] <-
                    updated_data[i, "PROTOCOL_CATEGORY"]
                  session$userData$reactiveValues$methodsData[
                    i,
                    "PROTOCOL_NAME"
                  ] <-
                    updated_data[i, "PROTOCOL_NAME"]
                  session$userData$reactiveValues$methodsData[
                    i,
                    "PROTOCOL_COMMENT"
                  ] <-
                    updated_data[i, "PROTOCOL_COMMENT"]
                }
              }
            }
          },
          error = function(e) {
            showNotification(
              paste0(
                "Error updating methods data from table. Please report to admin. Additional details:",
                e$message
              ),
              type = "error"
            )
          }
        )
      }
    }) |>
      bindEvent(input$methods_table, ignoreInit = TRUE, ignoreNULL = TRUE)

    ## observe: Check overall validation status and update reactiveValues ----
    # upstream: session$userData$reactiveValues$methodsData, iv
    # downstream: session$userData$reactiveValues$methodsDataValid
    # CHANGED: Update validation status in userData
    observe({
      validation_result <- iv$is_valid()

      if (
        validation_result &&
          nrow(session$userData$reactiveValues$methodsData) > 0
      ) {
        session$userData$reactiveValues$methodsDataValid <- TRUE
      } else {
        session$userData$reactiveValues$methodsDataValid <- FALSE
      }
    }) |>
      bindEvent(iv$is_valid(), session$userData$reactiveValues$methodsDataValid)

    ## observe ~ bindEvent: Load methods data from LLM extraction ----
    observe({
      methodsDataLLM <- session$userData$reactiveValues$methodsDataLLM

      # CHANGED: Update userData directly instead of moduleState
      # If methodsDataLLM has rows, replace existing data
      if (nrow(methodsDataLLM) > 0) {
        session$userData$reactiveValues$methodsData <- methodsDataLLM
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$methodsDataLLM,
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

    ## observe: Update protocol IDs and campaign names ----
    ## upstream: session$userData$reactiveValues$methodsData, session$userData$reactiveValues$campaignData
    ## downstream: methods HOT table and data objects
    observe({
      # CHANGED: Reference userData instead of moduleState
      if (nrow(session$userData$reactiveValues$methodsData) > 0) {
        campaign_name <- get_session_data_safe(
          session,
          "campaignData",
          "CAMPAIGN_NAME",
          "UnknownCampaign"
        )

        campaign_name_short <- get_session_data_safe(
          session,
          "campaignData",
          "CAMPAIGN_NAME_SHORT",
          "UnknownCampaign"
        )

        # Update data with proper protocol IDs and campaign names
        updated_data <- session$userData$reactiveValues$methodsData |>
          group_by(PROTOCOL_CATEGORY) |>
          mutate(sequence = row_number()) |>
          ungroup() |> #there might meant to be a rowwise() here, check master before the pr if necessary...
          mutate(
            PROTOCOL_ID = generate_protocol_id(
              PROTOCOL_CATEGORY,
              PROTOCOL_NAME,
              sequence,
              campaign_name_short
            ),
            CAMPAIGN_NAME = campaign_name
          ) |>

          select(-sequence) |>
          relocate(PROTOCOL_ID)

        # Only update if there are actual changes to avoid infinite loops
        # CHANGED: Compare and update within userData
        if (
          !identical(session$userData$reactiveValues$methodsData, updated_data)
        ) {
          session$userData$reactiveValues$methodsData <- updated_data
        }
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$methodsData,
        session$userData$reactiveValues$campaignData,
        ignoreInit = TRUE
      )

    ## observer: receive data from session$userData$reactiveValues$methodsData (import) ----
    ## and update module data
    # CHANGED: Data is already in userData, just log the event
    observe({
      print_dev("Loaded saved data into methods userData.")
    }) |>
      bindEvent(
        session$userData$reactiveValues$saveExtractionComplete,
        session$userData$reactiveValues$saveExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    # 4. Outputs ----

    ## output: methods_table ----
    # upstream: session$userData$reactiveValues$methodsData
    # downstream: UI table display
    output$methods_table <- renderRHandsontable({
      # CHANGED: Reference userData instead of moduleState
      # if (nrow(session$userData$reactiveValues$methodsData) == 0) {
      #   # Show empty table structure
      #   rhandsontable(
      #     initialise_methods_tibble(),
      #     selectCallback = TRUE,
      #     width = NULL
      #   ) |>
      #     hot_table(overflow = "visible", stretchH = "all") |>
      #     hot_context_menu(
      #       allowRowEdit = TRUE, # Enable row operations
      #       allowColEdit = FALSE, # Disable column operations
      #       customOpts = list(
      #         # Only include remove_row in the menu
      #         "row_above" = NULL,
      #         "row_below" = NULL,
      #         "remove_row" = list(
      #           name = "Remove selected rows"
      #         )
      #       )
      #     )
      # } else {
      rhandsontable(
        session$userData$reactiveValues$methodsData,
        selectCallback = TRUE,
        stretchH = "all",
        width = NULL
      ) |>
        hot_table(overflow = "visible", stretchH = "all") |>
        hot_cols(
          manualColumnResize = TRUE,
          colWidths = c(100, 100, 100, 200, 600)
        ) |>
        hot_col(
          "PROTOCOL_ID",
          readOnly = TRUE,
          renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          td.style.backgroundColor = '#f5f5f5';
          td.style.fontStyle = 'italic';
        }
      "
        ) |>
        hot_col(
          "CAMPAIGN_NAME",
          readOnly = TRUE,
          renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          td.style.backgroundColor = '#f5f5f5';
        }
      "
        ) |>
        hot_col(
          "PROTOCOL_CATEGORY",
          type = "dropdown",
          source = protocol_categories_vocabulary(),
          strict = TRUE
        ) |>
        hot_col(
          "PROTOCOL_NAME",
          type = "dropdown",
          source = protocol_options_vocabulary(),
          strict = TRUE
        ) |>
        hot_col(
          "PROTOCOL_COMMENT",
          type = "text"
        ) |>
        hot_context_menu(
          allowRowEdit = TRUE, # Enable row operations
          allowColEdit = FALSE, # Disable column operations
          customOpts = list(
            # Only include remove_row in the menu
            "row_above" = NULL,
            "row_below" = NULL,
            "remove_row" = list(
              name = "Remove selected rows"
            )
          )
        )
      # }
    })

    ## output: validation_reporter ----
    # upstream: session$userData$reactiveValues$methodsDataValid, mod_llm output
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
        session$userData$reactiveValues$methodsDataValid
      ) {
        div(
          bs_icon("clipboard2-check"),
          paste(
            "All methods data validated successfully.",
            nrow(session$userData$reactiveValues$methodsData),
            "method(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Add at least one valid method to proceed. Use the form above to add methods.",
          class = "validation-status validation-warning"
        )
      }

      div(llm_indicator, validation_status, class = "validation-container")
    })

    ## output: validated_data_display ----
    # upstream: session$userData$reactiveValues$methodsData (when valid)
    # downstream: UI data display
    output$validated_data_display <- renderText({
      # CHANGED: Show data only when valid, reference userData
      if (
        session$userData$reactiveValues$methodsDataValid &&
          nrow(session$userData$reactiveValues$methodsData) > 0
      ) {
        # Format each method as a separate entry
        method_entries <- lapply(
          1:nrow(session$userData$reactiveValues$methodsData),
          function(i) {
            method <- session$userData$reactiveValues$methodsData[i, ]
            method_lines <- sapply(names(method), function(name) {
              value <- method[[name]]
              if (is.na(value) || is.null(value) || value == "") {
                paste0("  ", name, " = NA")
              } else if (is.character(value)) {
                paste0("  ", name, " = '", value, "'")
              } else {
                paste0("  ", name, " = ", as.character(value))
              }
            })
            paste0("Method ", i, ":\n", paste(method_lines, collapse = "\n"))
          }
        )

        paste(method_entries, collapse = "\n\n")
      } else {
        "# Methods data will appear here when valid methods are added"
      }
    })
  })
}

## To be copied in the UI ----
# mod_methods_ui("methods_1")

## To be copied in the server ----
# methods_data <- mod_methods_server("methods_1")
