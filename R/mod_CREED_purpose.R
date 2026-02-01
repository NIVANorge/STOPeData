# CREED Purpose Statement Module ----
# A Shiny module for defining CREED assessment purpose and thresholds

#' CREED_purpose UI Function
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textAreaInput h5 p strong div actionButton fileInput downloadButton hr h5
#' @importFrom bslib layout_columns input_task_button
#' @importFrom bsicons bs_icon
#' @import eDataDRF
mod_CREED_purpose_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Info section ----
    # Purpose statement section ----
    p(
      "Describe the objective for which the usability of the dataset is assessed, including any required dataset thresholds. 
      CREED Relevance Questions (RV01 - RV11) are based on the study purpose. Reliability Questions (RB01 - RB19) are common across all studies.",
      class = "text-muted"
    ),
    textAreaInput(
      inputId = ns("purpose_statement"),
      label = "Purpose Statement (Pre-made copper statement/thresholds):",
      placeholder = "Enter the assessment purpose and any specific requirements...",
      rows = 6,
      width = "100%",
      value = copper_CREED_purpose_statement()$study_purpose
    ),

    hr(),

    # Import/Export section ----
    div(
      "You can usually import/export purpose statements here, but since we're just doing a copper AEP for now I've hard-coded it.",
      class = "text-muted"
    ),
    # layout_column_wrap(
    #   width = "300px",

    #   fileInput(
    #     inputId = ns("import_file"),
    #     label = "Import Purpose & Thresholds (YAML)",
    #     accept = c(".yaml", ".yml")
    #   ),
    #   input_task_button(
    #     id = ns("import_btn"),
    #     label = "Load Imported Data",
    #     icon = bs_icon("upload")
    #   ),
    #   downloadButton(
    #     outputId = ns("export_btn"),
    #     label = "Export Purpose & Thresholds",
    #     icon = icon("download")
    #   ),
    #   input_task_button(
    #     id = ns("clear_all"),
    #     label = "Clear All Fields",
    #     icon = bs_icon("trash"),
    #     class = "btn-outline-danger"
    #   )
    # ),

    hr(),

    # Purpose summary table section ----
    p(
      "Enter thresholds for 'Partly Met' (minimum requirements) and 'Fully Met' (optimal requirements). ",
      "Leave fields blank if no specific threshold is needed.",
      class = "text-muted"
    ),

    # RV1 ----
    h5(
      copper_CREED_purpose_statement()$criteria[[1]]$name
    ),
    layout_column_wrap(
      width = "400px",
      textAreaInput(
        inputId = ns("RV1_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[1]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                1
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  1
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV1_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[1]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                1
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  1
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV2 ----
    h5(copper_CREED_purpose_statement()$criteria[[2]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV2_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[2]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                2
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  2
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV2_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[2]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                2
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  2
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV3 ----
    h5(copper_CREED_purpose_statement()$criteria[[3]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV3_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[3]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                3
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  3
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV3_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[3]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                3
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  3
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV4 ----
    h5(copper_CREED_purpose_statement()$criteria[[4]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV4_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[4]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                4
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  4
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV4_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[4]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                4
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  4
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV5 ----
    h5(copper_CREED_purpose_statement()$criteria[[5]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV5_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[5]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                5
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  5
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV5_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[5]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                5
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  5
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV6 ----
    h5(copper_CREED_purpose_statement()$criteria[[6]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV6_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[6]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                6
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  6
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV6_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[6]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                6
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  6
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV7 ----
    h5(copper_CREED_purpose_statement()$criteria[[7]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV7_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[7]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                7
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  7
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV7_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[7]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                7
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  7
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV8 ----
    h5(copper_CREED_purpose_statement()$criteria[[8]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV8_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[8]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                8
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  8
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV8_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[8]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                8
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  8
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV9 ----
    h5(copper_CREED_purpose_statement()$criteria[[9]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV9_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[9]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                9
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  9
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV9_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[9]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                9
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  9
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV10 ----
    h5(copper_CREED_purpose_statement()$criteria[[10]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV10_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[
            10
          ]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                10
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  10
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV10_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[10]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                10
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  10
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # RV11 ----
    h5(copper_CREED_purpose_statement()$criteria[[11]]$name),
    layout_columns(
      col_widths = c(6, 6),
      textAreaInput(
        inputId = ns("RV11_partly_met"),
        label = "Partly Met (minimum requirements)",
        placeholder = "Enter minimum threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[
            11
          ]]$partly_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                11
              ]]$partly_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  11
                ]]$partly_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      ),
      textAreaInput(
        inputId = ns("RV11_fully_met"),
        label = "Fully Met (optimal requirements)",
        placeholder = "Enter optimal threshold for this criterion...",
        rows = 3,
        width = "100%",
        value = paste(
          copper_CREED_purpose_statement()$criteria[[11]]$fully_met$description,
          "\n",
          if (
            length(
              copper_CREED_purpose_statement()$criteria[[
                11
              ]]$fully_met$requirements
            ) >
              0
          ) {
            paste(
              paste(
                "- ",
                copper_CREED_purpose_statement()$criteria[[
                  11
                ]]$fully_met$requirements,
                collapse = "\n"
              )
            )
          } else {
            ""
          }
        )
      )
    ),

    # hr(),
    # input_task_button(
    #   id = ns("save_assessment"),
    #   label = "Save Section",
    #   icon = icon("save"),
    #   class = "btn-success"
    # ),

    # # Status section ----
    # h5("Status"),
    # uiOutput(ns("purpose_status"))
  )
}

#' CREED_purpose Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI req updateTextAreaInput showNotification
#' @importFrom yaml read_yaml write_yaml
mod_CREED_purpose_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: purpose_data ----
    purpose_data <- reactiveValues()

    # Define criterion IDs ----
    criterion_ids <- paste0("RV", 1:11)

    # 2. Reactive expressions ----

    ## reactive: collect_purpose_data ----
    # upstream: all input fields
    # downstream: purpose_data, session$userData
    collect_purpose_data <- reactive({
      # Collect purpose statement
      purpose_statement <- input$purpose_statement %||% ""

      # Collect thresholds for each criterion
      thresholds <- list()
      for (criterion_id in criterion_ids) {
        partly_met <- input[[paste0(criterion_id, "_partly_met")]] %||% ""
        fully_met <- input[[paste0(criterion_id, "_fully_met")]] %||% ""

        thresholds[[criterion_id]] <- list(
          partly_met = partly_met,
          fully_met = fully_met
        )
      }

      list(
        purpose_statement = purpose_statement,
        thresholds = thresholds,
        created_date = Sys.Date(),
        module_version = "1.0"
      )
    })

    # 3. Observers ----

    ## observe: Store data automatically ----
    # upstream: collect_purpose_data()
    # downstream: purpose_data, session$userData
    observe({
      data <- collect_purpose_data()
      purpose_data$current <- data

      # Store in session for other modules to access
      session$userData$reactiveValues$creedPurpose <- data
    }) |>
      bindEvent(
        input$purpose_statement,
        c(lapply(criterion_ids, function(x) input[[paste0(x, "_partly_met")]])),
        c(lapply(criterion_ids, function(x) input[[paste0(x, "_fully_met")]])),
        ignoreInit = FALSE,
        ignoreNULL = FALSE
      )

    ## observe: Import data: DISABLED YAMLs ----
    # upstream: input$import_btn, input$import_file
    # downstream: UI field updates
    # observe({
    #   req(input$import_file)

    #   tryCatch(
    #     {
    #       imported_data <- yaml::read_yaml(input$import_file$datapath)

    #       # Update purpose statement
    #       if (!is.null(imported_data$purpose_statement)) {
    #         updateTextAreaInput(
    #           session,
    #           "purpose_statement",
    #           value = imported_data$purpose_statement
    #         )
    #       }

    #       # Update thresholds
    #       if (!is.null(imported_data$thresholds)) {
    #         for (criterion_id in criterion_ids) {
    #           if (!is.null(imported_data$thresholds[[criterion_id]])) {
    #             criterion_data <- imported_data$thresholds[[criterion_id]]

    #             if (!is.null(criterion_data$partly_met)) {
    #               updateTextAreaInput(
    #                 session,
    #                 paste0(criterion_id, "_partly_met"),
    #                 value = criterion_data$partly_met
    #               )
    #             }

    #             if (!is.null(criterion_data$fully_met)) {
    #               updateTextAreaInput(
    #                 session,
    #                 paste0(criterion_id, "_fully_met"),
    #                 value = criterion_data$fully_met
    #               )
    #             }
    #           }
    #         }
    #       }

    #       showNotification(
    #         "Purpose data imported successfully",
    #         type = "message"
    #       )
    #     },
    #     error = function(e) {
    #       showNotification(
    #         paste("Import failed:", e$message),
    #         type = "error"
    #       )
    #     }
    #   )
    # }) |>
    #   bindEvent(input$import_btn)

    ## observe: Clear all fields ----
    # upstream: input$clear_all
    # downstream: UI field updates
    observe({
      # Clear purpose statement
      updateTextAreaInput(session, "purpose_statement", value = "")

      # Clear all thresholds
      for (criterion_id in criterion_ids) {
        updateTextAreaInput(
          session,
          paste0(criterion_id, "_partly_met"),
          value = ""
        )
        updateTextAreaInput(
          session,
          paste0(criterion_id, "_fully_met"),
          value = ""
        )
      }

      showNotification(
        "All fields cleared",
        type = "message"
      )
    }) |>
      bindEvent(input$clear_all)

    # 4. Outputs ----

    ## output: Export handler: DISABLED ----
    # upstream: collect_purpose_data() Do we actually want to use YAML here?
    # downstream: YAML download
    # output$export_btn <- downloadHandler(
    #   filename = function() {
    #     paste0("CREED_purpose_", Sys.Date(), ".yaml")
    #   },
    #   content = function(file) {
    #     data <- collect_purpose_data()
    #     yaml::write_yaml(data, file)
    #   },
    #   contentType = "application/x-yaml"
    # )

    ## output: purpose_status ----
    # upstream: purpose_data$current
    # downstream: UI status display
    output$purpose_status <- renderUI({
      if (is.null(purpose_data$current)) {
        div(
          bs_icon("exclamation-triangle", class = "text-warning"),
          "No purpose data entered yet",
          class = "text-muted"
        )
      } else {
        data <- purpose_data$current

        # Count completed fields
        purpose_complete <- nchar(data$purpose_statement %||% "") > 0

        thresholds_with_data <- sum(sapply(data$thresholds, function(x) {
          nchar(x$partly_met %||% "") > 0 || nchar(x$fully_met %||% "") > 0
        }))

        div(
          if (purpose_complete) {
            span(
              bs_icon("check-circle", class = "text-success"),
              "Purpose statement complete",
              br()
            )
          } else {
            span(
              bs_icon("exclamation-circle", class = "text-warning"),
              "Purpose statement needed",
              br()
            )
          },
          span(
            bs_icon("list-task"),
            paste(
              "Thresholds defined for",
              thresholds_with_data,
              "of",
              length(criterion_ids),
              "criteria"
            )
          )
        )
      }
    })
  })
}

## To be copied in the UI ----
# mod_CREED_purpose_ui("CREED_purpose_1")

## To be copied in the server ----
# mod_CREED_purpose_server("CREED_purpose_1")
