# Export Module ----
# A Shiny module for exporting measurement data to Excel or CSV formats

#' Export UI Function ----
#' @noRd
#'
#' @importFrom shiny NS tagList downloadButton hr div
#' @importFrom bslib card card_body accordion accordion_panel layout_column_wrap
#' @importFrom bsicons bs_icon
#' @import eDataDRF
mod_export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main export card ----
    card(
      full_screen = TRUE,
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Export Information",
            icon = bs_icon("info-circle"),
            "Export your campaign data to Excel or CSV format. You can download individual datasets as CSV files (with accompanying metadata .txt), or download all data as a single Excel workbook with each dataset on a separate sheet and a metadata sheet, or as a ZIP file containing all CSV + .txt files."
          ),
          div(
            style = "margin: 10px 10px 0 10px; display: flex; align-items: center; gap: 15px;",
            actionButton(
              inputId = ns("get_data"),
              label = "Get Data from Modules",
              icon = icon("refresh"),
              class = "btn-primary"
            ),
            uiOutput(
              ns("export_status"),
            )
          )
        ),
        hr(),
        ## Individual dataset exports ----
        div(
          h4("Individual Dataset Exports (CSV + .txt)"),
          uiOutput(ns("individual_downloads"))
        ),
        hr(),
        ## Combined workbook export ----
        div(
          h4("Combined Export (Excel Workbook or Zipped CSV + .txt)"),
          uiOutput(ns("workbook_summary"), style = "margin-bottom: 10px;"),
          br(),
          uiOutput(ns("download_combined_ui"))
        )
      )
    )
  )
}

#' Export Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive renderUI downloadHandler req HTML
#' @importFrom openxlsx2 wb_workbook wb_add_worksheet wb_add_data wb_save
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom bsicons bs_icon
#' @importFrom bslib layout_column_wrap
#' @importFrom golem get_golem_version print_dev
#' @importFrom readr write_excel_csv
#' @importFrom zip zip
#' @import eDataDRF
mod_export_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      export_ready = FALSE,
      available_datasets = character(0),
      campaign_name = "Unknown_Campaign",
      dataset_dimensions = list()
    )

    # 3. Observers and Reactives ----

    ## observe: Check data availability ----
    # upstream: input$get_data (action button)
    # downstream: moduleState$export_ready, moduleState$available_datasets, moduleState$dataset_dimensions
    observe({
      rv <- session$userData$reactiveValues

      # Define all possible datasets
      dataset_names <- c(
        "sitesData",
        "parametersData",
        "compartmentsData",
        "referenceData",
        "campaignData",
        "methodsData",
        "samplesData",
        "biotaData",
        "measurementsData"
      )

      # Check which datasets have data and store dimensions
      available <- character(0)
      dimensions <- list()

      for (dataset in dataset_names) {
        data <- rv[[dataset]]
        if (!is.null(data) && nrow(data) > 0) {
          available <- c(available, dataset)
          dimensions[[dataset]] <- list(
            rows = nrow(data),
            cols = ncol(data)
          )
        }
      }

      moduleState$available_datasets <- available
      moduleState$dataset_dimensions <- dimensions
      moduleState$export_ready <- length(available) > 0

      # Get campaign name for filename
      if (!is.null(rv$campaignData) && nrow(rv$campaignData) > 0) {
        if ("CAMPAIGN_NAME" %in% names(rv$campaignData)) {
          campaign_name <- rv$campaignData$CAMPAIGN_NAME[1]
          if (!is.na(campaign_name) && campaign_name != "") {
            moduleState$campaign_name <- campaign_name
          }
        }
      }

      print_dev(glue(
        "mod_export: {length(available)} datasets available: {paste(available, collapse = ', ')}"
      ))
    }) |>
      bindEvent(input$get_data)

    ## observe: Create individual CSV download handlers ----
    # upstream: moduleState$available_datasets
    # downstream: individual CSV + JSON downloads
    observe({
      req(moduleState$export_ready)

      # Create a download handler for each available dataset
      lapply(moduleState$available_datasets, function(dataset_name) {
        output[[paste0("download_", dataset_name)]] <- downloadHandler(
          filename = function() {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
            display_name <- gsub(
              " ",
              "_",
              get_dataset_display_name(dataset_name)
            )
            glue("{campaign}_{display_name}_{timestamp}.zip")
          },

          content = function(file) {
            print_dev(glue(
              "mod_export: Exporting {dataset_name} as CSV + TXT..."
            ))

            rv <- session$userData$reactiveValues
            data <- rv[[dataset_name]]
            metadata <- get_export_metadata(session = session)

            # Create temporary directory for files
            temp_dir <- tempdir()
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
            display_name <- gsub(
              " ",
              "_",
              get_dataset_display_name(dataset_name)
            )
            base_name <- glue("{campaign}_{display_name}_{timestamp}")

            csv_file <- file.path(temp_dir, glue("{base_name}.csv"))
            txt_file <- file.path(temp_dir, glue("{base_name}_metadata.txt"))

            # Write CSV
            write_excel_csv(data, file = csv_file, na = "NA")

            # Write metadata TXT
            write_metadata_txt(metadata, txt_file)

            # Create zip file
            zip(
              zipfile = file,
              files = c(csv_file, txt_file),
              mode = "cherry-pick"
            )

            # Clean up temp files
            unlink(csv_file)
            unlink(txt_file)

            print_dev(glue(
              "mod_export: ZIP export complete for {dataset_name} ({nrow(data)} rows)"
            ))
          },

          contentType = "application/zip"
        )
      })
    }) |>
      bindEvent(moduleState$available_datasets)

    # 4. Outputs ----

    ## output: export_status ----
    # upstream: moduleState$export_ready, moduleState$available_datasets
    # downstream: UI status display
    output$export_status <- renderUI({
      if (moduleState$export_ready) {
        datasets <- moduleState$available_datasets

        div(
          bs_icon("check-circle"),
          glue(" Export ready: {length(datasets)} datasets available"),
          class = "validation-status validation-complete",
          style = "display: inline-block;"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          " Click to check data availability.",
          class = "validation-status validation-warning",
          style = "display: inline-block;"
        )
      }
    })

    ## output: workbook_summary ----
    # upstream: moduleState$available_datasets, moduleState$dataset_dimensions
    # downstream: UI summary of available worksheets
    output$workbook_summary <- renderUI({
      req(moduleState$export_ready)

      # Create summary text for each dataset
      summaries <- sapply(
        moduleState$available_datasets,
        function(dataset_name) {
          display_name <- get_dataset_display_name(dataset_name)
          dims <- moduleState$dataset_dimensions[[dataset_name]]
          glue("{display_name} ({dims$rows} \u00D7 {dims$cols})")
        }
      )

      summary_text <- paste(summaries, collapse = ", ")

      tags$code(summary_text)
    }) |>
      bindEvent(input$get_data)

    ## output: download_combined_ui ----
    # upstream: moduleState$export_ready
    # downstream: UI download buttons for combined exports (enabled/disabled based on data)
    output$download_combined_ui <- renderUI({
      if (moduleState$export_ready) {
        div(
          style = "display: flex; gap: 20px; margin-top: 10px;",
          downloadButton(
            outputId = ns("download_workbook"),
            label = "Download All as Excel Workbook",
            class = "btn-secondary",
            icon = icon("file-excel")
          ),
          downloadButton(
            outputId = ns(""),
            label = "Download All as CSV + JSON (ZIP)",
            class = "btn-secondary",
            icon = icon("file-zipper")
          )
        )
      } else {
        div(
          style = "display: flex; gap: 20px; margin-top: 10px;",
          tags$button(
            "Download All as Excel Workbook",
            class = "btn btn-secondary",
            disabled = "disabled",
            style = "opacity: 0.6; cursor: not-allowed;",
            icon("file-excel")
          ),
          tags$button(
            "Download All as CSV + JSON (ZIP)",
            class = "btn btn-secondary",
            disabled = "disabled",
            style = "opacity: 0.6; cursor: not-allowed;",
            icon("file-zipper")
          )
        )
      }
    })

    ## output: individual_downloads ----
    # upstream: moduleState$available_datasets, moduleState$dataset_dimensions, session$userData$reactiveValues
    # downstream: UI download buttons for each dataset (always visible, enabled/disabled based on data)
    output$individual_downloads <- renderUI({
      rv <- session$userData$reactiveValues

      # Define all possible datasets
      all_datasets <- c(
        "sitesData",
        "parametersData",
        "compartmentsData",
        "referenceData",
        "campaignData",
        "methodsData",
        "samplesData",
        "biotaData",
        "measurementsData"
      )

      # Create a card for each dataset (available or not)
      cards <- lapply(all_datasets, function(dataset_name) {
        display_name <- get_dataset_display_name(dataset_name)

        # Check if we have dimension info (only available after "Get Data" is clicked)
        if (dataset_name %in% names(moduleState$dataset_dimensions)) {
          dims <- moduleState$dataset_dimensions[[dataset_name]]
          dimensions <- glue("{dims$rows} rows \u00D7 {dims$cols} columns")
          has_data <- TRUE
        } else {
          dimensions <- "No data available"
          has_data <- FALSE
        }

        if (has_data) {
          button_element <- downloadButton(
            outputId = ns(paste0("download_", dataset_name)),
            label = "Download CSV + JSON",
            class = "btn-secondary",
            icon = icon("download")
          )
        } else {
          button_element <- tags$button(
            "Download CSV + JSON",
            class = "btn btn-secondary",
            disabled = "disabled",
            style = "opacity: 0.6; cursor: not-allowed;",
            icon("download")
          )
        }

        div(
          style = "border: 1px solid #ddd; border-radius: 4px; padding: 15px; margin: 10px 0;",
          h5(display_name),
          tags$code(dimensions),
          br(),
          br(),
          button_element
        )
      })

      layout_column_wrap(
        width = "250px",
        !!!cards
      )
    }) |>
      bindEvent(input$get_data)

    ## output: download_workbook ----
    # upstream: session$userData$reactiveValues, moduleState
    # downstream: Excel workbook download
    output$download_workbook <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        campaign <- gsub("[^A-Za-z0-9_]", "_", moduleState$campaign_name)
        glue("{campaign}_{timestamp}.xlsx")
      },

      content = function(file) {
        print_dev("mod_export: Starting Excel workbook export...")

        rv <- session$userData$reactiveValues
        metadata <- get_export_metadata(session = session)

        # Create workbook
        wb <- wb_workbook()

        # Add metadata sheet first
        metadata_tibble <- create_metadata_tibble(metadata)
        wb <- wb |>
          wb_add_worksheet(sheet = "Metadata") |>
          wb_add_data(
            sheet = "Metadata",
            x = metadata_tibble,
            startRow = 1,
            startCol = 1
          )

        # Add each available dataset as a sheet
        for (dataset_name in moduleState$available_datasets) {
          data <- rv[[dataset_name]]

          if (!is.null(data) && nrow(data) > 0) {
            # Use display name for sheet
            sheet_name <- get_dataset_display_name(dataset_name)
            sheet_name <- substr(sheet_name, 1, 31) # Excel sheet name limit

            print_dev(glue(
              "mod_export: Adding sheet '{sheet_name}' with {nrow(data)} rows"
            ))

            # Add worksheet and data
            wb <- wb |>
              wb_add_worksheet(sheet = sheet_name) |>
              wb_add_data(
                sheet = sheet_name,
                x = data,
                startRow = 1,
                startCol = 1
              )
          }
        }

        # Save workbook
        wb_save(wb, file = file, overwrite = TRUE)

        print_dev(glue(
          "mod_export: Excel file saved with {length(moduleState$available_datasets) + 1} sheets"
        ))
      },

      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    # download all csv button we turn into a function in fct_download
    output$download_allcsv <- download_all_data(
      session = session,
      moduleState = moduleState
    )
  })
}

## To be copied in the UI ----
# mod_export_ui("export_1")

## To be copied in the server ----
# mod_export_server("export_1")
