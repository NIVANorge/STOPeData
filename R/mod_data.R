# Data Entry Module ----
# A Shiny module for consolidating setup data and entering measurement values

#' Data UI Function ----
#'
#' @description A shiny Module for measurement data entry with consolidated sample-parameter combinations.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card card_header card_body accordion accordion_panel
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs
#' @export
mod_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main data entry card ----
    card(
      card_header("Measurement Data Entry"),
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "Data Entry Information",
            icon = bs_icon("info-circle"),
            "This module consolidates all setup data and presents sample-parameter combinations for measurement data entry. All modules must be completed and validated before data entry is enabled. Enter measured concentrations, detection limits, and associated metadata for each combination."
          )
        ),

        ## Validation status overview ----
        accordion(
          id = ns("validation_accordion"),
          accordion_panel(
            value = "validation_accordion_panel",
            style = "margin: 20px 0;",
            title = "Module Validation Status",
            icon = bs_icon("exclamation-triangle"),
            uiOutput(ns("validation_overview")),
          )
        ),

        ## Data entry controls ----
        div(
          style = "margin: 20px 0;",
          h5("Measurement Data Entry"),
          p(
            "Complete measurement data for all sample-parameter combinations below. All fields marked as required must be filled.",
            class = "text-muted"
          )
        ),

        ## Measurement data table ----
        rHandsontableOutput(
          ns("measurement_table"),
          width = "100%",
          height = "100%"
        ),

        ## Validation status ----
        div(
          style = "margin-top: 15px;",
          uiOutput(ns("validation_reporter"))
        ),

        ## Raw data accordion ----
        accordion(
          id = ns("data_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Preview Complete Dataset",
            icon = bs_icon("eye"),
            verbatimTextOutput(ns("complete_data_preview"))
          )
        )
      )
    )
  )
}

#' Data Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_to_r hot_col hot_context_menu
#' @importFrom shinyjs enable disable
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @importFrom dplyr cross_join mutate select rename
#' @importFrom tibble tibble
#' @export
mod_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      all_modules_valid = FALSE,
      data_entry_ready = FALSE,
      measurement_combinations = data.frame(),
      complete_dataset = NULL,
      is_valid = FALSE
    )

    ## Controlled vocabulary options ----
    measured_flags <- c("", "<LOQ", "<LOD")

    measured_units <- c(
      "mg/L",
      "ug/L",
      "ng/L",
      "pg/L",
      "mol/L",
      "mmol/L",
      "umol/L",
      "nmol/L",
      "pmol/L",
      "M",
      "mM",
      "uM",
      "nM",
      "pM",
      "deg C",
      "K",
      "Pa",
      "bar",
      "atm",
      "psu",
      "%",
      "ppm",
      "ppb",
      "ppt",
      "Gy",
      "Gy/h",
      "mGy",
      "uGy",
      "J/m2",
      "W/m2",
      "mm",
      "cm",
      "m",
      "g",
      "kg",
      "Other"
    )

    ## InputValidator for measurement data validation ----
    iv <- InputValidator$new()
    iv$add_rule("measurement_table_validation", function(value) {
      if (!moduleState$data_entry_ready) {
        return("Complete all setup modules before entering measurement data")
      }

      if (nrow(moduleState$measurement_combinations) == 0) {
        return("No sample-parameter combinations available")
      }

      # Check that all required fields are filled
      required_fields <- c("MEASURED_UNIT")

      for (i in 1:nrow(moduleState$measurement_combinations)) {
        # Check required fields
        for (field in required_fields) {
          value <- moduleState$measurement_combinations[i, field]
          if (is.na(value) || value == "") {
            return(paste("Row", i, "is missing required field:", field))
          }
        }

        # Conditional validation for measurement values and flags
        measured_flag <- moduleState$measurement_combinations[
          i,
          "MEASURED_FLAG"
        ]
        measured_value <- moduleState$measurement_combinations[
          i,
          "MEASURED_VALUE"
        ]
        loq_value <- moduleState$measurement_combinations[i, "LOQ_VALUE"]
        lod_value <- moduleState$measurement_combinations[i, "LOD_VALUE"]

        # If flag is blank, measured_value is required
        if (is.na(measured_flag) || measured_flag == "") {
          if (is.na(measured_value) || measured_value == "") {
            return(paste(
              "Row",
              i,
              "requires MEASURED_VALUE when no flag is set"
            ))
          }
        }

        # If flag is <LOQ, LOQ_VALUE is required
        if (!is.na(measured_flag) && measured_flag == "<LOQ") {
          if (is.na(loq_value) || loq_value == "") {
            return(paste("Row", i, "requires LOQ_VALUE when flag is '<LOQ'"))
          }
        }

        # If flag is <LOD, LOD_VALUE is required
        if (!is.na(measured_flag) && measured_flag == "<LOD") {
          if (is.na(lod_value) || lod_value == "") {
            return(paste("Row", i, "requires LOD_VALUE when flag is '<LOD'"))
          }
        }

        # Validate numeric values are positive
        numeric_fields <- c(
          "MEASURED_VALUE",
          "LOQ_VALUE",
          "LOD_VALUE",
          "MEASURED_SD"
        )
        for (field in numeric_fields) {
          field_value <- moduleState$measurement_combinations[i, field]
          if (
            !is.na(field_value) &&
              field_value != "" &&
              as.numeric(field_value) < 0
          ) {
            return(paste("Row", i, "has invalid", field, "(must be positive)"))
          }
        }
      }

      NULL # All validations passed
    })

    iv$enable()

    # 2. Helper functions ----

    ## Check if all required modules are validated ----
    check_all_modules_valid <- function() {
      required_data <- list(
        campaign = session$userData$reactiveValues$campaignData,
        references = session$userData$reactiveValues$referencesData,
        sites = session$userData$reactiveValues$sitesData,
        parameters = session$userData$reactiveValues$parametersData,
        compartments = session$userData$reactiveValues$compartmentsData,
        methods = session$userData$reactiveValues$methodsData,
        samples = session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
          session$userData$reactiveValues$sampleData
      )

      all(sapply(required_data, function(x) !is.null(x)))
    }

    ## Get validation status for each module ----
    get_module_status <- function() {
      modules <- list(
        Campaign = session$userData$reactiveValues$campaignData,
        References = session$userData$reactiveValues$referencesData,
        Sites = session$userData$reactiveValues$sitesData,
        Parameters = session$userData$reactiveValues$parametersData,
        Compartments = session$userData$reactiveValues$compartmentsData,
        Methods = session$userData$reactiveValues$methodsData,
        Samples = session$userData$reactiveValues$sampleData,
        Biota = session$userData$reactiveValues$biotaValidated
      )

      status_list <- lapply(names(modules), function(name) {
        data <- modules[[name]]
        if (name == "Biota") {
          # Special handling for biota validation flag
          status <- if (isTruthy(data)) "✓ Validated" else "⚠ No biota samples"
          count <- if (isTruthy(session$userData$reactiveValues$biotaData)) {
            nrow(session$userData$reactiveValues$biotaData)
          } else {
            "No biota samples"
          }
        } else {
          status <- if (isTruthy(data) && nrow(data) > 0) "✓ Validated" else
            "⚠ Pending"
          count <- if (isTruthy(data)) {
            nrow(data)
          } else {
            0
          }
        }

        list(module = name, status = status, count = count)
      })

      return(status_list)
    }

    get_protocols <- function() {
      session$userData$reactiveValues$methodsData$PROTOCOL_NAME
      print(session$userData$reactiveValues$methodsData$PROTOCOL_NAME)
    }

    ## Create sample-parameter combinations for measurement entry ----
    create_measurement_combinations <- function() {
      # Get all validated data
      samples_data <- session$userData$reactiveValues$sampleDataWithBiota %|truthy|%
        session$userData$reactiveValues$sampleData
      parameters_data <- session$userData$reactiveValues$parametersData
      campaign_data <- session$userData$reactiveValues$campaignData
      reference_data <- session$userData$reactiveValues$referencesData

      if (is.null(samples_data) || is.null(parameters_data)) {
        return(data.frame())
      }

      # Create cartesian product of samples and parameters
      combinations <- cross_join(
        samples_data |>
          select(
            SAMPLE_ID,
            SITE_CODE,
            SAMPLING_DATE,
            ENVIRON_COMPARTMENT,
            ENVIRON_COMPARTMENT_SUB,
            REPLICATE
          ),
        parameters_data |> select(PARAMETER_NAME, PARAMETER_TYPE, MEASURED_TYPE)
      )

      # Add campaign and reference info
      combinations <- combinations |>
        mutate(
          CAMPAIGN_NAME = campaign_data$CAMPAIGN_NAME,
          REFERENCE_ID = reference_data$REFERENCE_TYPE, # Simplified for now

          # Add measurement fields with empty defaults
          MEASURED_FLAG = "",
          MEASURED_VALUE = NA,
          MEASURED_SD = NA,
          MEASURED_UNIT = "mg/L", # Default unit
          LOQ_VALUE = NA,
          LOQ_UNIT = "mg/L", # Should match MEASURED_UNIT
          LOD_VALUE = NA,
          LOD_UNIT = "mg/L", # Should match MEASURED_UNIT

          # Add method info (simplified)
          SAMPLING_PROTOCOL = "Not reported",
          FRACTIONATION_PROTOCOL = "Not reported",
          EXTRACTION_PROTOCOL = "Not reported",
          ANALYTICAL_PROTOCOL = "Not reported"
        )

      return(combinations)
    }

    ## Initialize measurement combinations data frame ----
    init_measurement_df <- function() {
      tibble(
        SAMPLE_ID = character(0),
        SITE_CODE = character(0),
        PARAMETER_NAME = character(0),
        SAMPLING_DATE = character(0),
        ENVIRON_COMPARTMENT = character(0),
        ENVIRON_COMPARTMENT_SUB = character(0),
        REPLICATE = integer(0),
        MEASURED_FLAG = character(0),
        MEASURED_VALUE = numeric(0),
        MEASURED_SD = numeric(0),
        MEASURED_UNIT = character(0),
        LOQ_VALUE = numeric(0),
        LOQ_UNIT = character(0),
        LOD_VALUE = numeric(0),
        LOD_UNIT = character(0),
        SAMPLING_PROTOCOL = character(0),
        EXTRACTION_PROTOCOL = character(0),
        FRACTIONATION_PROTOCOL = character(0),
        ANALYTICAL_PROTOCOL = character(0)
      )
    }

    # 3. Observers and Reactives ----

    ## observe: Check validation status continuously ----
    # upstream: all session$userData$reactiveValues
    # downstream: moduleState$all_modules_valid, moduleState$data_entry_ready
    observe({
      moduleState$all_modules_valid <- check_all_modules_valid()

      if (moduleState$all_modules_valid) {
        moduleState$data_entry_ready <- TRUE
        # Create measurement combinations when ready
        moduleState$measurement_combinations <- create_measurement_combinations()
        print_dev(glue(
          "mod_data: All modules validated, created {nrow(moduleState$measurement_combinations)} measurement combinations"
        ))
      } else {
        moduleState$data_entry_ready <- FALSE
        moduleState$measurement_combinations <- init_measurement_df()
        print_dev("mod_data: Some modules pending, data entry disabled")
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$campaignData,
        session$userData$reactiveValues$referencesData,
        session$userData$reactiveValues$sitesData,
        session$userData$reactiveValues$parametersData,
        session$userData$reactiveValues$compartmentsData,
        session$userData$reactiveValues$methodsData,
        session$userData$reactiveValues$sampleData,
        session$userData$reactiveValues$biotaValidated
      )

    ## observe: Handle table changes ----
    # upstream: input$measurement_table changes
    # downstream: moduleState$measurement_combinations
    observe({
      if (!is.null(input$measurement_table) && moduleState$data_entry_ready) {
        updated_data <- hot_to_r(input$measurement_table)
        moduleState$measurement_combinations <- updated_data

        # Update LOQ_UNIT and LOD_UNIT to match MEASURED_UNIT
        for (i in 1:nrow(moduleState$measurement_combinations)) {
          measured_unit <- moduleState$measurement_combinations[
            i,
            "MEASURED_UNIT"
          ]
          if (!is.na(measured_unit) && measured_unit != "") {
            moduleState$measurement_combinations[i, "LOQ_UNIT"] <- measured_unit
            moduleState$measurement_combinations[i, "LOD_UNIT"] <- measured_unit
          }
        }
      }
    }) |>
      bindEvent(input$measurement_table)

    ## observe: Check measurement data validation and save to session ----
    # upstream: moduleState$measurement_combinations, iv
    # downstream: moduleState$is_valid, moduleState$complete_dataset, session$userData
    observe({
      validation_result <- iv$is_valid()

      if (validation_result && nrow(moduleState$measurement_combinations) > 0) {
        moduleState$is_valid <- TRUE

        # Create complete dataset by merging all components
        complete_data <- moduleState$measurement_combinations

        # Add campaign info
        campaign_data <- session$userData$reactiveValues$campaignData
        for (col in names(campaign_data)) {
          if (!col %in% names(complete_data)) {
            complete_data[[col]] <- campaign_data[[col]]
          }
        }

        moduleState$complete_dataset <- complete_data
        session$userData$reactiveValues$dataData <- moduleState$complete_dataset

        print_dev(glue(
          "mod_data: Data validated and saved - {nrow(moduleState$complete_dataset)} complete records"
        ))
      } else {
        moduleState$is_valid <- FALSE
        moduleState$complete_dataset <- NULL
        session$userData$reactiveValues$dataData <- NULL
        print_dev("mod_data: Data validation failed")
      }
    })

    # 4. Outputs ----

    ## output: validation_overview ----
    # upstream: session data
    # downstream: UI validation status display
    output$validation_overview <- renderUI({
      status_list <- get_module_status()

      status_divs <- lapply(status_list, function(item) {
        status_class <- if (grepl("✓", item$status)) {
          "validation-status validation-complete"
        } else {
          "validation-status validation-warning"
        }

        div(
          class = status_class,
          style = "margin: 5px 0; padding: 8px; border-radius: 4px;",
          strong(item$module),
          ": ",
          item$status,
          " (",
          item$count,
          if (item$module == "Campaign" || item$module == "References")
            " record" else " records",
          ")"
        )
      })

      div(
        do.call(tagList, status_divs),
        hr(),
        if (moduleState$all_modules_valid) {
          div(
            bs_icon("check-circle"),
            " All modules validated - ",
            nrow(moduleState$measurement_combinations),
            " measurement combinations ready",
            class = "validation-status validation-complete",
            style = "margin-top: 10px; padding: 10px; border-radius: 4px;"
          )
        } else {
          div(
            bs_icon("exclamation-triangle"),
            " Complete all modules to enable data entry",
            class = "validation-status validation-warning",
            style = "margin-top: 10px; padding: 10px; border-radius: 4px;"
          )
        }
      )
    })

    ## output: measurement_table ----
    # upstream: moduleState$measurement_combinations, moduleState$data_entry_ready
    # downstream: UI table display
    output$measurement_table <- renderRHandsontable({
      if (
        !moduleState$data_entry_ready ||
          nrow(moduleState$measurement_combinations) == 0
      ) {
        rhandsontable(
          init_measurement_df(),
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all")
      } else {
        rhandsontable(
          moduleState$measurement_combinations,
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
          hot_col(
            c(
              "SAMPLE_ID",
              "SITE_CODE",
              "PARAMETER_NAME",
              "SAMPLING_DATE",
              "ENVIRON_COMPARTMENT",
              "REPLICATE"
            ),
            readOnly = TRUE
          ) |>
          #
          # # Configure measurement fields
          hot_col(
            "MEASURED_FLAG",
            type = "dropdown",
            source = measured_flags,
            strict = TRUE
          ) |>
          hot_col(
            c("MEASURED_VALUE", "MEASURED_SD", "LOQ_VALUE", "LOD_VALUE"),
            type = "numeric",
            format = "0.0000"
          ) |>
          hot_col(
            c("MEASURED_UNIT", "LOQ_UNIT", "LOD_UNIT"),
            type = "dropdown",
            source = measured_units,
            strict = TRUE
          ) |>
          hot_col(
            c(
              "SAMPLING_PROTOCOL",
              "FRACTIONATION_PROTOCOL",
              "EXTRACTION_PROTOCOL",
              "ANALYTICAL_PROTOCOL"
            ),
            type = "dropdown",
            source = methods,
            strict = TRUE
          ) |>
          hot_context_menu(
            allowRowEdit = FALSE, # Disable row operations for measurement data
            allowColEdit = FALSE, # Disable column operations
            customOpts = list()
          )
      }
    })

    ## output: validation_reporter ----
    # upstream: moduleState$is_valid, moduleState$data_entry_ready
    # downstream: UI validation status
    output$validation_reporter <- renderUI({
      if (!moduleState$data_entry_ready) {
        div(
          bs_icon("info-circle"),
          "Complete all setup modules to enable measurement data entry.",
          class = "validation-status validation-info"
        )
      } else if (moduleState$is_valid) {
        div(
          bs_icon("clipboard2-check"),
          paste(
            "All measurement data validated successfully.",
            nrow(moduleState$measurement_combinations),
            "complete measurement record(s)."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Complete all measurement data fields. Check flags, values, and units for consistency.",
          class = "validation-status validation-warning"
        )
      }
    })

    ## output: complete_data_preview ----
    # upstream: moduleState$complete_dataset
    # downstream: UI data display
    output$complete_data_preview <- renderText({
      if (isTruthy(moduleState$complete_dataset)) {
        # Show overview of complete dataset
        dataset_info <- paste0(
          "Complete Dataset Overview:\n",
          "Total Records: ",
          nrow(moduleState$complete_dataset),
          "\n",
          "Total Columns: ",
          ncol(moduleState$complete_dataset),
          "\n",
          "Sample-Parameter Combinations: ",
          length(unique(paste(
            moduleState$complete_dataset$SAMPLE_ID,
            moduleState$complete_dataset$PARAMETER_NAME
          ))),
          "\n",
          "Unique Sites: ",
          length(unique(moduleState$complete_dataset$SITE_CODE)),
          "\n",
          "Unique Parameters: ",
          length(unique(moduleState$complete_dataset$PARAMETER_NAME)),
          "\n\n",
          "First 3 records:\n",
          paste(
            capture.output(head(moduleState$complete_dataset, 3)),
            collapse = "\n"
          )
        )

        return(dataset_info)
      } else if (moduleState$data_entry_ready) {
        "# Complete dataset will appear here when all measurement data is validated"
      } else {
        "# Complete all setup modules to preview dataset structure"
      }
    })
  })
}

## To be copied in the UI ----
# mod_data_ui("data_1")

## To be copied in the server ----
# mod_data_server("data_1")
