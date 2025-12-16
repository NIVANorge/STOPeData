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
#' @importFrom bslib card card_body accordion accordion_panel input_switch
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyjs useShinyjs show hide hidden
#' @import eDataDRF
#' @export
mod_data_ui <- function(id) {
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
        info_accordion(content_file = "inst/app/www/md/intro_data.md"),

        ## Dynamic validation status accordion ----
        div(
          class = "accordion_class",
          accordion(
            id = ns("validation_accordion"),
            accordion_panel(
              value = "validation_accordion_panel",
              style = "margin: 20px 0;",
              title = "Check Module Validation",
              icon = bs_icon("exclamation-triangle"),
              uiOutput(ns("validation_overview"))
            )
          )
        ),

        ## Data entry controls ----
        div(
          style = "margin: 20px 0 0 0;",
          h5("Measurement Data Entry"),
          p(
            "Complete measurement data for all sample-parameter combinations below. All fields marked as required must be filled.",
            class = "text-muted"
          )
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
    ),

    # Measurement data table card ----
    card(
      full_screen = TRUE,
      ## Measurement Data Validation status ----
      div(
        style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin: 15px 0; justify-content: center;",
        input_task_button(
          id = ns("save_table_data"),
          label = "Save and Validate"
        ),
        ### Validation status reporters ----
        div(
          class = "validation-container",
          style = "display: flex; flex-direction: column; gap: 5px;",

          # LLM extraction indicator (conditionally shown)
          hidden(div(
            id = ns("llm_extraction_indicator"),
            bs_icon("cpu"),
            "Some data populated from LLM extraction - please review for accuracy",
            class = "validation-status validation-llm"
          )),

          # Data validation status
          uiOutput(ns("data_validation_reporter"))
        ),
      ),
      div(
        rHandsontableOutput(
          ns("measurement_table"),
          width = "100%",
          height = NULL
        ),
        style = "margin-bottom: 10px; margin-left: 10px;"
      )
    )
  )
}

#' Data Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification isolate
#' @importFrom rhandsontable renderRHandsontable hot_cols rhandsontable hot_to_r hot_col hot_context_menu hot_cols
#' @importFrom shinyjs enable disable show hide
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @importFrom dplyr cross_join mutate select rename pull filter relocate left_join
#' @importFrom tibble tibble
#' @importFrom utils capture.output head
#' @importFrom purrr is_empty map_lgl
#' @import eDataDRF
#' @export
mod_data_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    # CHANGED: Keep only UI-specific transient state
    moduleState <- reactiveValues(
      data_entry_ready = FALSE,
      validation_message = ""
    )

    # reactive: are all the previous modules ready?
    # check module$ready == TRUE in all cases
    modulesStatus <- reactive({
      map_lgl(get_modules_status(), function(x) {
        x$ready
      }) |>
        all()
    })

    ## Controlled vocabulary options ----
    measured_units <- parameter_unit_vocabulary("MEASURED_UNIT")

    ## InputValidator for measurement data validation ----
    iv <- InputValidator$new()

    # Rule 1: Check if data entry is ready
    iv$add_rule("measurement_table_validation", function(value) {
      if (!moduleState$data_entry_ready) {
        moduleState$validation_message <<- "Complete all setup modules before entering measurement data"
        return("Complete all setup modules before entering measurement data")
      }
    })

    # Rule 2: Check if measurement combinations exist
    # this triggers all the time... except when we actually import data...
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(session$userData$reactiveValues$measurementsData) == 0) {
        moduleState$validation_message <<- "No sample-parameter combinations available"
        return("No sample-parameter combinations available")
      }
    })

    # Rule 3: Check for missing required fields
    # CHANGED: Reference userData instead of moduleState
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(session$userData$reactiveValues$measurementsData) > 0) {
        required_fields <- c("MEASURED_UNIT")

        for (i in 1:nrow(session$userData$reactiveValues$measurementsData)) {
          for (field in required_fields) {
            value <- session$userData$reactiveValues$measurementsData[i, field]
            if (is.na(value) || value == "") {
              message <- paste("Row", i, "is missing required field:", field)
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 4: Check MEASURED_VALUE when no flag is set
    # CHANGED: Reference userData instead of moduleState
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(session$userData$reactiveValues$measurementsData) > 0) {
        for (i in 1:nrow(session$userData$reactiveValues$measurementsData)) {
          measured_flag <- session$userData$reactiveValues$measurementsData[
            i,
            "MEASURED_FLAG"
          ]
          measured_value <- session$userData$reactiveValues$measurementsData[
            i,
            "MEASURED_VALUE"
          ]

          if (is.na(measured_flag) || measured_flag == "") {
            if (is.na(measured_value) || measured_value == "") {
              message <- paste(
                "Row",
                i,
                "requires MEASURED_VALUE when no flag is set"
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 5: Check LOQ_VALUE when flag is <LOQ
    # CHANGED: Reference userData instead of moduleState
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(session$userData$reactiveValues$measurementsData) > 0) {
        for (i in 1:nrow(session$userData$reactiveValues$measurementsData)) {
          measured_flag <- session$userData$reactiveValues$measurementsData[
            i,
            "MEASURED_FLAG"
          ]
          loq_value <- session$userData$reactiveValues$measurementsData[
            i,
            "LOQ_VALUE"
          ]

          if (!is.na(measured_flag) && measured_flag == "< LOQ") {
            if (is.na(loq_value) || loq_value == "") {
              message <- paste(
                "Row",
                i,
                "requires LOQ_VALUE when flag is '< LOQ'"
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 6: Check LOD_VALUE when flag is < LOD
    # CHANGED: Reference userData instead of moduleState
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(session$userData$reactiveValues$measurementsData) > 0) {
        for (i in 1:nrow(session$userData$reactiveValues$measurementsData)) {
          measured_flag <- session$userData$reactiveValues$measurementsData[
            i,
            "MEASURED_FLAG"
          ]
          lod_value <- session$userData$reactiveValues$measurementsData[
            i,
            "LOD_VALUE"
          ]

          if (!is.na(measured_flag) && measured_flag == "< LOD") {
            if (is.na(lod_value) || lod_value == "") {
              message <- paste(
                "Row",
                i,
                "requires LOD_VALUE when flag is '< LOD'"
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    # Rule 8: Check that all protocol fields are filled
    # CHANGED: Reference userData instead of moduleState
    iv$add_rule("measurement_table_validation", function(value) {
      if (nrow(session$userData$reactiveValues$measurementsData) > 0) {
        protocol_fields <- c(
          "SAMPLING_PROTOCOL",
          "FRACTIONATION_PROTOCOL",
          "EXTRACTION_PROTOCOL",
          "ANALYTICAL_PROTOCOL"
        )

        for (i in 1:nrow(session$userData$reactiveValues$measurementsData)) {
          for (field in protocol_fields) {
            field_value <- session$userData$reactiveValues$measurementsData[
              i,
              field
            ]
            if (is.na(field_value) || field_value == "") {
              message <- paste(
                "Row",
                i,
                "is missing required protocol field:",
                field
              )
              moduleState$validation_message <<- message
              return(message)
            }
          }
        }
      }
    })

    iv$enable()

    # 2. Helper functions ----

    ## Get validation status for each module ----
    # CHANGED: Check validation flags instead of nrow
    get_modules_status <- function() {
      modules <- list(
        Campaign = list(
          data = session$userData$reactiveValues$campaignData,
          valid = session$userData$reactiveValues$campaignDataValid
        ),
        References = list(
          data = session$userData$reactiveValues$referenceData,
          valid = session$userData$reactiveValues$referenceDataValid
        ),
        Sites = list(
          data = session$userData$reactiveValues$sitesData,
          valid = session$userData$reactiveValues$sitesDataValid
        ),
        Parameters = list(
          data = session$userData$reactiveValues$parametersData,
          valid = session$userData$reactiveValues$parametersDataValid
        ),
        Compartments = list(
          data = session$userData$reactiveValues$compartmentsData,
          valid = session$userData$reactiveValues$compartmentsDataValid
        ),
        Methods = list(
          data = session$userData$reactiveValues$methodsData,
          valid = session$userData$reactiveValues$methodsDataValid
        ),
        Samples = list(
          data = session$userData$reactiveValues$samplesData,
          valid = session$userData$reactiveValues$samplesDataValid
        ),
        Biota = list(
          data = session$userData$reactiveValues$biotaData,
          valid = session$userData$reactiveValues$biotaDataValid
        )
      )

      status_list <- lapply(names(modules), function(name) {
        data <- modules[[name]]$data
        valid <- modules[[name]]$valid %||% FALSE
        nrow <- nrow(data) %||% 0

        # CHANGED: Use validation flag as primary check
        if (
          name == "Biota" &&
            !("Biota" %in%
              session$userData$reactiveValues$samplesData$ENVIRON_COMPARTMENT)
        ) {
          # No biota samples detected - auto-validate
          message <- "No biota samples detected - Auto-validated"
          ready <- TRUE
        } else if (
          name == "Biota" &&
            "Biota" %in%
              session$userData$reactiveValues$samplesData$ENVIRON_COMPARTMENT &&
            !valid
        ) {
          # Biota samples exist but not validated
          message <- "Biota samples detected but no biota data found - Attention required"
          ready <- FALSE
        } else if (valid) {
          # Module is validated
          message <- "Validated"
          ready <- TRUE
        } else {
          # Module not validated
          message <- "Attention required"
          ready <- FALSE
        }

        list(module = name, message = message, ready = ready, count = nrow)
      })

      return(status_list)
    }

    ## Create sample-parameter combinations for measurement entry ----
    # ! FORMAT-BASED
    create_measurement_combinations <- function() {
      # Get all validated data and abbreviate
      samplesDataWithBiota <- session$userData$reactiveValues$samplesDataWithBiota
      samplesData <- session$userData$reactiveValues$samplesData
      parameters_data <- session$userData$reactiveValues$parametersData
      campaign_data <- session$userData$reactiveValues$campaignData
      reference_data <- session$userData$reactiveValues$referenceData

      # check to see if either samplesData or samplesDataWithBiota actually exist
      samples_data <- if (nrow(samplesDataWithBiota) > 0) {
        samplesDataWithBiota
      } else if (nrow(samplesData)) {
        samplesData
      } else {
        initialise_measurements_tibble()
      }

      # and check to see if parameters_data is an empty tibble
      if (nrow(samples_data) < 1 || nrow(parameters_data) < 1) {
        return(initialise_measurements_tibble())
      }

      # TODO: This is an awful fix but will work for now
      samplesData$PARAMETER_NAME <- "Copper"

      # get available methods
      available_methods <- session$userData$reactiveValues$methodsData
      # make each first available method the default for new combos
      first_sampling <- available_methods |>
        filter(PROTOCOL_CATEGORY == "Sampling Protocol") |>
        pull(PROTOCOL_ID)

      first_extraction <- available_methods |>
        filter(PROTOCOL_CATEGORY == "Extraction Protocol") |>
        pull(PROTOCOL_ID)

      first_fractionation <- available_methods |>
        filter(PROTOCOL_CATEGORY == "Fractionation Protocol") |>
        pull(PROTOCOL_ID)

      first_analytical <- available_methods |>
        filter(PROTOCOL_CATEGORY == "Analytical Protocol") |>
        pull(PROTOCOL_ID)

      # rejoin extended parameter data to samples
      combinations <- left_join(
        samples_data |>
          select(
            SAMPLE_ID,
            SITE_CODE,
            PARAMETER_NAME,
            SAMPLING_DATE,
            ENVIRON_COMPARTMENT,
            ENVIRON_COMPARTMENT_SUB,
            SUBSAMPLE
          ),
        parameters_data |>
          select(PARAMETER_NAME, PARAMETER_TYPE, MEASURED_TYPE) |>
          # we need to be a little bit careful to avoid including parameters that the user hasn't specifically
          # added in mod_samples
          # apparently under some circumstances this can return multiple PARAMETER_NAMES
          filter(PARAMETER_NAME %in% samplesData$PARAMETER_NAME |> unique()),
        by = "PARAMETER_NAME"
      ) |>
        mutate(
          # make sure SUBSAMPLE is always a string
          SUBSAMPLE = as.character(SUBSAMPLE)
        )

      # Fixme: if the user makes it all the way to samples without
      # entering a username, our attempt to get a reference id will
      # fail. But this is a very unlikely chain of events, so we can
      # probably just slap a temporary patch on it.
      reference_id <- if (nrow(reference_data) == 0) {
        "UnknownReference"
      } else {
        reference_data$REFERENCE_ID
      }

      campaign <- if (nrow(campaign_data) == 0) {
        "Unknown Campaign"
      } else {
        campaign_data$CAMPAIGN_NAME_SHORT
      }

      # Add campaign and reference info
      combinations <- combinations |>
        mutate(
          # Add measurement fields with empty defaults
          MEASURED_FLAG = "",
          MEASURED_VALUE = NA,
          UNCERTAINTY_TYPE = NA,
          UNCERTAINTY_UPPER = NA,
          UNCERTAINTY_LOWER = NA,
          MEASURED_N = NA,
          MEASURED_UNIT = "",
          LOQ_VALUE = NA,
          LOQ_UNIT = "",
          LOD_VALUE = NA,
          LOD_UNIT = "",

          # Add method info
          SAMPLING_PROTOCOL = first_sampling[1],
          FRACTIONATION_PROTOCOL = first_extraction[1],
          EXTRACTION_PROTOCOL = first_extraction[1],
          ANALYTICAL_PROTOCOL = first_analytical[1],

          REFERENCE_ID = reference_id,
          CAMPAIGN_NAME_SHORT = campaign,
        ) |>
        relocate(SAMPLE_ID, ENVIRON_COMPARTMENT, .after = REFERENCE_ID)

      return(combinations)
    }

    # 3. Observers and Reactives ----

    observe({
      # set validation variable true when ready
      if (isTRUE(iv$is_valid())) {
        session$userData$reactiveValues$measurementsDataValid <- TRUE
      } else {
        session$userData$reactiveValues$measurementsDataValid <- FALSE
      }
    }) |>
      bindEvent(
        iv$is_valid(),
        session$userData$reactiveValues$saveExtractionSuccessful
      )

    # Observer: show llm UI only if relevant ----
    observe({
      if (session$userData$reactiveValues$llmExtractionComplete) {
        show(id = "validation-llm-reporter", asis = TRUE)
      } else {
        hide(id = "validation-llm-reporter", asis = TRUE)
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$llmExtractionComplete,
        ignoreInit = TRUE
      )

    ## observe: Check upstream modules validation status continuously ----
    # upstream: all session$userData$reactiveValues validation flags
    # downstream: moduleState$data_entry_ready, session$userData$reactiveValues$measurementsData
    observe({
      # req(isFALSE(session$userData$reactiveValues$saveExtractionSuccessful)) # need to be a little careful here, as if we upload measurments data
      # the observer will create combinations and upload existing data, resulting in n new entries each time.
      if (modulesStatus()) {
        moduleState$data_entry_ready <- TRUE

        # CHANGED: Create measurement combinations and store in userData
        # Use add_row() rather than recreate from scratch to preserve entered data
        new_combinations <- session$userData$reactiveValues$measurementsData |>
          mutate(SUBSAMPLE = as.character(SUBSAMPLE)) |>
          add_row(create_measurement_combinations()) |>
          distinct(.keep_all = FALSE) # TODO: All very clever, but doesn't catch imported rows that have entered data (see req above)

        session$userData$reactiveValues$measurementsData <- new_combinations

        # TODO: This also fires way too often, when modulesStatus() definitely shouldn't be returning true. Why?
        print_dev(glue(
          "mod_data: All modules validated, created {nrow(session$userData$reactiveValues$measurementsData)} measurement combinations"
        ))
      } else {
        moduleState$data_entry_ready <- FALSE
        session$userData$reactiveValues$measurementsData <- initialise_measurements_tibble()

        print_dev("mod_data: Some modules pending, data entry disabled")
      }
    }) |>
      bindEvent(
        modulesStatus(),
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    ## observe: receive data from session$userData$reactiveValues$measurementsData (import) ----
    ## Data is already in userData, just log the event
    observe({
      print_dev("Loaded saved data into measurements userData.")
    }) |>
      bindEvent(
        session$userData$reactiveValues$saveExtractionComplete,
        session$userData$reactiveValues$saveExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    ## observe: Handle table changes ----
    # upstream: ainput$save_table_data
    # downstream: session$userData$reactiveValues$measurementsData
    observe({
      if (
        !is.null(input$measurement_table) &&
          moduleState$data_entry_ready &&
          nrow(session$userData$reactiveValues$measurementsData) > 0
      ) {
        updated_data <- hot_to_r(input$measurement_table)

        # CHANGED: Update userData directly
        session$userData$reactiveValues$measurementsData <- updated_data

        # Merge in campaign data
        campaign_data <- isolate(session$userData$reactiveValues$campaignData)
        for (col in names(campaign_data)) {
          if (
            !col %in% names(session$userData$reactiveValues$measurementsData)
          ) {
            session$userData$reactiveValues$measurementsData[[
              col
            ]] <- campaign_data[[col]]
          }
        }
      }
    }) |>
      bindEvent(input$save_table_data)

    ## observe: Update method dropdown options whenever methods change ----
    # Define some methods for the UI to find so it doesn't crash
    sampling_methods <- analytical_methods <- extraction_methods <- fractionation_methods <- c(
      ""
    )

    observe({
      # Get available methods from mod_methods
      available_methods <- session$userData$reactiveValues$methodsData

      if (nrow(available_methods) > 0) {
        analytical_methods <- available_methods |>
          filter(PROTOCOL_CATEGORY == "Analytical Protocol") |>
          pull(PROTOCOL_ID)

        if (length(analytical_methods) == 0) {
          analytical_methods <<- c("", "")
        } else if (length(analytical_methods) == 1) {
          analytical_methods <<- c(analytical_methods)
        }

        sampling_methods <- available_methods |>
          filter(PROTOCOL_CATEGORY == "Sampling Protocol") |>
          pull(PROTOCOL_ID)

        if (length(sampling_methods) == 0) {
          sampling_methods <<- c("", "")
        } else if (length(sampling_methods) == 1) {
          sampling_methods <<- c(sampling_methods)
        }

        extraction_methods <- available_methods |>
          filter(PROTOCOL_CATEGORY == "Extraction Protocol") |>
          pull(PROTOCOL_ID)

        if (length(extraction_methods) == 0) {
          extraction_methods <<- c("", "")
        } else if (length(extraction_methods) == 1) {
          extraction_methods <<- c(extraction_methods)
        }

        fractionation_methods <- available_methods |>
          filter(PROTOCOL_CATEGORY == "Fractionation Protocol") |>
          pull(PROTOCOL_ID)

        if (length(fractionation_methods) == 0) {
          fractionation_methods <<- c("", "")
        } else if (length(fractionation_methods) == 1) {
          fractionation_methods <<- c(fractionation_methods)
        }
      } else {
        analytical_methods <<- c("", "")
        sampling_methods <<- c("", "")
        extraction_methods <<- c("", "")
        fractionation_methods <<- c("", "")
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$methodsData,
        ignoreInit = TRUE
      )

    ## observe: Navigate to Campaign when go_to_campaign clicked ----
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "01-campaign"
      )
    }) |>
      bindEvent(input$go_to_campaign)

    ## observe: Navigate to References when go_to_references clicked ----
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "02-references"
      )
    }) |>
      bindEvent(input$go_to_references)

    ## observe: Navigate to Sites when go_to_sites clicked ----
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "03-sites"
      )
    }) |>
      bindEvent(input$go_to_sites)

    ## observe: Navigate to Parameters when go_to_parameters clicked ----
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "04-parameters"
      )
    }) |>
      bindEvent(input$go_to_parameters)

    ## observe: Navigate to Compartments when go_to_compartments clicked ----
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "05-compartments"
      )
    }) |>
      bindEvent(input$go_to_compartments)

    ## observe: Navigate to Methods when go_to_methods clicked ----
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "06-methods"
      )
    }) |>
      bindEvent(input$go_to_methods)

    ## observe: Navigate to Samples when go_to_samples clicked ----
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "07-samples"
      )
    }) |>
      bindEvent(input$go_to_samples)

    ## observe: Navigate to Biota when go_to_biota clicked ----
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "08-biota"
      )
    }) |>
      bindEvent(input$go_to_biota)

    # 4. Outputs ----

    ## output: validation_overview ----
    # upstream: session userData validation flags
    # downstream: UI validation status display
    output$validation_overview <- renderUI({
      status_list <- get_modules_status()

      # Create all module elements ----
      module_elements <- lapply(1:8, function(i) {
        item <- status_list[[i]]
        valid <- item$ready
        icon <- if (valid) "clipboard2-check" else "exclamation-triangle"
        class <- if (valid) {
          "validation-status validation-complete"
        } else {
          "validation-status validation-warning"
        }
        button_id <- paste0("go_to_", tolower(item$module))
        button_id <- NS(id, button_id)

        list(
          # Column 1: Module name
          span(
            bs_icon("table"),
            strong(item$module, ":"),
            paste(item$count, "row(s)"),
            style = "align-self: center;"
          ),
          # Column 2: Validation message
          div(
            class = class,
            style = "padding: 8px; border-radius: 4px; margin: 0px;",
            bs_icon(icon),
            " ",
            item$message
          ),
          # Column 3: Button
          input_task_button(
            id = button_id,
            label = HTML(paste(
              "Edit",
              bs_icon("pencil-square")
            )),
            class = "btn-sm",
            style = "align-self: center;"
          )
        )
      })

      # Flatten the list for layout_column_wrap
      all_elements <- do.call(c, module_elements)

      # Return complete UI ----
      div(
        layout_column_wrap(
          width = NULL,
          fill = FALSE,
          fillable = FALSE,
          style = css(
            grid_template_columns = "1fr 8fr 1fr",
            align_items = "center",
            margin = "-10px 0"
          ),
          !!!all_elements,
          gap = "0.5rem"
        )
      )
    })

    ## output: data_validation_reporter ----
    # upstream: iv validation state, moduleState$validation_message
    # downstream: UI validation status display
    output$data_validation_reporter <- renderUI({
      # Trigger reactivity on validation state changes
      iv$is_valid()

      if (iv$is_valid()) {
        div(
          bs_icon("clipboard2-check"),
          paste(
            "All measurement data validated successfully.",
            nrow(session$userData$reactiveValues$measurementsData),
            "measurement(s) ready."
          ),
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          if (nzchar(moduleState$validation_message)) {
            moduleState$validation_message
          } else {
            "Complete all required measurement fields to proceed."
          },
          class = "validation-status validation-warning"
        )
      }
    })

    ## output: measurement_table ----
    # upstream: session$userData$reactiveValues$measurementsData, moduleState$data_entry_ready
    # downstream: UI table display
    output$measurement_table <- renderRHandsontable({
      # CHANGED: Reference userData instead of moduleState
      if (
        !moduleState$data_entry_ready ||
          nrow(session$userData$reactiveValues$measurementsData) == 0
      ) {
        rhandsontable(
          initialise_measurements_tibble(),
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all")
      } else {
        rhandsontable(
          session$userData$reactiveValues$measurementsData,
          selectCallback = TRUE,
          width = NULL
        ) |>
          hot_table(overflow = "visible", stretchH = "all") |>
          hot_col(
            c(
              "SITE_CODE",
              "PARAMETER_NAME",
              "SAMPLING_DATE",
              "ENVIRON_COMPARTMENT_SUB",
              "SUBSAMPLE"
            ),
            readOnly = TRUE
          ) |>
          hot_col(
            "MEASURED_FLAG",
            type = "dropdown",
            source = measured_flags_vocabulary(),
            strict = TRUE
          ) |>
          hot_col(
            "MEASURED_VALUE",
            type = "numeric",
            format = "0.0000"
          ) |>
          hot_col(
            "UNCERTAINTY_TYPE",
            type = "dropdown",
            source = uncertainty_types_vocabulary(),
            strict = TRUE
          ) |>
          hot_col(
            c(
              "UNCERTAINTY_UPPER",
              "UNCERTAINTY_LOWER",
              "LOQ_VALUE",
              "LOD_VALUE"
            ),
            type = "numeric",
            format = "0.0000"
          ) |>
          hot_col(
            "MEASURED_N",
            type = "numeric",
            format = "0"
          ) |>
          hot_col(
            c("MEASURED_UNIT", "LOQ_UNIT", "LOD_UNIT"),
            type = "dropdown",
            source = measured_units,
            strict = TRUE
          ) |>
          hot_col(
            "SAMPLING_PROTOCOL",
            type = "dropdown",
            source = sampling_methods,
            strict = TRUE
          ) |>
          hot_col(
            "FRACTIONATION_PROTOCOL",
            type = "dropdown",
            source = fractionation_methods,
            strict = TRUE
          ) |>
          hot_col(
            "EXTRACTION_PROTOCOL",
            type = "dropdown",
            source = extraction_methods,
            strict = TRUE
          ) |>
          hot_col(
            "ANALYTICAL_PROTOCOL",
            type = "dropdown",
            source = analytical_methods,
            strict = TRUE
          ) |>
          hot_col(
            c("SAMPLE_ID", "REFERENCE_ID", "ENVIRON_COMPARTMENT"),
            readOnly = TRUE
          ) |>
          hot_cols(
            manualColumnResize = TRUE,
            columnSorting = TRUE
          )
      }
    })

    ## output: complete_data_preview ----
    # upstream: session$userData$reactiveValues$measurementsData
    # downstream: UI data display
    output$complete_data_preview <- renderText({
      # CHANGED: Reference userData instead of moduleState
      if (nrow(session$userData$reactiveValues$measurementsData) > 0) {
        # Show overview of complete dataset
        dataset_info <- paste0(
          "Complete Dataset Overview:\n",
          "Total Records: ",
          nrow(session$userData$reactiveValues$measurementsData),
          "\n",
          "Total Columns: ",
          ncol(session$userData$reactiveValues$measurementsData),
          "\n",
          "Sample-Parameter Combinations: ",
          length(unique(paste(
            session$userData$reactiveValues$measurementsData$SAMPLE_ID,
            session$userData$reactiveValues$measurementsData$PARAMETER_NAME
          ))),
          "\n",
          "Unique Sites: ",
          length(unique(
            session$userData$reactiveValues$measurementsData$SITE_CODE
          )),
          "\n",
          "Unique Parameters: ",
          length(unique(
            session$userData$reactiveValues$measurementsData$PARAMETER_NAME
          )),
          "\n\n",
          "First 3 records:\n",
          paste(
            capture.output(head(
              session$userData$reactiveValues$measurementsData,
              3
            )),
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
