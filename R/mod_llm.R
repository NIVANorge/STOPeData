# LLM Extraction Module ----
# A Shiny module for PDF upload and automated data extraction using Claude

#' LLM Extraction UI Function ----
#'
#' @description A shiny Module for PDF upload and Claude-powered data extraction.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fileInput textInput actionButton
#' @importFrom bslib card card_header card_body accordion accordion_panel tooltip layout_column_wrap input_task_button
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs disabled
mod_llm_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    useShinyjs(),

    # Main extraction card ----
    card(
      card_header("Automated Data Extraction"),
      card_body(
        ## Info accordion ----
        accordion(
          id = ns("info_accordion"),
          accordion_panel(
            title = "LLM Extraction Information",
            icon = bs_icon("info-circle"),
            "This module uses Claude AI to automatically extract environmental exposure data from uploaded PDFs. Upload a research paper or report, provide your Anthropic API key, and the system will attempt to populate all form fields automatically. Review and correct the extracted data in subsequent modules before validation."
          )
        ),

        ## Upload and API section ----
        div(
          style = "margin: 20px 0;",
          h5("Upload & Configuration"),

          layout_column_wrap(
            width = "400px",
            fill = FALSE,
            fillable = FALSE,

            ### PDF upload ----
            fileInput(
              inputId = ns("pdf_file"),
              label = tooltip(
                list("Upload PDF", bs_icon("info-circle-fill")),
                "Upload a research paper or report containing environmental exposure data"
              ),
              accept = ".pdf",
              width = "100%"
            ),

            ### API key input ----
            textInput(
              inputId = ns("api_key"),
              label = tooltip(
                list("Anthropic API Key", bs_icon("info-circle-fill")),
                "Your Anthropic API key for Claude access. Set ANTHROPIC_API_KEY environment variable to avoid entering this each time."
              ),
              placeholder = "sk-ant-...",
              value = Sys.getenv("ANTHROPIC_API_KEY") %||% "",
              width = "100%"
            )
          )
        ),

        ## Prompt and Schema Configuration ----
        accordion(
          id = ns("config_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Advanced Configuration",
            icon = bs_icon("gear"),
            div(
              h6("System Prompt"),
              textAreaInput(
                inputId = ns("system_prompt"),
                label = "Extraction Instructions",
                value = create_extraction_prompt(),
                rows = 8,
                width = "100%"
              ),

              h6("Extraction Schema"),
              textAreaInput(
                inputId = ns("extraction_schema_display"),
                label = "Schema Definition",
                value = get_schema_display(),
                rows = 12,
                width = "100%"
              ),

              div(
                style = "margin-top: 10px;",
                actionButton(
                  ns("reset_defaults"),
                  "Reset to Defaults",
                  class = "btn-secondary btn-sm"
                )
              )
            )
          )
        ),

        ## Extract button ----
        div(
          style = "margin-top: 15px;",
          input_task_button(
            id = ns("extract_data"),
            label = "Extract Data from PDF",
            icon = icon("magic"),
            class = "btn-success",
            width = "200px"
          ) |>
            disabled()
        ),
        input_task_button(
          id = ns("fake_data"),
          label = "Use fake data from earlier and save money"
        ),

        ## Status and results ----
        div(
          style = "margin-top: 20px;",
          uiOutput(ns("extraction_status"))
        ),

        ## Extraction results accordion ----
        accordion(
          id = ns("results_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Extraction Results",
            icon = bs_icon("cpu"),
            div(
              h6("Raw Extraction Output"),
              verbatimTextOutput(ns("extraction_results")),

              br(),

              ### Action buttons for extracted data ----
              div(
                style = "margin-top: 15px;",
                input_task_button(
                  id = ns("populate_forms"),
                  label = "Populate Forms with Extracted Data",
                  icon = icon("download"),
                  class = "btn-primary",
                  width = "250px"
                ) |>
                  disabled(),

                input_task_button(
                  id = ns("clear_extraction"),
                  label = "Clear Extraction",
                  icon = icon("trash"),
                  class = "btn-danger",
                  width = "150px"
                ) |>
                  disabled()
              )
            )
          )
        ),

        ## TODO items accordion ----
        accordion(
          id = ns("todo_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Future Directions",
            icon = bs_icon("list-check"),
            div(
              tags$ul(
                tags$li("Add confidence scoring for extracted fields"),
                tags$li("Progressive field-by-field acceptance/rejection"),
                tags$li(
                  "Support for different document types (reports, theses, etc.)"
                ),
              )
            )
          )
        )
      )
    )
  )
}

#' LLM Extraction Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI showNotification updateTextAreaInput
#' @importFrom shinyjs enable disable
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @importFrom ellmer chat_anthropic params content_pdf_file type_object type_string type_integer type_number type_array
mod_llm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      extraction_complete = FALSE,
      extraction_successful = FALSE,
      raw_extraction = NULL,
      structured_data = NULL,
      error_message = NULL
    )

    # 2. Observers and Reactives ----

    ## observe: Reset configuration to defaults ----
    observe({
      updateTextAreaInput(
        session,
        "system_prompt",
        value = create_extraction_prompt()
      )
      updateTextAreaInput(
        session,
        "extraction_schema_display",
        value = get_schema_display()
      )
      showNotification("Configuration reset to defaults", type = "message")
    }) |>
      bindEvent(input$reset_defaults)

    ## observe: Enable extract button when PDF and API key available ----
    # upstream: input$pdf_file, input$api_key
    # downstream: extract_data button state
    observe({
      if (
        !is.null(input$pdf_file) &&
          isTruthy(input$api_key) &&
          nchar(input$api_key) > 10
      ) {
        enable("extract_data")
      } else {
        disable("extract_data")
      }
    })

    ## observe: PDF data extraction ----
    # upstream: user clicks input$extract_data
    # downstream: moduleState$*, session$userData$reactiveValues$*DataLLM
    observe({
      req(input$pdf_file, input$api_key)

      # Show processing status
      showNotification("Starting PDF extraction...", type = "message")

      # Validate API key format (basic check)
      if (!grepl("^sk-ant-", input$api_key)) {
        showNotification(
          "API key should start with 'sk-ant-'. Please check your key.",
          type = "warning"
        )
        return()
      }

      tryCatch(
        {
          # Set up Claude chat with API key
          Sys.setenv(ANTHROPIC_API_KEY = input$api_key)

          # Test API connectivity first
          test_chat <- NULL
          tryCatch(
            {
              test_chat <- chat_anthropic(
                model = "claude-sonnet-4-20250514",
                params = params(max_tokens = 50)
              )
              test_response <- test_chat$chat(
                "Hello, please respond with 'API connection successful'"
              )
              print_dev("LLM API connection opened.")
            },
            error = function(e) {
              showNotification(
                paste("LLM API connection failed:", e$message),
                type = "error"
              )
              return()
            }
          )

          if (is.null(test_chat)) return()

          # Create chat instance for extraction
          chat <- chat_anthropic(
            model = "claude-sonnet-4-20250514",
            params = params(max_tokens = 4000)
          )

          # Define structured data types for extraction
          extraction_schema <- create_extraction_schema()

          # Create PDF content object
          pdf_content <- content_pdf_file(input$pdf_file$datapath)

          # Use custom prompt if provided, otherwise default
          system_prompt <- if (isTruthy(input$system_prompt)) {
            input$system_prompt
          } else {
            create_extraction_prompt()
          }

          showNotification("Extracting data from PDF...", type = "message")

          # Extract data using structured chat
          result <- chat$chat_structured(
            system_prompt,
            pdf_content,
            type = extraction_schema
          )

          # Store results
          moduleState$extraction_complete <- TRUE
          moduleState$extraction_successful <- TRUE
          moduleState$structured_data <- result
          moduleState$raw_extraction <- result
          moduleState$error_message <- NULL

          # Store in session data with LLM suffix
          store_llm_data_in_session(session, result)

          showNotification(
            "PDF extraction completed successfully!",
            type = "default"
          )

          # Enable form population button
          enable("populate_forms")
          enable("clear_extraction")
        },
        error = function(e) {
          moduleState$extraction_complete <- TRUE
          moduleState$extraction_successful <- FALSE
          moduleState$error_message <- e$message
          moduleState$structured_data <- NULL

          showNotification(
            paste("Extraction failed:", e$message),
            type = "error"
          )

          print_dev(glue("LLM extraction error: {e$message}"))
        }
      )
    }) |>
      bindEvent(input$extract_data)

    ## observe: Populate forms with extracted data ----
    # upstream: user clicks input$populate_forms
    # downstream: trigger form population in other modules
    observe({
      req(moduleState$structured_data)

      tryCatch(
        {
          # Populate form fields directly
          if (!is.null(moduleState$structured_data$campaign)) {
            populate_campaign_from_llm(
              session,
              moduleState$structured_data$campaign
            )
          }

          if (!is.null(moduleState$structured_data$references)) {
            populate_references_from_llm(
              session,
              moduleState$structured_data$references
            )
          }

          # Create structured data for table-based modules and store in session
          if (!is.null(moduleState$structured_data$sites)) {
            print(paste("sites_data:", moduleState$structured_data$sites))
            sites_data <- create_sites_from_llm(
              moduleState$structured_data$sites
            )
            print(paste("sites_data:", sites_data))
            session$userData$reactiveValues$sitesDataLLM <- sites_data
          }

          if (!is.null(moduleState$structured_data$parameters)) {
            parameters_data <- create_parameters_from_llm(
              moduleState$structured_data$parameters
            )
            session$userData$reactiveValues$parametersDataLLM <- parameters_data
          }

          if (!is.null(moduleState$structured_data$compartments)) {
            compartments_data <- create_compartments_from_llm(
              moduleState$structured_data$compartments
            )
            session$userData$reactiveValues$compartmentsDataLLM <- compartments_data
          }

          # Set extraction status flags
          session$userData$reactiveValues$llmExtractionComplete <- TRUE
          session$userData$reactiveValues$llmExtractionSuccessful <- TRUE

          # showNotification(
          #   "Forms populated with extracted data! Review and correct in each module.",
          #   type = "default"
          # )

          print_dev("All forms populated from LLM extraction")
        },
        error = function(e) {
          showNotification(
            paste("Error populating forms:", e$message),
            type = "error"
          )
          print_dev(glue("Form population error: {e$message}"))
        }
      )
    }) |>
      bindEvent(input$populate_forms)

    ## observe: fake data! ----
    observe({
      # Extract data using structured chat
      result <- readRDS(
        file = "inst/data/clean/example_chaudry_extract_list.rds"
      )

      # Store results
      moduleState$extraction_complete <- TRUE
      moduleState$extraction_successful <- TRUE
      moduleState$structured_data <- result
      moduleState$raw_extraction <- result
      moduleState$error_message <- NULL

      # Store in session data with LLM suffix
      store_llm_data_in_session(session, result)

      # Enable form population button
      enable("populate_forms")
      enable("clear_extraction")
    }) |>
      bindEvent(input$fake_data)

    ## observe: Clear extraction ----
    # upstream: user clicks input$clear_extraction
    # downstream: reset module state and session data
    observe({
      # Clear module state
      moduleState$extraction_complete <- FALSE
      moduleState$extraction_successful <- FALSE
      moduleState$raw_extraction <- NULL
      moduleState$structured_data <- NULL
      moduleState$error_message <- NULL

      # Clear session LLM data and status flags
      clear_llm_data_from_session(session)
      session$userData$reactiveValues$llmExtractionComplete <- FALSE
      session$userData$reactiveValues$llmExtractionSuccessful <- FALSE

      # Disable buttons
      disable("populate_forms")
      disable("clear_extraction")

      showNotification("Extraction cleared", type = "message")
    }) |>
      bindEvent(input$clear_extraction)

    # 3. Outputs ----

    ## output: extraction_status ----
    # upstream: moduleState
    # downstream: UI status display
    output$extraction_status <- renderUI({
      if (!moduleState$extraction_complete) {
        div(
          bs_icon("info-circle"),
          "Upload a PDF and provide your API key to begin extraction.",
          class = "validation-status validation-info"
        )
      } else if (moduleState$extraction_successful) {
        div(
          bs_icon("check-circle"),
          "PDF extraction completed successfully. Review results below.",
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          paste("Extraction failed:", moduleState$error_message),
          class = "validation-status validation-warning"
        )
      }
    })

    ## output: extraction_results ----
    # upstream: moduleState$raw_extraction
    # downstream: UI results display
    output$extraction_results <- renderText({
      if (!is.null(moduleState$raw_extraction)) {
        # Format the extraction results for display
        if (is.list(moduleState$raw_extraction)) {
          # Pretty print the structured data
          capture.output(str(moduleState$raw_extraction, max.level = 6)) |>
            paste(collapse = "\n")
        } else {
          as.character(moduleState$raw_extraction)
        }
      } else if (!is.null(moduleState$error_message)) {
        paste("Extraction Error:", moduleState$error_message)
      } else {
        "No extraction performed yet."
      }
    })
  })
}

# 4. Helper Functions ----

#' Create extraction schema with correct ellmer syntax
#' @noRd
create_extraction_schema <- function() {
  type_object(
    .description = "Extract environmental exposure study data from this document",

    # Campaign data
    campaign = type_object(
      .description = "Basic study/campaign information",
      campaign_name = type_string(
        description = "Short identifier for the study/campaign (max 100 chars)",
        required = FALSE
      ),
      campaign_start_date = type_string(
        description = "Study start date in YYYY-MM-DD format",
        required = FALSE
      ),
      campaign_end_date = type_string(
        description = "Study end date in YYYY-MM-DD format",
        required = FALSE
      ),
      organisation = type_string(
        description = "Organization that conducted the study (max 50 chars)",
        required = FALSE
      ),
      campaign_comment = type_string(
        description = "Additional study details or notes (max 1000 chars)",
        required = FALSE
      )
    ),

    # Reference data
    references = type_object(
      .description = "Bibliographic information about this document",
      author = type_string(
        description = "Authors in format: Last1, First1; Last2, First2 (max 1000 chars)",
        required = FALSE
      ),
      title = type_string(
        description = "Document title (max 1000 chars)",
        required = FALSE
      ),
      year = type_integer(
        description = "Publication year (1800-2026)",
        required = FALSE
      ),
      periodical_journal = type_string(
        description = "Journal name for articles",
        required = FALSE
      ),
      volume = type_integer(
        description = "Journal volume number",
        required = FALSE
      ),
      issue = type_integer(
        description = "Journal issue number",
        required = FALSE
      ),
      publisher = type_string(
        description = "Publisher name",
        required = FALSE
      ),
      doi = type_string(
        description = "Digital Object Identifier",
        required = FALSE
      )
    ),

    # Sites data - ARRAY OF OBJECTS
    sites = type_array(
      type_object(
        .description = "Information about a sampling site",
        site_code = type_string(
          description = "Short site identifier/code",
          required = FALSE
        ),
        site_name = type_string(
          description = "Descriptive site name",
          required = FALSE
        ),
        latitude = type_number(
          description = "Latitude in decimal degrees (-90 to 90)",
          required = FALSE
        ),
        longitude = type_number(
          description = "Longitude in decimal degrees (-180 to 180)",
          required = FALSE
        ),
        country = type_string(
          description = "Country where site is located",
          required = FALSE
        ),
        site_geographic_feature = type_string(
          description = "Geographic feature type (river, lake, ocean, etc.)",
          required = FALSE
        )
      )
    ),

    # Parameters data - ARRAY OF OBJECTS
    parameters = type_array(
      type_object(
        .description = "A measured parameter/stressor",
        parameter_name = type_string(
          description = "Name of the parameter/chemical/stressor measured",
          required = FALSE
        ),
        parameter_type = type_string(
          description = "Type: Stressor, Quality parameter, Normalization, or Background",
          required = FALSE
        ),
        cas_rn = type_string(
          description = "CAS Registry Number if chemical",
          required = FALSE
        )
      )
    ),

    # Compartments data - ARRAY OF OBJECTS
    compartments = type_array(
      type_object(
        .description = "An environmental compartment sampled",
        environ_compartment = type_string(
          description = "Main compartment: Aquatic, Atmospheric, Terrestrial, or Biota",
          required = FALSE
        ),
        environ_compartment_sub = type_string(
          description = "Sub-compartment (e.g., Freshwater, Soil A Horizon, etc.)",
          required = FALSE
        ),
        measured_category = type_string(
          description = "Measurement category: External, Internal, or Surface",
          required = FALSE
        )
      )
    )
  )
}

#' Get schema as formatted display for textarea
#' @noRd
get_schema_display <- function() {
  tryCatch(
    {
      # Create the actual schema and capture its structure
      schema <- create_extraction_schema()

      # Convert to a readable format showing the actual ellmer object structure
      schema_str <- capture.output({
        print(schema, width = 80)
      })

      # Join the output lines
      paste(schema_str, collapse = "\n")
    },
    error = function(e) {
      paste("Error displaying schema:", e$message)
    }
  )
}

#' Create extraction prompt with controlled vocabulary
#' @description Creates the system prompt for Claude extraction
#' @noRd
create_extraction_prompt <- function() {
  paste0(
    "You are an expert at extracting environmental exposure study data from scientific documents. ",
    "Extract the following information from the uploaded PDF, following these strict guidelines:\n\n",

    "CRITICAL RULES:\n",
    "- Only extract information that is explicitly stated in the document\n",
    "- Do NOT guess, infer, or make assumptions about missing data\n",
    "- Use 'null' for any field where information is not clearly provided\n",
    "- For dates, use YYYY-MM-DD format only\n",
    "- For years, only use values between 1800-2026\n",
    "- For coordinates, use decimal degrees only\n\n",

    "CONTROLLED VOCABULARY (use these exact terms when applicable):\n",
    "Parameter Types: Stressor, Quality parameter, Normalization, Background\n",
    "Compartments: Aquatic, Atmospheric, Terrestrial, Biota\n",
    "Sub-compartments: Freshwater, Marine/Salt Water, Brackish/Transitional Water, ",
    "Groundwater, Wastewater, Indoor Air, Outdoor Air, Soil A Horizon (Topsoil), ",
    "Soil O Horizon (Organic), Biota Terrestrial, Biota Aquatic\n",
    "Measurement Categories: External, Internal, Surface\n",
    "Geographic Features: River stream canal, Lake pond pool reservoir, ",
    "Ocean sea territorial waters, Coastal fjord, Estuary, Cropland, ",
    "Woodland forest, Grassland, Other\n\n",

    "Focus on extracting:\n",
    "1. Study metadata (dates, organization, campaign details)\n",
    "2. Bibliographic information (authors, title, journal, DOI)\n",
    "3. Sampling sites (locations, coordinates, site descriptions)\n",
    "4. Measured parameters (chemicals, stressors, quality parameters)\n",
    "5. Environmental compartments sampled\n\n",

    "Return structured data following the provided schema. Be conservative - ",
    "it's better to return null than to guess incorrectly."
  )
}

#' Store LLM extracted data in session reactiveValues
#' @param session Shiny session object
#' @param extracted_data Structured data from Claude
#' @noRd
store_llm_data_in_session <- function(session, extracted_data) {
  # Campaign data
  if (!is.null(extracted_data$campaign)) {
    session$userData$reactiveValues$campaignDataLLM <- extracted_data$campaign
    print_dev("Stored campaign data from LLM extraction")
  }

  # References data
  if (!is.null(extracted_data$references)) {
    session$userData$reactiveValues$referencesDataLLM <- extracted_data$references
    print_dev("Stored references data from LLM extraction")
  }

  # Sites data
  if (!is.null(extracted_data$sites) && length(extracted_data$sites) > 0) {
    session$userData$reactiveValues$sitesDataLLM <- extracted_data$sites
    print_dev(glue(
      "Stored {length(extracted_data$sites)} sites from LLM extraction"
    ))
  }

  # Parameters data
  if (
    !is.null(extracted_data$parameters) && length(extracted_data$parameters) > 0
  ) {
    session$userData$reactiveValues$parametersDataLLM <- extracted_data$parameters
    print_dev(glue(
      "Stored {length(extracted_data$parameters)} parameters from LLM extraction"
    ))
  }

  # Compartments data
  if (
    !is.null(extracted_data$compartments) &&
      length(extracted_data$compartments) > 0
  ) {
    session$userData$reactiveValues$compartmentsDataLLM <- extracted_data$compartments
    print_dev(glue(
      "Stored {length(extracted_data$compartments)} compartments from LLM extraction"
    ))
  }
}

#' Clear LLM data from session reactiveValues
#' @param session Shiny session object
#' @noRd
clear_llm_data_from_session <- function(session) {
  session$userData$reactiveValues$campaignDataLLM <- NULL
  session$userData$reactiveValues$referencesDataLLM <- NULL
  session$userData$reactiveValues$sitesDataLLM <- NULL
  session$userData$reactiveValues$parametersDataLLM <- NULL
  session$userData$reactiveValues$compartmentsDataLLM <- NULL

  print_dev("Cleared all LLM extracted data from session")
}

## To be copied in the UI ----
# mod_llm_ui("llm_1")

## To be copied in the server ----
# mod_llm_server("llm_1")
