# References Import Module ----
# A Shiny module for reference data entry with conditional validation based on reference type

#' References UI Function ----
#'
#' @description A shiny Module for reference data entry and validation with conditional fields.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput dateInput selectInput textAreaInput actionButton numericInput
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel tooltip
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom rcrossref cr_works
#' @importFrom httr GET content http_error
#' @importFrom xml2 read_xml xml_find_first xml_text
#' @import eDataDRF
#' @export
mod_references_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs ----
    useShinyjs(),

    # Main input card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_references.md"),

        ## Import tools section ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### DOI lookup ----
          div(
            textInput(
              inputId = ns("doi_lookup"),
              label = tooltip(
                list("DOI Lookup", bs_icon("info-circle-fill")),
                "Enter a DOI to automatically populate reference fields"
              ),
              placeholder = "Look up a paper on Crossref",
              width = "100%"
            ),
            actionButton(
              inputId = ns("lookup_doi"),
              icon = icon("search"),
              label = "Look up DOI on Crossref",
              class = "btn-info",
              width = "100%"
            )
          ),

          ### BibTeX import ----
          div(
            textAreaInput(
              inputId = ns("bibtex_import"),
              label = tooltip(
                list("BibTeX Import", bs_icon("info-circle-fill")),
                "Paste BibTeX entry here. Import will overwrite existing field values."
              ),
              placeholder = "Paste BibTeX entry here",
              rows = 3,
              width = "100%"
            ),
            actionButton(
              inputId = ns("import_bibtex"),
              icon = icon("file-code"),
              label = "Import BibTeX (overwrites existing data)",
              class = "btn-info",
              width = "100%"
            )
          )
        ),

        ## Reference type selector (always required) ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,
          selectInput(
            inputId = ns("REFERENCE_TYPE"),
            label = tooltip(
              list("Reference Type", bs_icon("info-circle-fill")),
              "Select the type of reference to add"
            ),
            choices = c(
              "Journal Article" = "journal",
              "Book" = "book",
              "Report" = "report",
              "Dataset/Database" = "dataset"
            ),
            selected = "journal",
            width = "300px"
          ),

          ## Data source selector (always required) ----
          selectInput(
            inputId = ns("DATA_SOURCE"),
            label = tooltip(
              list("Data Source", bs_icon("info-circle-fill")),
              "Primary: Data were collected as part of the work cited; Secondary: Data were gathered from other sources/literature."
            ),
            choices = c(
              "Primary" = "Primary",
              "Secondary/Review" = "Secondary, Review",
              "Other" = "Other"
            ),
            selected = "Primary",
            width = "300px"
          )
        ),

        ## Always required fields ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### REFERENCE_ID_DISPLAY - automatically generated ----
          textInput(
            inputId = ns("REFERENCE_ID_DISPLAY"),
            label = tooltip(
              list(
                "Reference ID (Auto-generated)",
                bs_icon("info-circle-fill")
              ),
              "Automatically generated reference identifier"
            ),
            value = "",
            width = "100%"
          ) |>
            disabled(),

          ### AUTHOR - Always required, 1000 char ----
          textAreaInput(
            inputId = ns("AUTHOR"),
            label = tooltip(
              list("Author(s)", bs_icon("info-circle-fill")),
              "Authors in format: Last1, First1; Last2, First2"
            ),
            placeholder = "Last1, First1; Last2, First2",
            rows = 2,
            width = "100%"
          ),

          ### TITLE - Always required, 1000 char ----
          textAreaInput(
            inputId = ns("TITLE"),
            label = tooltip(
              list("Title", bs_icon("info-circle-fill")),
              "Full title of the publication"
            ),
            placeholder = "Full publication title",
            rows = 2,
            width = "100%"
          ),

          ### YEAR - Always required, int ----
          numericInput(
            inputId = ns("YEAR"),
            label = tooltip(
              list("Year", bs_icon("info-circle-fill")),
              "Year of publication, last update, etc. as appropriate"
            ),
            value = as.numeric(format(Sys.Date(), "%Y")),
            min = 1800,
            max = as.numeric(format(Sys.Date(), "%Y")) + 5,
            step = 1,
            width = "100%"
          )
        ),

        ## Conditional fields layout ----
        layout_column_wrap(
          width = "300px",
          fill = FALSE,
          fillable = FALSE,

          ### ACCESS_DATE - Required for all ----
          dateInput(
            inputId = ns("ACCESS_DATE"),
            label = tooltip(
              list("Date Accessed", bs_icon("info-circle-fill")),
              "Date when you accessed or retrieved this reference"
            ),
            value = as.Date(NA),
            format = "yyyy-mm-dd",
            width = "100%"
          ) |>
            suppressWarnings(),

          ### ENTERED_BY - From mod_campaign if available ----
          textInput(
            inputId = ns("ENTERED_BY"),
            label = tooltip(
              list("Entered by", bs_icon("info-circle-fill")),
              "Your name or initials. Autofilled from first module if available."
            ),
            placeholder = "Your name or initials...",
            width = "100%"
          ),

          ### Journal-specific fields ----
          textInput(
            inputId = ns("PERIODICAL_JOURNAL"),
            label = tooltip(
              list("Journal Name", bs_icon("info-circle-fill")),
              "Name of journal or publication series"
            ),
            placeholder = "Journal name if relevant",
            width = "100%"
          ),

          numericInput(
            inputId = ns("VOLUME"),
            label = tooltip(
              list("Volume", bs_icon("info-circle-fill")),
              "Volume number of journal or publication series"
            ),
            value = NA,
            min = 1,
            step = 1,
            width = "100%"
          ),

          numericInput(
            inputId = ns("ISSUE"),
            label = tooltip(
              list("Issue", bs_icon("info-circle-fill")),
              "Issue number within volume if relevant"
            ),
            value = NA,
            min = 1,
            step = 1,
            width = "100%"
          ),

          textInput(
            inputId = ns("PUBLISHER"),
            label = tooltip(
              list("Publisher", bs_icon("info-circle-fill")),
              "Publishing organisation or company"
            ),
            placeholder = "Publishing organisation if relevant",
            width = "100%"
          ),

          ### Report-specific fields ----
          textInput(
            inputId = ns("INSTITUTION"),
            label = tooltip(
              list("Institution", bs_icon("info-circle-fill")),
              "Institution responsible for data"
            ),
            placeholder = "Institution responsible for data",
            width = "100%"
          ),

          ### Optional fields for all types ----
          textInput(
            inputId = ns("DOI"),
            label = tooltip(
              list("DOI", bs_icon("info-circle-fill")),
              "Digital Object Identifier for publication"
            ),
            placeholder = "Digital Object Identifier",
            width = "100%"
          ),

          textInput(
            inputId = ns("URL"),
            label = tooltip(
              list("URL", bs_icon("info-circle-fill")),
              "Web address where the reference/data can be accessed"
            ),
            placeholder = "Web address",
            width = "100%"
          ),

          textInput(
            inputId = ns("ISBN_ISSN"),
            label = tooltip(
              list("ISBN/ISSN", bs_icon("info-circle-fill")),
              "International Standard Book Number or International Standard Serial Number"
            ),
            placeholder = "ISBN for books or ISSN for journals if relevant",
            width = "100%"
          ),

          textInput(
            inputId = ns("EDITION"),
            label = tooltip(
              list("Edition", bs_icon("info-circle-fill")),
              "Edition number or description (e.g., 2nd, revised)"
            ),
            placeholder = "Edition number or description if relevant",
            width = "100%"
          ),

          textInput(
            inputId = ns("DOCUMENT_NUMBER"),
            label = tooltip(
              list("Document Number", bs_icon("info-circle-fill")),
              "Official document identification number"
            ),
            placeholder = "Document identification number if relevant",
            width = "100%"
          )
        ),

        ### REF_COMMENT - Full width text area ----
        textAreaInput(
          inputId = ns("REF_COMMENT"),
          label = tooltip(
            list("Reference Comment", bs_icon("info-circle-fill")),
            "Additional notes or comments about this reference"
          ),
          placeholder = "Please add any additional notes about the reference that may be relevant or useful to later use.",
          width = "100%",
          rows = 3
        ),

        ## Validation status and raw data ----
        uiOutput(ns("validation_reporter")),
        accordion(
          id = ns("data_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Click to view raw validated data",
            icon = bs_icon("code"),
            verbatimTextOutput(ns("validated_data_display"))
          )
        ),

        ## Action buttons ----
        actionButton(
          inputId = ns("clear"),
          label = "Clear All Fields",
          class = "btn-danger",
          width = "300px"
        )
      )
    )
  )
}

#' References Server Functions ----
#'
#' @noRd
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText updateTextInput updateDateInput updateNumericInput updateTextAreaInput updateSelectInput bindEvent
#' @importFrom shinyjs enable disable
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr add_row
#' @import eDataDRF
#' @export
mod_references_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## InputValidator$new: iv ----
    iv <- InputValidator$new()

    ### Always required fields ----
    iv$add_rule("REFERENCE_TYPE", sv_required())

    iv$add_rule("DATA_SOURCE", sv_required())

    iv$add_rule("AUTHOR", sv_required())
    iv$add_rule("AUTHOR", function(value) {
      if (isTruthy(value) && nchar(value) > 1000) {
        "Author(s) must be 1000 characters or less"
      }
    })

    iv$add_rule("TITLE", sv_required())
    iv$add_rule("TITLE", function(value) {
      if (isTruthy(value) && nchar(value) > 1000) {
        "Title must be 1000 characters or less"
      }
    })

    iv$add_rule("ENTERED_BY", sv_required())
    iv$add_rule("ENTERED_BY", function(value) {
      if (isTruthy(value) && nchar(value) > 100) {
        "Username must be 100 characters or less"
      }
    })

    iv$add_rule("YEAR", sv_required())
    iv$add_rule("YEAR", function(value) {
      if (
        isTruthy(value) &&
          (value < 1800 || value > as.numeric(format(Sys.Date(), "%Y")) + 5)
      ) {
        "Year must be between 1800 and 5 years in the future"
      }
    })

    ### Conditionally required fields ----
    # ACCESS_DATE - required for all reference types
    iv$add_rule("ACCESS_DATE", function(value) {
      if (!isTruthy(value)) {
        "Date Accessed is required for all reference types"
      }
    })

    # Journal-specific required fields
    iv$add_rule("PERIODICAL_JOURNAL", function(value) {
      if (
        isTruthy(input$REFERENCE_TYPE) &&
          input$REFERENCE_TYPE == "journal" &&
          !isTruthy(value)
      ) {
        "Journal Name is required for journal articles"
      }
    })

    # Book-specific required fields
    iv$add_rule("PUBLISHER", function(value) {
      if (
        isTruthy(input$REFERENCE_TYPE) &&
          input$REFERENCE_TYPE %in% c("book", "report") &&
          !isTruthy(value)
      ) {
        "Publisher is required for books and reports"
      }
    })

    # Report-specific required fields
    iv$add_rule("INSTITUTION", function(value) {
      if (
        isTruthy(input$REFERENCE_TYPE) &&
          input$REFERENCE_TYPE == "report" &&
          !isTruthy(value)
      ) {
        "Institution is required for reports"
      }
    })

    for (field_name in names(reference_character_limits())) {
      limit <- reference_character_limits()[[field_name]]
      iv$add_rule(field_name, function(value) {
        if (isTruthy(value) && nchar(value) > limit) {
          paste(field_name, "must be", limit, "characters or less")
        }
      })
    }

    ## InputValidator$enable() ----
    iv$enable()

    # 2. Observers and Reactives ----

    ## observe: update ENTERED_BY field with user_id ----
    # upstream: session$userData$reactiveValues$ENTERED_BY
    # downstream: input$ENTERED_BY
    observe({
      updateTextInput(
        session,
        "ENTERED_BY",
        value = session$userData$reactiveValues$ENTERED_BY
      )
    }) |>
      bindEvent(session$userData$reactiveValues$ENTERED_BY)

    ## observe: DOI/PMID lookup functionality ----
    # upstream: input$lookup_doi, input$doi_lookup
    # downstream: all input field updates
    observe({
      req(input$doi_lookup)

      # Validate and lookup identifier using external function
      lookup_result <- validate_and_lookup_identifier(input$doi_lookup)

      if (!lookup_result$success) {
        showNotification(lookup_result$message, type = "error")
        return()
      }

      # Show success notification with identifier type
      showNotification(lookup_result$message, type = "default")

      # Map fields to reference fields
      mapped_fields <- lookup_result$data

      # Update all input fields with mapped values ----
      updateSelectInput(
        session,
        "REFERENCE_TYPE",
        selected = mapped_fields$REFERENCE_TYPE
      )
      updateTextAreaInput(
        session,
        "AUTHOR",
        value = mapped_fields$AUTHOR %||% ""
      )
      updateTextAreaInput(session, "TITLE", value = mapped_fields$TITLE %||% "")
      updateNumericInput(session, "YEAR", value = mapped_fields$YEAR)
      updateDateInput(session, "ACCESS_DATE", value = mapped_fields$ACCESS_DATE)
      updateTextInput(
        session,
        "PERIODICAL_JOURNAL",
        value = mapped_fields$PERIODICAL_JOURNAL %||% ""
      )
      updateNumericInput(session, "VOLUME", value = mapped_fields$VOLUME)
      updateNumericInput(session, "ISSUE", value = mapped_fields$ISSUE)
      updateTextInput(
        session,
        "PUBLISHER",
        value = mapped_fields$PUBLISHER %||% ""
      )
      updateTextInput(
        session,
        "INSTITUTION",
        value = mapped_fields$INSTITUTION %||% ""
      )
      updateTextInput(session, "DOI", value = mapped_fields$DOI %||% "")
      updateTextInput(session, "URL", value = mapped_fields$URL %||% "")
      updateTextInput(
        session,
        "ISBN_ISSN",
        value = mapped_fields$ISBN_ISSN %||% ""
      )
      updateTextInput(session, "EDITION", value = mapped_fields$EDITION %||% "")
      updateTextInput(
        session,
        "DOCUMENT_NUMBER",
        value = mapped_fields$DOCUMENT_NUMBER %||% ""
      )
      updateTextAreaInput(
        session,
        "REF_COMMENT",
        value = mapped_fields$REF_COMMENT %||% ""
      )

      # Clear the lookup input after successful import
      updateTextInput(session, "doi_lookup", value = "")
    }) |>
      bindEvent(input$lookup_doi)

    ## observe: BibTeX import functionality ----
    # upstream: input$import_bibtex, input$bibtex_import
    # downstream: all input field updates
    observe({
      req(input$bibtex_import)

      # Validate and parse BibTeX using external function
      parse_result <- validate_and_parse_bibtex(input$bibtex_import)

      if (!parse_result$success) {
        showNotification(parse_result$message, type = "error")
        return()
      }

      # Show warning if multiple entries detected
      if (!is.null(parse_result$warning)) {
        showNotification(parse_result$warning, type = "warning")
      }

      # Map BibTeX fields to our reference fields using external function
      mapped_fields <- map_bibtex_to_reference_fields(parse_result$data)

      # Update all input fields with mapped values ----
      updateSelectInput(
        session,
        "REFERENCE_TYPE",
        selected = mapped_fields$REFERENCE_TYPE
      )
      updateTextAreaInput(
        session,
        "AUTHOR",
        value = mapped_fields$AUTHOR %||% ""
      )
      updateTextAreaInput(session, "TITLE", value = mapped_fields$TITLE %||% "")
      updateNumericInput(session, "YEAR", value = mapped_fields$YEAR)
      updateDateInput(session, "ACCESS_DATE", value = mapped_fields$ACCESS_DATE)
      updateTextInput(
        session,
        "PERIODICAL_JOURNAL",
        value = mapped_fields$PERIODICAL_JOURNAL %||% ""
      )
      updateNumericInput(session, "VOLUME", value = mapped_fields$VOLUME)
      updateNumericInput(session, "ISSUE", value = mapped_fields$ISSUE)
      updateTextInput(
        session,
        "PUBLISHER",
        value = mapped_fields$PUBLISHER %||% ""
      )
      updateTextInput(
        session,
        "INSTITUTION",
        value = mapped_fields$INSTITUTION %||% ""
      )
      updateTextInput(session, "DOI", value = mapped_fields$DOI %||% "")
      updateTextInput(session, "URL", value = mapped_fields$URL %||% "")
      updateTextInput(
        session,
        "ISBN_ISSN",
        value = mapped_fields$ISBN_ISSN %||% ""
      )
      updateTextInput(session, "EDITION", value = mapped_fields$EDITION %||% "")
      updateTextInput(
        session,
        "DOCUMENT_NUMBER",
        value = mapped_fields$DOCUMENT_NUMBER %||% ""
      )
      updateTextAreaInput(
        session,
        "REF_COMMENT",
        value = mapped_fields$REF_COMMENT %||% ""
      )

      # Clear the BibTeX input after successful import
      updateTextAreaInput(session, "bibtex_import", value = "")

      # Show success notification
      showNotification(
        "BibTeX data imported successfully",
        type = "default"
      )
    }) |>
      bindEvent(input$import_bibtex)

    ## observe: check validation status and send to session$userData ----
    # upstream: iv
    # downstream: session$userData$reactiveValues$referenceData, referenceDataValid
    observe({
      if (iv$is_valid()) {
        # Generate REFERENCE_ID
        reference_id <- generate_reference_id(
          date = input$YEAR,
          author = input$AUTHOR,
          title = input$TITLE
        )

        # Collect validated data
        validated_data <- initialise_references_tibble() |>
          add_row(
            REFERENCE_ID = reference_id,
            REFERENCE_TYPE = input$REFERENCE_TYPE,
            DATA_SOURCE = input$DATA_SOURCE,
            AUTHOR = input$AUTHOR,
            TITLE = input$TITLE,
            YEAR = input$YEAR,
            ACCESS_DATE = input$ACCESS_DATE,
            PERIODICAL_JOURNAL = input$PERIODICAL_JOURNAL %|truthy|% NA,
            VOLUME = input$VOLUME %|truthy|% NA,
            ISSUE = input$ISSUE %|truthy|% NA,
            PUBLISHER = input$PUBLISHER %|truthy|% NA,
            INSTITUTION = input$INSTITUTION %|truthy|% NA,
            DOI = input$DOI %|truthy|% NA,
            URL = input$URL %|truthy|% NA,
            ISBN_ISSN = input$ISBN_ISSN %|truthy|% NA,
            EDITION = input$EDITION %|truthy|% NA,
            DOCUMENT_NUMBER = input$DOCUMENT_NUMBER %|truthy|% NA,
            REF_COMMENT = input$REF_COMMENT %|truthy|% NA
          )

        # CHANGED: Store directly to userData
        session$userData$reactiveValues$referenceData <- validated_data
        session$userData$reactiveValues$referenceDataValid <- TRUE

        # Update the displayed reference ID
        updateTextInput(session, "REFERENCE_ID_DISPLAY", value = reference_id)
      } else {
        # CHANGED: Set validation flag to FALSE
        session$userData$reactiveValues$referenceDataValid <- FALSE
      }
    })

    ## observe: Populate from LLM data when available ----
    # upstream: session$userData$reactiveValues$referenceDataLLM
    # downstream: input fields
    observe({
      llm_data <- session$userData$reactiveValues$referenceDataLLM
      if (
        nrow(llm_data) > 0 &&
          session$userData$reactiveValues$llmExtractionSuccessful
      ) {
        populate_references_from_llm(session, llm_data)

        # showNotification(
        #   "References form populated.",
        #   type = "message"
        # )
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$referenceDataLLM,
        session$userData$reactiveValues$llmExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )

    ## observer: receive data from session$userData$reactiveValues$referenceData (import) ----
    ## and update module inputs
    observe({
      # CHANGED: Data is already in userData, just need to populate the form
      reference_data <- session$userData$reactiveValues$referenceData |>
        rename_with(.fn = function(x) toupper(x))
      # import data is SCREAMING_NAME but module expects snake_case, so we need to convert the list names
      populate_references_from_llm(
        session,
        reference_data
      )
      print_dev("Populated references form from saved data")
    }) |>
      bindEvent(
        session$userData$reactiveValues$saveExtractionComplete,
        session$userData$reactiveValues$saveExtractionSuccessful,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    ## observe ~ bindEvent: Clear fields button ----
    # upstream: user clicks input$clear
    # downstream: all input fields
    observe(
      {
        # Reset all inputs to default values
        updateSelectInput(session, "REFERENCE_TYPE", selected = "journal")
        updateSelectInput(session, "DATA_SOURCE", selected = "Primary")
        updateTextAreaInput(session, "AUTHOR", value = "")
        updateTextAreaInput(session, "TITLE", value = "")
        updateNumericInput(
          session,
          "YEAR",
          value = as.numeric(format(Sys.Date(), "%Y"))
        )
        updateDateInput(session, "ACCESS_DATE", value = as.Date(NA))
        updateTextInput(session, "PERIODICAL_JOURNAL", value = "")
        updateNumericInput(session, "VOLUME", value = NA)
        updateNumericInput(session, "ISSUE", value = NA)
        updateTextInput(session, "PUBLISHER", value = "")
        updateTextInput(session, "INSTITUTION", value = "")
        updateTextInput(session, "DOI", value = "")
        updateTextInput(session, "URL", value = "")
        updateTextInput(session, "ISBN_ISSN", value = "")
        updateTextInput(session, "EDITION", value = "")
        updateTextInput(session, "DOCUMENT_NUMBER", value = "")
        updateTextAreaInput(session, "REF_COMMENT", value = "")
        updateTextInput(session, "doi_lookup", value = "")
        updateTextAreaInput(session, "bibtex_import", value = "")

        # CHANGED: Clear validation state in userData
        session$userData$reactiveValues$referenceData <- initialise_references_tibble()
        session$userData$reactiveValues$referenceDataValid <- FALSE
      } |>
        suppressWarnings()
    ) |>
      bindEvent(input$clear)

    # 3. Outputs ----

    ## output: validation_reporter ----
    # upstream: session$userData$reactiveValues$referenceDataValid
    # downstream: UI update
    output$validation_reporter <- renderUI({
      # CHANGED: Reference userData validation status
      if (session$userData$reactiveValues$referenceDataValid) {
        div(
          bs_icon("clipboard2-check"),
          "All data validated successfully.",
          class = "validation-status validation-complete"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "Please ensure all required fields are filled, and all entered data is properly formatted.",
          class = "validation-status validation-warning"
        )
      }
    })

    ## output: validated_data_display ----
    # upstream: session$userData$reactiveValues$referenceData (when valid)
    # downstream: UI update
    output$validated_data_display <- renderText(
      {
        # CHANGED: Show data only when valid, reference userData
        if (
          session$userData$reactiveValues$referenceDataValid &&
            nrow(session$userData$reactiveValues$referenceData) > 0
        ) {
          printreactiveValues(session$userData$reactiveValues$referenceData)
        } else {
          "# Data object will be created when valid data is entered."
        }
      }
    )
  })
}

## To be copied in the UI ----
# mod_references_ui("references_1")

## To be copied in the server ----
# references_data <- mod_references_server("references_1")
