# CREED Reliability Assessment Module ----
# A Shiny module for CREED reliability criteria evaluation
#' @importFrom shiny NS tagList textInput textAreaInput actionButton checkboxInput renderText
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel input_task_button
#' @importFrom bsicons bs_icon

mod_CREED_reliability_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main reliability section ----

    input_task_button(
      id = ns("populate_from_data"),
      label = "Populate section from data",
      icon = bs_icon("arrow-down-circle")
    ),
    # Reliability criteria using helper functions ----

    # RB1: Media - Sample medium/matrix ----
    create_criterion_section(
      ns,
      criterion_id = "RB1",
      title = "Sample Medium/Matrix",
      type = "required",
      description = "Was the sampling medium/matrix reported in detail (for water: dissolved fraction or whole water; for sediment: sieved or whole; for soil, grain size; for biota, species, age, sex, tissue type), and was the matrix appropriate for the analyte of interest?"
    ),

    ## RB2: Media - Collection method/sample type ----
    create_criterion_section(
      ns,
      criterion_id = "RB2",
      title = "Collection Method/Sample Type",
      type = "recommended",
      description = "Was the sample collection method reported? Examples include grab, depth- and width-integrated, discrete, composite, or time-integrated samples, or continuous monitoring."
    ),

    ## RB3: Media - Sample handling ----
    create_criterion_section(
      ns,
      criterion_id = "RB3",
      title = "Sample Handling",
      type = "recommended",
      description = "Was information reported on sample handling (transport conditions, preservation, filtration, storage)? Was the type of container suitable for use with the analyte of interest (i.e., no loss or contamination)?"
    ),

    ## RB4: Spatial - Site location ----
    create_criterion_section(
      ns,
      criterion_id = "RB4",
      title = "Site Location",
      type = "required",
      description = "Were the site locations reported?"
    ),

    ## RB5: Temporal - Date and time ----
    create_criterion_section(
      ns,
      criterion_id = "RB5",
      title = "Date and Time",
      type = "required",
      description = "Were the date and time of sample collection reported?"
    ),

    ## RB6: Analytical - Analyte(s) measured ----
    create_criterion_section(
      ns,
      criterion_id = "RB6",
      title = "Analyte(s) Measured",
      type = "required",
      description = "Was/were the analyte(s) of interest suitably and definitively identified?"
    ),

    ## RB7: Analytical - LOD/LOQ ----
    create_criterion_section(
      ns,
      criterion_id = "RB7",
      title = "Limit of Detection and/or Limit of Quantification",
      type = "required",
      description = "Were limits of detection and/or quantification provided?"
    ),

    ## RB8: Analytical - Accreditation/QMS (Shortcut criterion) ----
    div(
      style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
        div(
          style = "flex-grow: 0; margin-right: 20px;",
          h6(
            HTML(paste(
              bs_icon("award-fill", class = "CREED-required"),
              "RB8: Accreditation/Quality Management System (Required - Shortcut Criterion)"
            ))
          ),
          p(
            strong("Criterion: "),
            "Were the laboratory and method accredited for all or almost all samples? Several national and international accreditation bodies are available (e.g., ISO, UKAS); was that laboratory and/or method certified to these standards? Was a quality system (such as, e.g., ISO 17025) adopted?"
          ),
          div(
            class = "alert alert-info",
            p(
              bs_icon("info-circle"),
              strong(" Shortcut Criterion: "),
              "If you answer 'Fully Met' to this question, you may skip questions RB9-RB12. If not, please complete those questions."
            )
          )
        ),
        selectInput(
          inputId = ns("RB8_score"),
          label = "Score:",
          choices = names(CREED_choices_vocabulary()),
          width = "150px"
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        create_relevant_data_input(ns, "RB8"),
        create_justification_input(ns, "RB8")
      )
    ),

    # RB9-RB12: Conditional criteria ----
    div(
      id = ns("RB9_container"),
      create_criterion_section(
        ns,
        criterion_id = "RB9",
        title = "Method",
        type = "required",
        description = "Was the method sufficiently described or referenced, such that it can be reproduced if necessary? Was method validation included?"
      )
    ),

    div(
      id = ns("RB10_container"),
      create_criterion_section(
        ns,
        criterion_id = "RB10",
        title = "Lab Blank Contamination",
        type = "recommended",
        description = "Was method blank contamination assessed with laboratory blanks?"
      )
    ),

    div(
      id = ns("RB11_container"),
      create_criterion_section(
        ns,
        criterion_id = "RB11",
        title = "Recovery/Accuracy",
        type = "recommended",
        description = "Were method recovery/accuracy and/or uncertainty 
    assessed by recovery of standard reference material (SRM) and/or were lab spike samples assessed?"
      )
    ),

    div(
      id = ns("RB12_container"),
      create_criterion_section(
        ns,
        criterion_id = "RB12",
        title = "Reproducibility/Precision",
        type = "recommended",
        description = "Were method reproducibility and/or uncertainty assessed with lab replicates and long-term control recoveries?"
      )
    ),

    ## RB13: Analytical - Field QC ----
    create_criterion_section(
      ns,
      criterion_id = "RB13",
      title = "Field QC",
      type = "recommended",
      description = "Were quality control (QC) samples collected during field 
    sampling (such as field blanks, spikes, replicates) to demonstrate the method performance for a given field study?"
    ),

    ## RB14: Data Handling - Calculations ----
    create_criterion_section(
      ns,
      criterion_id = "RB14",
      title = "Calculations (if dataset contains calculated values)",
      type = "recommended",
      description = "If chemical concentrations were normalised or adjusted (e.g., to represent bioavailability or toxicity), then were the calculations explained and were they appropriate?"
    ),

    ## RB15: Data Handling - Significant Figures ----
    create_criterion_section(
      ns,
      criterion_id = "RB15",
      title = "Significant Figures (if dataset contains calculated values)",
      type = "recommended",
      description = "During calculations, were data reported to the appropriate number of significant figures or decimal places?"
    ),

    ## RB16: Data Handling - Outliers ----
    create_criterion_section(
      ns,
      criterion_id = "RB16",
      title = "Outliers (if dataset mentions outliers)",
      type = "recommended",
      description = "For any outliers deleted from the data set, was evidence provided that these outliers were due to an error in measurement or contamination?"
    ),

    ## RB17: Data Handling - Censored Data ----
    create_criterion_section(
      ns,
      criterion_id = "RB17",
      title = "Censored Data (if dataset contains censored values)",
      type = "required",
      description = "Were censored data reported correctly (e.g., as a numerical value plus a less-than sign or another indicator of a nondetect)? If a substitution method was used for nondetects (e.g., censored data were replaced by zero, or by 1/2 or another fraction of the LOD/LOQ), then can the original censored data be restored by back-calculation using the reported LOD/LOQ?"
    ),

    ## RB18: Data Handling - Summary Statistics Procedures ----
    create_criterion_section(
      ns,
      criterion_id = "RB18",
      title = "Summary Statistics Procedures (if dataset contains summary statistics)",
      type = "recommended",
      description = "Were summary statistics calculated appropriately? If the dataset contained censored data, then were censored data included and were appropriate procedures used to determine summary statistics?"
    ),

    ## RB19: Supporting Parameters - Supporting Data Quality ----
    create_criterion_section(
      ns,
      criterion_id = "RB19",
      title = "Supporting Data Quality (if supporting parameters are required for the purpose)",
      type = "recommended",
      description = "If any supporting parameters are required for the assessment purpose, then were the supporting parameter data provided, and were their methods and data quality addressed?"
    ),

    ## Action buttons and status ----
    # input_task_button(
    #   id = ns("calc_scores"),
    #   label = "Calculate Reliability Score"
    # )
    # input_task_button(
    #   id = ns("save_assessment"),
    #   label = "Save Section",
    #   icon = icon("save"),
    #   class = "btn-success"
    # ),

    # div(
    #   style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
    #   h6("Completion Status"),
    #   uiOutput(ns("completion_status"))
    # )
  )
}

#' CREED Reliability Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderUI isTruthy
mod_CREED_reliability_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: reliability_scores ----
    reliability_scores <- reactiveValues()

    # 2. Observers ----

    ## observe: RB8 conditional logic ----
    # upstream: input$RB8_score
    # downstream: RB9-RB12 container styling
    observe({
      rb8_fully_met <- input$RB8_score == "fully"

      # Apply/remove disabled styling to conditional questions
      conditional_ids <- c(
        "RB9_container",
        "RB10_container",
        "RB11_container",
        "RB12_container"
      )

      for (container_id in conditional_ids) {
        if (rb8_fully_met) {
          shinyjs::addClass(id = container_id, class = "criterion-disabled")
        } else {
          shinyjs::removeClass(id = container_id, class = "criterion-disabled")
        }
      }
    }) |>
      bindEvent(input$RB8_score, ignoreInit = TRUE)

    ## observe: autopopulate from session$userData ----
    # upstream: button populate_from_data
    # downstream: reliability input fields
    observe({
      # Get auto-populated data directly from userData
      auto_data <- summarise_CREED_reliability(session$userData$reactiveValues)

      # Update relevant_data fields
      for (field_name in names(auto_data)) {
        updateTextAreaInput(
          session,
          field_name,
          value = auto_data[[field_name]]
        )
      }

      showNotification("Reliability fields updated from data", type = "message")
    }) |>
      bindEvent(input$populate_from_data)

    ## observe: Auto-populate relevant data fields ----
    # upstream: input$populate_from_data OR session$userData$reactiveValues$creedGetData
    # downstream: All RB*_relevant_data inputs
    observe({
      req(session$userData$reactiveValues)

      tryCatch(
        {
          # Get auto-populated field values
          field_updates <- autopop_reliability_fields(
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
            "Reliability data fields populated from dataset",
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

    ## observe: Collect reliability data ----
    # upstream: input$calc_scores, session$userData$reactiveValues$creedCalculateScores
    # downstream: session$userData$reactiveValues$creedReliability
    observe({
      # Collect and store data ----
      session$userData$reactiveValues$creedReliability <- collect_CREED_data(
        # defined in mod_CREED_fct_helpers.R
        criteria_config = CREED_reliability_criteria_config(),
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

## To be copied in the UI ----
# mod_CREED_reliability_ui("reliability_1")

## To be copied in the server ----
# mod_CREED_reliability_server("reliability_1")
