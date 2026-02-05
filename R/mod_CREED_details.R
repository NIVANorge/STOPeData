#' @noRd
#' @importFrom shiny NS tagList textAreaInput
#' @importFrom bslib input_task_button tooltip
mod_CREED_details_ui <- function(id) {
  ns <- NS(id)

  tagList(
    input_task_button(
      id = ns("populate_from_data"),
      label = "Populate section from data",
      icon = bs_icon("arrow-down-circle")
    ),
    br(),

    layout_column_wrap(
      width = "800px",
      fill = FALSE,
      fillable = FALSE,

      ### Auto-populated fields ----
      textAreaInput(
        inputId = ns("source_auto"),
        label = tooltip(
          list(
            "Source (reference)",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Reference citation or source of the dataset being processed."
        ),
        value = "",
        rows = 2,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("analyte_auto"),
        label = tooltip(
          list(
            "Reported Analyte",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Chemical analyte(s) or substance(s) measured in the study."
        ),
        value = "",
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("medium_auto"),
        label = tooltip(
          list(
            "Sample Medium/Matrix",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Type of environmental medium sampled (e.g., water, sediment, biota)."
        ),
        value = "",
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("study_area_auto"),
        label = tooltip(
          list(
            "Study Area",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Geographic area or region where sampling was conducted."
        ),
        value = "",
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("num_sites_auto"),
        label = tooltip(
          list(
            "Number of Sites",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Total number of sampling locations in the study."
        ),
        value = "",
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("num_samples_auto"),
        label = tooltip(
          list(
            "Number of Samples",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Total number of samples collected and analyzed."
        ),
        value = "",
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("sampling_period_auto"),
        label = tooltip(
          list(
            "Sampling Period",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Time period when samples were collected (e.g., dates, seasons, years)."
        ),
        value = "",
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("analytical_methods_auto"),
        label = tooltip(
          list(
            "Analytical Method(s)",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Laboratory methods used for chemical analysis of samples."
        ),
        value = "",
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("loq_auto"),
        label = tooltip(
          list(
            "Limit of Quantification",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Lowest concentration of a stressor that can be reliably quantified by the analytical method."
        ),
        value = "",
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("site_types"),
        label = tooltip(
          list(
            "Site Type(s)",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Description of sampling site characteristics (e.g., urban, rural, industrial, background)."
        ),
        rows = 1,
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("sampling_methods_auto"),
        label = tooltip(
          list(
            "Sampling Method(s)",
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          ),
          "Field protocols and equipment used for sample collection."
        ),
        rows = 1,
        width = "100%"
      ),

      ### User input fields ----

      textAreaInput(
        inputId = ns("sampling_conditions"),
        label = tooltip(
          list("Sampling Conditions", bs_icon("info-circle-fill")),
          "Describe environmental conditions during sampling (e.g., weather, season, flow conditions)."
        ),
        placeholder = "Describe sampling conditions, etc.",
        rows = 1,
        width = "100%"
      ),

      textInput(
        inputId = ns("site_density"),
        label = tooltip(
          list("Site Density", bs_icon("info-circle-fill")),
          "Spatial distribution of sampling sites (e.g., 1 site per 100 km\u00B2, grid spacing)."
        ),
        placeholder = "e.g., 1 site per 100 km\u00B2",
        width = "100%"
      ),
      textInput(
        inputId = ns("sampling_frequency"),
        label = tooltip(
          list("Sampling Frequency", bs_icon("info-circle-fill")),
          "Temporal frequency of sample collection (e.g., monthly, quarterly, one-time)."
        ),
        placeholder = "e.g., monthly, quarterly",
        width = "100%"
      ),

      textAreaInput(
        inputId = ns("other_details"),
        label = tooltip(
          list("Other Details", bs_icon("info-circle-fill")),
          "Any additional relevant information about the study design, methods, or data quality."
        ),
        placeholder = "Any additional relevant information",
        rows = 3,
        width = "100%"
      )
    ),

    ## Action buttons ----
    # input_task_button(
    #   id = ns("save_assessment"),
    #   label = "Save Section",
    #   icon = icon("save"),
    #   class = "btn-success"
    # ) |>
    #   shinyjs::disabled() # TODO: Necessary?
  )
}


#' CREED_details Server Functions
#' @noRd
#' @importFrom golem print_dev
#' @importFrom shiny updateTextAreaInput bindEvent isTruthy
#' @importFrom tibble tibble
#' @import eDataDRF
mod_CREED_details_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observe ~bindEvent(populate_from_data): Auto-populate fields ----
    # upstream: user clicks populate_from_data
    # downstream: all auto-populated textAreaInput fields
    observe({
      session$userData$reactiveValues$datasetDetails <- summarise_CREED_details(
        session$userData$reactiveValues
      )
      # print_dev(
      #   "writing dataset details to session data, auto-populating relevant inputs)"
      # )
      # print_dev(dput(session$userData$reactiveValues$datasetDetails))

      # Update UI fields
      # Avoid writing out the whole layered call every time
      dataset_details <- session$userData$reactiveValues$datasetDetails
      # convert to list for easier access
      dataset_details <- setNames(dataset_details$value, dataset_details$field)

      updateTextAreaInput(
        session,
        "source_auto",
        value = dataset_details[["source"]]
      )
      updateTextAreaInput(
        session,
        "analyte_auto",
        value = dataset_details[["analytes"]]
      )
      updateTextAreaInput(
        session,
        "medium_auto",
        value = dataset_details[["medium"]]
      )
      updateTextAreaInput(
        session,
        "study_area_auto",
        value = dataset_details[["study_area"]]
      )
      updateTextAreaInput(
        session,
        "num_sites_auto",
        value = dataset_details[["num_sites"]]
      )
      updateTextAreaInput(
        session,
        "site_types",
        value = dataset_details[["site_types"]]
      )
      updateTextAreaInput(
        session,
        "num_samples_auto",
        value = dataset_details[["num_samples"]]
      )
      updateTextAreaInput(
        session,
        "sampling_period_auto",
        value = dataset_details[["sampling_period"]]
      )
      updateTextAreaInput(
        session,
        "analytical_methods_auto",
        value = dataset_details[["analytical_methods"]]
      )
      updateTextAreaInput(
        session,
        "sampling_methods_auto",
        value = dataset_details[["sampling_methods"]]
      )
      updateTextAreaInput(
        session,
        "loq_auto",
        value = dataset_details[["loq_info"]]
      )

      print_dev("CREED Dataset Details auto-populated")
    }) |>
      bindEvent(
        input$populate_from_data,
        session$userData$reactiveValues$creedGetData,
        ignoreInit = TRUE
      )
  })
}

## To be copied in the UI
# mod_CREED_details_ui("CREED_details_1")

## To be copied in the server
# mod_CREED_details_server("CREED_details_1")
