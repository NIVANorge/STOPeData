# Review Module ----
# A Shiny module for visual review of measurement data before export

#' Review UI Function ----
#'
#' @description A shiny Module for visual data review with interactive plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput
#' @importFrom bslib card card_body layout_column_wrap accordion accordion_panel
#' @importFrom bsicons bs_icon
#' @importFrom plotly plotlyOutput
#' @import eDataDRF
#' @export
mod_review_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main review card ----
    card(
      fill = TRUE,
      card_body(
        ## Info accordion ----
        info_accordion(content_file = "inst/app/www/md/intro_review.md"),

        ## Data availability status ----
        div(
          style = "margin: 20px 0;",
          uiOutput(ns("data_status"))
        ),

        ## Plot controls ----
        div(
          style = "margin: 20px 0;",
          h5("Visualization Controls"),
          layout_column_wrap(
            width = "300px",
            fill = FALSE,
            fillable = FALSE,

            selectInput(
              inputId = ns("x_variable"),
              label = "X-Axis Variable",
              choices = c(
                "Parameter Name" = "PARAMETER_NAME",
                "Site Code" = "SITE_CODE",
                "Sampling Date" = "SAMPLING_DATE",
                "Compartment" = "ENVIRON_COMPARTMENT",
                "Sub-Compartment" = "ENVIRON_COMPARTMENT_SUB",
                "Parameter Type" = "PARAMETER_TYPE"
              ),
              selected = "PARAMETER_NAME",
              width = "100%"
            ),

            selectInput(
              inputId = ns("color_variable"),
              label = "Color/Group Variable",
              choices = c(
                "Parameter Name" = "PARAMETER_NAME",
                "Measurement Unit" = "MEASURED_UNIT",
                "Parameter Type" = "PARAMETER_TYPE",
                "Site Code" = "SITE_CODE",
                "Compartment" = "ENVIRON_COMPARTMENT",
                "Sub-Compartment" = "ENVIRON_COMPARTMENT_SUB",
                "Measurement Flag" = "MEASURED_FLAG"
              ),
              selected = "MEASURED_UNIT",
              width = "100%"
            ),

            selectInput(
              inputId = ns("facet_variable"),
              label = "Facet Variable (Optional)",
              choices = c(
                "None" = "none",
                "Parameter Name" = "PARAMETER_NAME",
                "Parameter Type" = "PARAMETER_TYPE",
                "Compartment" = "ENVIRON_COMPARTMENT",
                "Sub-Compartment" = "ENVIRON_COMPARTMENT_SUB",
                "Measurement Unit" = "MEASURED_UNIT",
                "Site Code" = "SITE_CODE"
              ),
              selected = "none",
              width = "100%"
            )
          )
        ),

        ## Main visualization ----
        div(
          style = "margin: 20px 0;",
          h5("Measurement Data Overview"),
          plotlyOutput(ns("main_plot"), height = "600px")
        ),

        ## Summary statistics ----
        div(
          style = "margin: 20px 0;",
          h5("Data Summary"),
          layout_column_wrap(
            width = "400px",
            fill = FALSE,
            fillable = FALSE,
          )
        ),

        ## Data quality checks ----
        accordion(
          id = ns("quality_accordion"),
          open = FALSE,
          accordion_panel(
            title = "Data Quality Summary",
            icon = bs_icon("shield-check"),
            div(
              h6("Data Completeness & Quality Indicators"),
              verbatimTextOutput(ns("quality_summary"))
            )
          )
        )
      )
    )
  )
}

#' Review Server Functions ----
#'
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveValues observe renderText renderUI
#' @importFrom plotly renderPlotly plot_ly add_markers add_bars layout config
#' @importFrom dplyr filter group_by summarise count mutate arrange desc case_when n
#' @importFrom ggplot2 ggplot aes geom_point geom_boxplot geom_bar theme_minimal labs
#' @importFrom plotly ggplotly add_text
#' @importFrom glue glue
#' @importFrom golem print_dev
#' @import eDataDRF
#' @export
mod_review_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Module setup ----
    ## ReactiveValues: moduleState ----
    moduleState <- reactiveValues(
      data_available = FALSE,
      review_data = NULL,
      filtered_data = NULL
    )

    # 2. Observers and Reactives ----

    ## Helper: Create dummy data for testing ----
    create_dummy_review_data <- function() {
      set.seed(42) # For reproducible dummy data

      dummy_data <- tibble(
        SAMPLE_ID = paste0("DUMMY_", sprintf("%03d", 1:50)),
        SITE_CODE = rep(
          c("DEMO_SITE_A", "DEMO_SITE_B", "DEMO_SITE_C"),
          length.out = 50
        ),
        PARAMETER_NAME = rep(
          c(
            "FAKE_Mercury",
            "FAKE_Lead",
            "FAKE_Cadmium",
            "FAKE_pH",
            "FAKE_Temperature"
          ),
          each = 10
        ),
        SAMPLING_DATE = rep(
          seq(as.Date("2024-01-01"), by = "week", length.out = 10),
          5
        ),
        ENVIRON_COMPARTMENT = rep(
          c("Aquatic", "Terrestrial", "Biota"),
          length.out = 50
        ),
        ENVIRON_COMPARTMENT_SUB = rep(
          c("Freshwater", "Soil A Horizon", "Biota, Aquatic"),
          length.out = 50
        ),
        PARAMETER_TYPE = rep(
          c(
            "Stressor",
            "Stressor",
            "Stressor",
            "Quality parameter",
            "Quality parameter"
          ),
          each = 10
        ),
        SUBSAMPLE = rep(1:2, length.out = 50),

        # Create realistic measurement data with some variation
        MEASURED_VALUE = c(
          # Mercury (ug/L)
          round(runif(10, 0.1, 5.2), 3),
          # Lead (ug/L)
          round(runif(10, 0.5, 15.8), 3),
          # Cadmium (ug/L)
          round(runif(10, 0.05, 2.1), 4),
          # pH (unitless)
          round(runif(10, 6.2, 8.5), 2),
          # Temperature (deg C)
          round(runif(10, 4.2, 18.7), 1)
        ),

        MEASURED_FLAG = c(
          # Some values below detection limits
          rep("", 8),
          rep("<LOQ", 2), # Mercury
          rep("", 9),
          rep("<LOD", 1), # Lead
          rep("", 7),
          rep("<LOQ", 3), # Cadmium
          rep("", 10), # pH
          rep("", 10) # Temperature
        ),

        MEASURED_UNIT = c(
          rep("ug/L", 30), # Metals
          rep("pH", 10), # pH
          rep("deg C", 10) # Temperature
        ),

        MEASURED_SD = c(
          round(runif(30, 0.01, 0.5), 3), # Metals
          round(runif(10, 0.1, 0.3), 2), # pH
          round(runif(10, 0.2, 1.0), 1) # Temperature
        ),

        LOQ_VALUE = c(
          rep(0.1, 10), # Mercury
          rep(0.5, 10), # Lead
          rep(0.05, 10), # Cadmium
          rep(NA, 20) # pH and Temperature
        ),

        LOQ_UNIT = c(
          rep("ug/L", 30),
          rep(NA, 20)
        ),

        LOD_VALUE = c(
          rep(0.05, 10), # Mercury
          rep(0.25, 10), # Lead
          rep(0.025, 10), # Cadmium
          rep(NA, 20) # pH and Temperature
        ),

        LOD_UNIT = c(
          rep("ug/L", 30),
          rep(NA, 20)
        ),

        # Campaign info
        CAMPAIGN_NAME = "DEMO_CAMPAIGN_2024"
      )

      return(dummy_data)
    }

    ## observe: Check for available data ----
    # upstream: session$userData$reactiveValues$measurementsData
    # downstream: moduleState$data_available, moduleState$review_data
    observe({
      data <- session$userData$reactiveValues$measurementsData

      if (!is.null(data) && nrow(data) > 0) {
        moduleState$data_available <- TRUE
        moduleState$review_data <- data
        print_dev(glue(
          "mod_review: Real data available - {nrow(data)} records"
        ))
      } else {
        # Use dummy data for testing/demo
        moduleState$data_available <- TRUE
        moduleState$review_data <- create_dummy_review_data()
        print_dev("mod_review: Using dummy data for demo purposes")
      }
    }) |>
      bindEvent(
        session$userData$reactiveValues$measurementsData,
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )

    ## reactive: Filter data for plotting ----
    # upstream: moduleState$review_data
    # downstream: plotting functions
    filtered_data <- reactive({
      req(moduleState$review_data)

      data <- moduleState$review_data

      # Filter out rows with missing measured values (unless flagged as <LOQ/<LOD)
      data <- data |>
        filter(
          !is.na(MEASURED_VALUE) |
            (!is.na(MEASURED_FLAG) & MEASURED_FLAG %in% c("<LOQ", "<LOD"))
        )

      # For plotting purposes, set <LOQ/<LOD values to half of their respective limits
      data <- data |>
        mutate(
          PLOT_VALUE = case_when(
            !is.na(MEASURED_VALUE) ~ MEASURED_VALUE,
            MEASURED_FLAG == "<LOQ" & !is.na(LOQ_VALUE) ~ LOQ_VALUE / 2,
            MEASURED_FLAG == "<LOD" & !is.na(LOD_VALUE) ~ LOD_VALUE / 2,
            TRUE ~ NA_real_
          ),
          VALUE_TYPE = case_when(
            !is.na(MEASURED_VALUE) ~ "Measured",
            MEASURED_FLAG == "<LOQ" ~ "Below LOQ",
            MEASURED_FLAG == "<LOD" ~ "Below LOD",
            TRUE ~ "Missing"
          )
        )

      return(data)
    })

    # 3. Outputs ----

    ## output: data_status ----
    # upstream: moduleState$data_available
    # downstream: UI status display
    output$data_status <- renderUI({
      if (moduleState$data_available) {
        data_summary <- list(
          total_records = nrow(moduleState$review_data),
          unique_parameters = length(unique(
            moduleState$review_data$PARAMETER_NAME
          )),
          unique_sites = length(unique(moduleState$review_data$SITE_CODE)),
          unique_units = length(unique(moduleState$review_data$MEASURED_UNIT))
        )

        div(
          bs_icon("check-circle"),
          glue(
            "Data available for review: {data_summary$total_records} records, "
          ),
          glue("{data_summary$unique_parameters} parameters, "),
          glue("{data_summary$unique_sites} sites, "),
          glue("{data_summary$unique_units} measurement units"),
          class = "validation-status validation-complete",
          style = "padding: 10px; border-radius: 4px;"
        )
      } else {
        div(
          bs_icon("exclamation-triangle"),
          "No measurement data available. Complete the Data Entry module first.",
          class = "validation-status validation-warning",
          style = "padding: 10px; border-radius: 4px;"
        )
      }
    })

    ## output: main_plot ----
    # upstream: filtered_data(), input controls
    # downstream: UI main visualization
    output$main_plot <- renderPlotly({
      req(filtered_data())

      data <- filtered_data()
      x_var <- input$x_variable
      color_var <- input$color_variable
      facet_var <- input$facet_variable

      if (nrow(data) == 0) {
        # Empty plot with message
        plot_ly() |>
          add_text(x = 0.5, y = 0.5, text = "No data available for plotting") |>
          layout(
            title = "No Data Available",
            xaxis = list(showgrid = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE)
          )
      } else {
        # Create scatter plot
        p <- plot_ly(
          data = data,
          x = ~ get(x_var),
          y = ~PLOT_VALUE,
          color = ~ get(color_var),
          symbol = ~VALUE_TYPE,
          text = ~ paste(
            "Parameter:",
            PARAMETER_NAME,
            "<br>Site:",
            SITE_CODE,
            "<br>Value:",
            ifelse(
              !is.na(MEASURED_VALUE),
              MEASURED_VALUE,
              paste(MEASURED_FLAG, "flag")
            ),
            "<br>Unit:",
            MEASURED_UNIT,
            "<br>Date:",
            SAMPLING_DATE
          ),
          hovertemplate = "%{text}<extra></extra>",
          type = "scatter",
          mode = "markers"
        ) |>
          layout(
            title = paste("Measured Values by", gsub("_", " ", x_var)),
            xaxis = list(title = gsub("_", " ", x_var)),
            yaxis = list(title = "Measured Value (log scale)", type = "log"),
            hovermode = "closest"
          ) |>
          config(displayModeBar = TRUE)

        # Add faceting if requested
        if (facet_var != "none") {
          # Note: plotly doesn't have built-in faceting like ggplot2
          # For now, we'll just color by the facet variable instead
          p <- p |>
            layout(
              title = paste(
                "Measured Values by",
                gsub("_", " ", x_var),
                "- Grouped by",
                gsub("_", " ", facet_var)
              )
            )
        }

        p
      }
    })

    ## output: value_distribution ----
    # upstream: filtered_data()
    # downstream: UI value distribution plot
    output$value_distribution <- renderPlotly({
      req(filtered_data())

      data <- filtered_data()

      if (nrow(data) == 0) {
        plot_ly() |>
          add_text(x = 0.5, y = 0.5, text = "No data") |>
          layout(title = "No Data")
      } else {
        plot_ly(
          data = data,
          x = ~PLOT_VALUE,
          type = "histogram",
          nbinsx = 20
        ) |>
          layout(
            title = "Distribution of Measured Values",
            xaxis = list(title = "Measured Value (log scale)", type = "log"),
            yaxis = list(title = "Count")
          )
      }
    })

    ## output: quality_summary ----
    # upstream: filtered_data()
    # downstream: UI quality summary text
    output$quality_summary <- renderText({
      if (!moduleState$data_available) {
        return("No data available for quality assessment")
      }

      data <- filtered_data()
      original_data <- moduleState$review_data

      # Calculate quality metrics
      total_records <- nrow(original_data)
      measured_values <- sum(!is.na(original_data$MEASURED_VALUE))
      loq_flags <- sum(original_data$MEASURED_FLAG == "<LOQ", na.rm = TRUE)
      lod_flags <- sum(original_data$MEASURED_FLAG == "<LOD", na.rm = TRUE)
      missing_values <- total_records - measured_values - loq_flags - lod_flags

      # Unit consistency
      units_per_parameter <- original_data |>
        group_by(PARAMETER_NAME) |>
        summarise(
          unique_units = length(unique(MEASURED_UNIT)),
          .groups = "drop"
        ) |>
        filter(unique_units > 1)

      # Value range summary
      value_summary <- summary(data$PLOT_VALUE[!is.na(data$PLOT_VALUE)])

      quality_report <- paste0(
        "DATA COMPLETENESS:\n",
        "Total Records: ",
        total_records,
        "\n",
        "Measured Values: ",
        measured_values,
        " (",
        round(100 * measured_values / total_records, 1),
        "%)\n",
        "Below LOQ: ",
        loq_flags,
        " (",
        round(100 * loq_flags / total_records, 1),
        "%)\n",
        "Below LOD: ",
        lod_flags,
        " (",
        round(100 * lod_flags / total_records, 1),
        "%)\n",
        "Missing/Invalid: ",
        missing_values,
        " (",
        round(100 * missing_values / total_records, 1),
        "%)\n\n",

        "UNIT CONSISTENCY:\n",
        "Parameters with multiple units: ",
        nrow(units_per_parameter),
        "\n",
        if (nrow(units_per_parameter) > 0) {
          paste0(
            "Warning: Check unit consistency for: ",
            paste(units_per_parameter$PARAMETER_NAME, collapse = ", "),
            "\n"
          )
        } else {
          "All parameters have consistent units\n"
        },
        "\n",

        "VALUE DISTRIBUTION:\n",
        "Min: ",
        round(value_summary[1], 4),
        "\n",
        "1st Qu: ",
        round(value_summary[2], 4),
        "\n",
        "Median: ",
        round(value_summary[3], 4),
        "\n",
        "Mean: ",
        round(value_summary[4], 4),
        "\n",
        "3rd Qu: ",
        round(value_summary[5], 4),
        "\n",
        "Max: ",
        round(value_summary[6], 4),
        "\n"
      )

      return(quality_report)
    })
  })
}

## To be copied in the UI ----
# mod_review_ui("review_1")

## To be copied in the server ----
# mod_review_server("review_1")
