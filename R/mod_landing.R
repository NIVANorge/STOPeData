#' landing UI Function
#' @noRd
#'
#' @importFrom shiny NS tagList icon
#' @importFrom shinyjs disabled
#' @importFrom bslib layout_column_wrap card card_body
#' @importFrom bslib input_task_button
mod_landing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1 / 2,
      card(
        fill = TRUE,
        fillable = TRUE,
        card_body(
          fill = TRUE,
          fillable = TRUE,
          ## Info accordion ----
          info_accordion(
            title = "Getting Started",
            content_file = "inst/app/www/md/intro_landing.md",
            div(
              "Most sections can be expanded to fill the full screen with the ",
              tooltip(
                bs_icon("arrows-angle-expand"),
                "The actual icons don't quite look identical to this one. If you can tell me why you will win a prize."
              ),
              "Expand button in the bottom right of the section.",
              "Mouse over icons like this for more information",
              tooltip(
                bs_icon("info-circle-fill"),
                "This type of mouse-over element is often called a 'tooltip'"
              ),
              " are available on each page. For an FAQ and more details instructions, go to the",
              bs_icon("info-circle"),
              "Info tab."
            )
          ),
          h5("Quick Start Buttons"),
          layout_column_wrap(
            width = "400px",
            min_height = "200px",
            fill = FALSE,
            tooltip(
              input_task_button(
                id = ns("narrative_llm"),
                label = HTML(paste(
                  bs_icon("cpu"),
                  "Extract with LLM"
                )),
                width = "350px",
                type = "primary",
              ),
              "Extract data from a PDF using a Large Language Model. Generally faster and more efficient."
            ),
            tooltip(
              input_task_button(
                id = ns("narrative_manual"),
                label = HTML(paste(
                  bs_icon("keyboard"),
                  "Extract data manually"
                )),
                width = "350px",
                type = "info"
              ),
              "Extract data from a source manually. Less efficient, but may be necessary under some circumstances."
            ),

            tooltip(
              input_task_button(
                id = ns("upload_zip"),
                label = HTML(paste(
                  bs_icon("file-earmark-zip"),
                  "Continue an extraction"
                )),
                width = "350px",
                type = "success"
              ),

              "Continue an earlier extraction by uploading a .zip save file."
            )
          ),
        )
      ),
      card(
        fill = TRUE,
        fillable = TRUE,
        card_body(
          div(
            style = "margin-bottom: 10px;",
            # style = "display: block !important; margin: auto !important; max-width: 100% !important; max-height: 500px !important;",
            info_accordion(
              title = paste0("Version ", golem::get_golem_version()),
              content_file = "inst/app/www/md/whats_new.md"
            ),
            br()
          )
        )
      )
    )
  )
}

#' landing Server Functions
#'
#' @param parent_session The parent session object to access main navbar
#' @noRd
#' @importFrom shiny moduleServer updateNavbarPage
mod_landing_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Navigation observers ----

    ## observe: Navigate to LLM extract when LLM button clicked ----
    # Upstream: input$narrative_llm button click
    # Downstream: Updates main navbar to LLM extract tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "00-llm-extract"
      )
    }) |>
      bindEvent(input$narrative_llm)

    ## observe: Navigate to campaign when manual entry button clicked ----
    # Upstream: input$narrative_manual button click
    # Downstream: Updates main navbar to campaign tab
    observe({
      updateNavbarPage(
        session = parent_session,
        inputId = "main-page",
        selected = "01-campaign"
      )
    }) |>
      bindEvent(input$narrative_manual)
  })
}

## To be copied in the UI
# mod_landing_ui("landing_1")

## To be copied in the server
# mod_landing_server("landing_1")
