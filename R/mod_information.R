#' information UI Function
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card card_body
mod_information_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(card_body(
      includeMarkdown(system.file(
        "app/www/md/manual.md",
        package = "STOPeData"
      )),
      includeMarkdown(system.file(
        "app/www/md/citations.md",
        package = "STOPeData"
      )),
      includeMarkdown(system.file(
        "app/www/md/package_citations.md",
        package = "STOPeData"
      ))
    ))
  )
}

#' information Server Functions
#'
#' @noRd
mod_information_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_information_ui("information_1")

## To be copied in the server
# mod_information_server("information_1")
