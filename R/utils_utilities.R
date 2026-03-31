#' utilities
#' @title isTruthy switch operator
#' @name isTruthy switch operator
#'
#' @description isTruthy switch operator
#'
#' @return object types are unchanged
#'
#' @details
#' A simple switch operator based on purrr's %||% that returns the first
#' argument if it isTruthy (exists, not NULL/NA/"", etc.), and otherwise the
#' second argument.
#'
#' @param first The first argument, a value we want to use only if it isTruthy
#' @param second The second argument, a safe alternative if first isn't Truthy
#'
#' @importFrom shiny isTruthy
#' @noRd
`%|truthy|%` <- function(first, second) {
  if (isTruthy(first)) {
    first
  } else {
    second
  }
}


#' Reference field character limits
#'
#' Returns maximum character lengths for text fields in the references table
#' ([eDataDRF::initialise_references_tibble()]).
#'
#' @details
#' Provides a named list of character limits for reference metadata fields to ensure
#' data quality and database compatibility. Limits include:
#'
#' DOCUMENT_NUMBER: 200 characters
#'
#' DOI: 200 characters
#'
#' EDITION: 200 characters
#'
#' INSTITUTION: 200 characters
#'
#' ISBN_ISSN: 200 characters
#'
#' PERIODICAL_JOURNAL: 200 characters
#'
#' PUBLISHER: 200 characters
#'
#' REF_COMMENT: 1000 characters
#'
#' URL: 200 characters
#'
#' @return A named list of character limits for reference fields
#' @family limits
#' @family reference
#' @seealso [eDataDRF::initialise_references_tibble()],
#'   [eDataDRF::example_references_tibble()],
#'   [eDataDRF::reference_type_vocabulary()],
#'   [eDataDRF::data_source_vocabulary()]
#' @examples
#' limits <- reference_character_limits()
#' limits$DOI
#' limits$REF_COMMENT
#' @export
reference_character_limits <- function() {
  list(
    # ACCESSION_NUMBER = 200,  # COMMENTED OUT
    # DB_NAME = 200,           # COMMENTED OUT
    # DB_PROVIDER = 200,       # COMMENTED OUT
    DOCUMENT_NUMBER = 200,
    DOI = 200,
    EDITION = 200,
    INSTITUTION = 200,
    ISBN_ISSN = 200,
    # NUMBER_OF_PAGES = 50,    # COMMENTED OUT
    # NUMBER_OF_VOLUMES = 100, # COMMENTED OUT
    # PAGES = 200,             # COMMENTED OUT
    PERIODICAL_JOURNAL = 200,
    # PMCID = 200,             # COMMENTED OUT
    # PUBLISHED_PLACE = 200,   # COMMENTED OUT
    PUBLISHER = 200,
    REF_COMMENT = 1000,
    # SERIES_EDITOR = 200,     # COMMENTED OUT
    # SERIES_TITLE = 200,      # COMMENTED OUT
    URL = 200
  )
}

#' @title isRelevant
#'
#' @description Check if a categorical variable is "Not relevant" or "Not reported"
#'
#' @return Boolean
#'
#' @details
#' Check if a user has entered "Not relevant" or "Not reported" in selectInput
#' for the purpose of including conditional fields, validation, etc.
#'
#' @param input the variable to check
#'
#' @noRd
isRelevant <- function(input) {
  stopifnot(is.character(input))
  !(input %in% c("Not relevant", "Not reported"))
}

#' Print content of reactiveValues object
#'
#' @description
#' Print a reactiveValues object, with each named variable and its value on a new
#' line
#'
#' @param data A reactiveValues object with named variables
#' @return A string of variable names and values
#' @importFrom shiny reactiveValues
#' @examples
#' \dontrun{
#'   rv <- shiny::reactiveValues(campaign_name = "North Sea 2022", year = 2022)
#'   printreactiveValues(rv)
#' }
#' @export
printreactiveValues <- function(data) {
  data_lines <- sapply(
    names(data),
    function(name) {
      value <- data[[name]]
      if (is.na(value) || is.null(value)) {
        paste0(name, " = NA")
      } else if (inherits(value, "Date")) {
        paste0(name, " = as.Date('", as.character(value), "')")
      } else if (is.character(value)) {
        paste0(name, " = '", value, "'")
      } else {
        paste0(name, " = ", as.character(value), "")
      }
    }
  )
  paste(data_lines, collapse = "\n")
}


#' Create a collapsible single-panel accordion containing a markdown file
#'
#' @description Creates a collapsible single-panel bslib accordion displaying
#'   the contents of a markdown file, with an info icon.
#' @param title the desired title of the accordion panel
#' @param content_file the path to a markdown file
#' @param ... other arguments to accordion()
#'
#' @return a bslib::accordion html element
#'
#' @examples
#' \dontrun{
#'   # Used inside a Shiny UI function
#'   info_accordion("Instructions", "path/to/instructions.md")
#' }
#' @export
#' @importFrom bslib card card_body accordion accordion_panel
#' @importFrom bsicons bs_icon
#' @importFrom htmltools includeMarkdown tags HTML
#' @importFrom glue glue
info_accordion <- function(title = "Instructions", content_file, ...) {
  accordion(
    accordion_panel(
      title,
      icon = bs_icon("info-circle"),
      if (!is.null(content_file)) {
        tagList(
          includeMarkdown(content_file),
          tags$script(HTML(
            "
            $(document).ready(function() {
              $('a').attr('target', '_blank').attr('rel', 'noopener noreferrer');
            });
          "
          ))
        )
      } else {
        p(glue("MD file {content_file} not found."))
      },
      ...
    )
  )
}

#' Calculate rHandsontable height
#' @description Calculates a pixel height for a rHandsontable widget based on
#'   the number of data rows, clamped between a minimum and maximum value.
#' @param minHeight Integer. Minimum height in pixels. Default 200.
#' @param maxHeight Integer. Maximum height in pixels. Default 700.
#' @param defaultRowHeight Integer. Height per data row in pixels. Default 23.
#' @param defaultHeaderHeight Integer. Height of the header row in pixels. Default 25.
#' @param dataTable Data frame or tibble. The data to be displayed.
#' @return Integer. Pixel height for the table widget.
#' @noRd
rHandsontableGetHeight <- function(
  minHeight = 200,
  maxHeight = 700,
  defaultRowHeight = 23,
  defaultHeaderHeight = 25,
  dataTable
) {
  calculated_height <- (nrow(dataTable) * defaultRowHeight) +
    defaultHeaderHeight

  if (calculated_height > maxHeight) {
    calculated_height = maxHeight
  }

  final_height <- max(minHeight, calculated_height)
  final_height <- as.integer(final_height)
  final_height
}
