#' utilities
#' @title isTruthy switch operator
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
#'
#' @importFrom shiny isTruthy
`%|truthy|%` <- function(first, second) {
  if (isTruthy(first)) {
    first
  } else {
    second
  }
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
#' @param first The first argument, a value we want to use only if it isTruthy
#' @param second The second argument, a safe alternative if first isn't Truthy
#'
#'
isRelevant <- function(input) {
  stopifnot(is.character(input))
  !(input %in% c("Not relevant", "Not reported"))
}

#' print content of reactiveValues object
#'
#' @param data A reactiveValues object with named variables
#' @description
#' Print a reactiveValues object, with each named variable and its value on a new
#' line
#'
#' @importFrom shiny reactiveValues
#'
#'
#' @returns a string of variable names and values
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
#' @param title the desired title of the accordion panel
#' @param content_file the path to a markdown file
#'
#' @returns a bslib::accordion html element
#'
#' @export
#' @importFrom bslib card card_body accordion accordion_panel
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

#' Abbreviate string to first n words with case formatting
#'
#' Takes a string, extracts the first n words (removing special characters),
#' and formats them according to the specified case style.
#'
#' @param string Character. The input string to abbreviate.
#' @param n_words Integer. Number of words to include in the abbreviation.
#' @param case Character. Case style for the output. One of:
#'   \itemize{
#'     \item "lower" - all lowercase, no separator (e.g., "dogsandcats")
#'     \item "upper" - all uppercase, no separator (e.g., "DOGSANDCATS")
#'     \item "sentence" - sentence case, no separator (e.g., "Dogsandcats")
#'     \item "snake" - lowercase with underscores (e.g., "dogs_and_cats")
#'     \item "title" - title case, no separator (e.g., "DogsAndCats")
#'     \item "screamingsnake" - uppercase with underscores (e.g., "DOGS_AND_CATS")
#'     \item "camel" - camel case (e.g., "dogsAndCats")
#'   }
#'
#' @return Character. The abbreviated and formatted string.
#'
#' @importFrom stringr str_to_title str_split
#'
#' @export
abbreviate_string <- function(
  string,
  n_words,
  case = c(
    "lower",
    "upper",
    "sentence",
    "snake",
    "title",
    "screamingsnake",
    "camel"
  )
) {
  case <- match.arg(case)

  stopifnot(
    is.character(as.character(string)),
    is.integer(as.integer(n_words))
  )

  n_words <- as.integer(n_words)
  string <- as.character(string)

  words <- strsplit(gsub("[^A-Za-z0-9 ]", " ", string), "\\s+")
  words <- words[[nchar(words) > 0]] # Remove empty strings

  # Take first n words
  selected_words <- head(words, n_words)

  # Apply case transformation
  result <- switch(
    case,
    "lower" = paste(tolower(selected_words), collapse = ""),
    "upper" = paste(toupper(selected_words), collapse = ""),
    "sentence" = {
      words_lower <- tolower(selected_words)
      words_lower[1] <- paste0(
        toupper(substr(words_lower[1], 1, 1)),
        substr(words_lower[1], 2, nchar(words_lower[1]))
      )
      paste(words_lower, collapse = "")
    },
    "snake" = paste(tolower(selected_words), collapse = "_"),
    "title" = paste(str_to_title(selected_words), collapse = ""),
    "screamingsnake" = paste(toupper(selected_words), collapse = "_"),
    "camel" = {
      camel_words <- selected_words
      # Capitalise first letter of each word
      camel_words <- paste0(
        toupper(substr(camel_words, 1, 1)),
        tolower(substr(camel_words, 2, nchar(camel_words)))
      )
      # Make first word lowercase
      camel_words[1] <- tolower(camel_words[1])
      paste0(camel_words, collapse = "")
    }
  )

  return(result)
}
