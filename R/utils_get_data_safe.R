# Get Session Data Safely ----
#' Safely retrieve data from session reactive values
#'
#' @description Safely retrieves a column or variable from a named table stored
#'   in session reactive values, returning a fallback value if the table or
#'   variable is not available.
#' @param session Shiny session object containing userData$reactiveValues
#' @param table_name Character string: name of the table in reactiveValues
#' @param variable_name Character string: name of the variable/column to retrieve
#' @param fallback_value Character string: value to return if conditions not met (default: "Unknown")
#' @return Value from the specified table/variable if available, fallback_value otherwise
#' @examples
#' \dontrun{
#'   # session is the Shiny session object from the module server function
#'   site_codes <- get_session_data_safe(session, "sitesData", "SITE_CODE")
#'   entered_by <- get_session_data_safe(session, "campaignData", "ENTERED_BY",
#'                                       fallback_value = "Unknown user")
#' }
#' @export
get_session_data_safe <- function(
  session,
  table_name,
  variable_name,
  fallback_value = "Unknown"
) {
  # Extract table data from session reactive values
  table_data <- session$userData$reactiveValues[[table_name]]

  # Check if table exists and is not NULL
  if (is.null(table_data)) {
    return(fallback_value)
  }

  # Check if data exists and has the requested variable column with content
  if (nrow(table_data) > 0 & length(table_data[[variable_name]]) > 0) {
    return(table_data[[variable_name]])
  } else {
    return(fallback_value)
  }
}
