#' Get parameter names for a specific parameter type
#'
#' @param param_type Character string specifying the parameter type
#' @param dummy_parameters Dataframe containing base parameters
#' @param session_parameters Optional list containing session-specific parameters
#'
#' @return Character vector of parameter names
#' @importFrom dplyr filter pull
#' @import eDataDRF
#' @export
get_parameters_of_types <- function(
  param_type,
  dummy_parameters,
  session_parameters = NULL
) {
  # Get base parameters from dummy_parameters dataframe
  base_params <- dummy_parameters |>
    filter(PARAMETER_TYPE == param_type) |>
    pull(PARAMETER_NAME)

  # Get session parameters if provided
  session_params <- if (!is.null(session_parameters)) {
    names(session_parameters[[param_type]] %||% list())
  } else {
    character(0)
  }

  all_params <- c("-- New Parameter --", base_params, session_params)
  return(all_params)
}

#' Create new blank parameter row
#'
#' @param param_type Character string specifying the parameter type
#' @param entered_by Character string specifying who entered the parameter
#'
#' @return tibble with blank parameter template
#' @import eDataDRF
#' @export
#' @seealso \code{\link{initialise_parameters_tibble}}
create_new_parameter <- function(param_type, entered_by) {
  template <- initialise_parameters_tibble()

  # Add single row with specified values
  template |>
    add_row(
      PARAMETER_TYPE = param_type,
      PARAMETER_TYPE_SUB = "",
      MEASURED_TYPE = "Concentration",
      PARAMETER_NAME = "",
      PARAMETER_NAME_SUB = "",
      INCHIKEY_SD = "",
      PUBCHEM_CID = NA_integer_,
      CAS_RN = "",
      ENTERED_BY = entered_by,
      PARAMETER_COMMENT = ""
    )
}

#' Create parameter row from existing parameter data
#'
#' @param param_type Character string specifying the parameter type
#' @param param_name Character string specifying the parameter name
#' @param dummy_parameters Dataframe containing base parameters
#' @param session_parameters Optional dataframe containing session-specific parameters
#'
#' @return tibble with parameter information or NULL if not found
#' @importFrom dplyr filter slice add_row
#' @import eDataDRF
#' @export
#' @seealso \code{\link{initialise_parameters_tibble}}
create_existing_parameter <- function(
  param_type,
  param_name,
  dummy_parameters,
  session_parameters = NULL
) {
  # Check dummy parameters first
  param_row <- dummy_parameters |>
    filter(PARAMETER_NAME == param_name)

  # If not found in dummy_parameters, check session_parameters
  if (nrow(param_row) == 0 && !is.null(session_parameters)) {
    param_row <- session_parameters |>
      filter(PARAMETER_TYPE == param_type, PARAMETER_NAME == param_name)
  }

  # Return NULL if parameter not found
  if (nrow(param_row) == 0) {
    return(NULL)
  }

  # Extract first matching row
  param_data <- param_row |> slice(1)

  # Start with template to ensure all columns exist
  template <- initialise_parameters_tibble()

  template |>
    add_row(
      PARAMETER_TYPE = param_type,
      PARAMETER_TYPE_SUB = param_data$PARAMETER_TYPE_SUB %||% "",
      MEASURED_TYPE = param_data$MEASURED_TYPE %||% "Concentration",
      PARAMETER_NAME = param_data$PARAMETER_NAME %||% param_name,
      PARAMETER_NAME_SUB = param_data$PARAMETER_NAME_SUB %||% "",
      INCHIKEY_SD = param_data$INCHIKEY_SD %||% "",
      PUBCHEM_CID = param_data$PUBCHEM_CID |> as.integer() %||% NA_integer_,
      CAS_RN = param_data$CAS_RN %||% "",
      ENTERED_BY = param_data$ENTERED_BY %||% "Not found",
      PARAMETER_COMMENT = ""
    )
}

#' Get parameter names filtered by type and optionally subtype
#'
#' @param param_type Character string specifying the parameter type
#' @param param_subtype Character string specifying the parameter subtype (optional)
#' @param dummy_parameters Dataframe containing base parameters
#' @param session_parameters Optional list containing session-specific parameters
#'
#' @return Character vector of parameter names
#' @importFrom dplyr filter pull
#' @export
get_parameters_filtered <- function(
  param_type,
  param_subtype = "Show all",
  dummy_parameters,
  session_parameters = NULL
) {
  # Filter base parameters by type
  base_params_filtered <- dummy_parameters |>
    filter(PARAMETER_TYPE == param_type)

  # Further filter by subtype if not "Show all"
  if (param_subtype != "Show all") {
    base_params_filtered <- base_params_filtered |>
      filter(PARAMETER_TYPE_SUB == param_subtype)
  }

  # Handle empty results gracefully
  base_params <- if (nrow(base_params_filtered) > 0) {
    base_params_filtered |> pull(PARAMETER_NAME)
  } else {
    character(0)
  }

  # Get session parameters if provided
  session_params <- if (
    !is.null(session_parameters) && !is.null(session_parameters[[param_type]])
  ) {
    session_type_params <- session_parameters[[param_type]]

    if (param_subtype != "Show all" && length(session_type_params) > 0) {
      # Filter session parameters by subtype - handle case where list might be empty
      session_params_filtered <- session_type_params[
        sapply(session_type_params, function(x) {
          # Handle case where PARAMETER_TYPE_SUB might not exist
          subtype <- x$PARAMETER_TYPE_SUB %||% ""
          subtype == param_subtype
        })
      ]
      names(session_params_filtered %||% list())
    } else {
      names(session_type_params %||% list())
    }
  } else {
    character(0)
  }

  all_params <- c(base_params, session_params)
  return(all_params)
}
