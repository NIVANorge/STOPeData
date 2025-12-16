# Sample Helper Functions ----
# Utility functions for the samples module

#' Update Sites Selectize Input ----
#' @param session Shiny session
#' @param sites_data tibble with SITE_CODE and SITE_NAME columns
#' @importFrom stats setNames
#' @noRd
update_sites_selectize <- function(session, sites_data) {
  if (nrow(sites_data) == 0) {
    choices <- character(0)
    placeholder <- "No sites available - add sites first"
  } else {
    # Validate expected columns exist
    if (!all(c("SITE_CODE", "SITE_NAME") %in% names(sites_data))) {
      warning("Sites data missing expected columns: SITE_CODE, SITE_NAME")
      choices <- character(0)
      placeholder <- "Invalid sites data format"
    } else {
      choices <- setNames(
        sites_data$SITE_CODE,
        paste(sites_data$SITE_CODE, "-", sites_data$SITE_NAME)
      )
      placeholder <- "Select sites..."
    }
  }

  updatePickerInput(
    session,
    "sites_select",
    choices = choices,
    options = list(placeholder = placeholder)
  )
}

#' Update Parameters Selectize Input ----
#' @param session Shiny session
#' @param parameters_data Data frame with PARAMETER_NAME and PARAMETER_TYPE columns
#' @importFrom stats setNames
#' @noRd
update_parameters_selectize <- function(session, parameters_data) {
  if (nrow(parameters_data) == 0) {
    choices <- character(0)
    placeholder <- "No parameters available - add parameters first"
  } else {
    # Validate expected columns exist
    if (
      !all(c("PARAMETER_NAME", "PARAMETER_TYPE") %in% names(parameters_data))
    ) {
      warning(
        "Parameters data missing expected columns: PARAMETER_NAME, PARAMETER_TYPE"
      )
      choices <- character(0)
      placeholder <- "Invalid parameters data format"
    } else {
      choices <- setNames(
        parameters_data$PARAMETER_NAME,
        paste(
          parameters_data$PARAMETER_NAME,
          paste0("(", parameters_data$PARAMETER_TYPE, ")")
        )
      )
      placeholder <- "Select parameters..."
    }
  }

  updatePickerInput(
    session,
    "parameters_select",
    choices = choices,
    options = list(placeholder = placeholder)
  )
}

#' Update Compartments Selectize Input ----
#' @param session Shiny session
#' @param compartments_data Data frame with compartment columns
#' @importFrom stats setNames
#' @noRd
update_compartments_selectize <- function(session, compartments_data) {
  if (nrow(compartments_data) == 0) {
    choices <- character(0)
    placeholder <- "No compartments available - add compartments first"
  } else {
    # Validate expected columns exist
    expected_cols <- c(
      "ENVIRON_COMPARTMENT",
      "ENVIRON_COMPARTMENT_SUB",
      "MEASURED_CATEGORY"
    )
    if (!all(expected_cols %in% names(compartments_data))) {
      warning(
        "Compartments data missing expected columns:",
        paste(setdiff(expected_cols, names(compartments_data)), collapse = ", ")
      )
      choices <- character(0)
      placeholder <- "Invalid compartments data format"
    } else {
      # Create merged values for selectize (keep existing format)
      comp_values <- paste(
        compartments_data$ENVIRON_COMPARTMENT,
        compartments_data$ENVIRON_COMPARTMENT_SUB,
        sep = " | "
      )
      comp_labels <- paste(
        compartments_data$ENVIRON_COMPARTMENT,
        "→",
        compartments_data$ENVIRON_COMPARTMENT_SUB,
        "→",
        compartments_data$MEASURED_CATEGORY
      )
      choices <- setNames(comp_values, comp_labels)
      placeholder <- "Select compartments..."
    }
  }

  updatePickerInput(
    session,
    "compartments_select",
    choices = choices,
    options = list(placeholder = placeholder)
  )
}

#' Parse Compartment Selections ----
#' @description Parse merged compartment selections back to individual components
#' @param compartment_selections Vector of merged compartment selections like "Aquatic | Freshwater"
#' @param available_compartments Available compartments data frame
#' @return Data frame with separate ENVIRON_COMPARTMENT, ENVIRON_COMPARTMENT_SUB, MEASURED_CATEGORY columns
#' @noRd
parse_compartment_selections <- function(
  compartment_selections,
  available_compartments
) {
  if (is.null(compartment_selections) || length(compartment_selections) == 0) {
    return(tibble::tibble(
      ENVIRON_COMPARTMENT = character(0),
      ENVIRON_COMPARTMENT_SUB = character(0),
      MEASURED_CATEGORY = character(0)
    ))
  }

  parsed <- tibble::tibble()

  for (selection in compartment_selections) {
    # Parse "Aquatic | Freshwater" format
    parts <- strsplit(selection, " | ", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      # Find matching row in available_compartments
      match_rows <- available_compartments[
        available_compartments$ENVIRON_COMPARTMENT == parts[1] &
          available_compartments$ENVIRON_COMPARTMENT_SUB == parts[2],
      ]
      if (nrow(match_rows) > 0) {
        # Add all matching combinations (in case there are multiple MEASURED_CATEGORY values)
        parsed <- rbind(parsed, match_rows)
      } else {
        warning(paste("Could not find compartment combination:", selection))
      }
    } else {
      warning(paste("Invalid compartment selection format:", selection))
    }
  }

  return(parsed)
}

#' Generate Sample ID with Components ----
#' @param site_code Site code (vectorized)
#' @param parameter_name Parameter name (vectorized)
#' @param environ_compartment Environmental compartment (vectorized)
#' @param environ_compartment_sub Environmental sub-compartment (vectorized)
#' @param date Sampling date (vectorized)
#' @param subsample subsample
#' @importFrom stringr str_to_title str_remove_all
#' @import eDataDRF
#' @noRd
generate_sample_id_with_components <- function(
  site_code,
  parameter_name,
  environ_compartment,
  environ_compartment_sub,
  date,
  subsample = 1
) {
  # Create abbreviated versions for ID (vectorized)
  param_abbrev <- substr(gsub("[^A-Za-z0-9]", "", parameter_name), 1, 8)
  comp_abbrev <- substr(
    gsub("[^A-Za-z0-9]", "", environ_compartment_sub),
    1,
    12
  )
  date_abbrev <- gsub("-", "-", date)

  base_id <- glue("{site_code}-{param_abbrev}-{comp_abbrev}-{date_abbrev}")

  # Vectorized replicate
  # Subsamples will generally be text, so let's abbreviate them a bit

  subsample_suffix <- sapply(
    subsample,
    function(x) abbreviate_string(string = x, n_words = 3, case = "title")
  )
  paste0(base_id, "-R-", subsample_suffix)
}

#' Check if Sample Combination with Components Exists ----
#' @param existing_data Existing samples data frame
#' @param site Site code
#' @param parameter Parameter name
#' @param environ_compartment Environmental compartment
#' @param environ_compartment_sub Environmental sub-compartment
#' @param measured_category Measured category
#' @param date Sampling date
#' @param subsample subsample identifiers
#' @noRd
combination_exists_with_components <- function(
  existing_data,
  site,
  parameter,
  environ_compartment,
  environ_compartment_sub,
  measured_category,
  date,
  subsample
) {
  if (nrow(existing_data) == 0) {
    return(FALSE)
  }

  any(
    existing_data$SITE_CODE == site &
      existing_data$PARAMETER_NAME == parameter &
      existing_data$ENVIRON_COMPARTMENT == environ_compartment &
      existing_data$ENVIRON_COMPARTMENT_SUB == environ_compartment_sub &
      existing_data$MEASURED_CATEGORY == measured_category &
      existing_data$SAMPLING_DATE == as.character(date) &
      existing_data$SUBSAMPLE == subsample
  )
}

#' Create Sample Combinations with Separate Compartment Components ----
#' @description
#' Create all valid combinations of sites, parameters, compartments, dates, and subsamples,
#' except for those already found in existing_data. Now handles separate compartment columns.
#' Fixed to handle Date objects properly and avoid class-stripping in loops.
#'
#' @param sites Vector of site codes
#' @param parameters Vector of parameter names
#' @param compartment_selections Vector of merged compartment selections like "Aquatic | Freshwater"
#' @param dates Vector of sampling dates
#' @param subsample commas-separated string used to identify subsamples
#' @param existing_data Existing samples data frame
#' @param available_compartments Available compartments data frame for parsing
#' @param available_sites Available sites data frame for lookup (optional)
#' @param available_parameters Available parameters data frame for lookup (optional)
#' @importFrom stats setNames
#' @importFrom purrr map_dfr map map_dbl
#' @importFrom tidyr expand_grid
#' @noRd
create_sample_combinations <- function(
  sites,
  parameters,
  compartment_selections,
  dates,
  subsamples = 1,
  existing_data,
  available_compartments,
  available_sites = NULL,
  available_parameters = NULL
) {
  stopifnot(
    isTruthy(sites),
    isTruthy(parameters),
    isTruthy(compartment_selections),
    isTruthy(dates)
  )

  # Parse compartment selections back to individual components
  parsed_compartments <- parse_compartment_selections(
    compartment_selections,
    available_compartments
  )

  if (nrow(parsed_compartments) == 0) {
    warning("No valid compartment combinations found")
    return(list(combinations = tibble::tibble(), skipped = 0))
  }

  # Process dates using functional approach to avoid class-stripping
  all_combinations_list <- map(dates, function(date) {
    # Ensure we have a proper Date object and convert to character for storage
    if (inherits(date, "Date")) {
      date_char <- as.character(date)
    } else {
      # Handle case where date might be numeric (from for loop stripping)
      date_obj <- as.Date(date, origin = "1970-01-01")
      date_char <- as.character(date_obj)
    }

    # Create base combinations for this date
    base_combinations <- expand_grid(
      SITE_CODE = sites,
      PARAMETER_NAME = parameters,
      SAMPLING_DATE = date_char
    )

    # Split subsamples cs-string into a vector, if possible
    # do some fairly careful checking of subsamples to see if it's empty
    # todo: we run this exact same code outside the function as well. could be consolidated.
    subsamples <- subsamples |> as.character()
    subsamples <- if (
      length(trimws(strsplit(subsamples, split = ",")[[1]])) > 1
    ) {
      trimws(strsplit(subsamples, split = ",")[[1]])
    } else {
      "1"
    }

    # Process each base combination with compartments and subsamples
    date_combinations <- tibble::tibble()
    skipped_for_date <- 0

    for (i in 1:nrow(base_combinations)) {
      for (j in 1:nrow(parsed_compartments)) {
        base_combo <- base_combinations[i, ]
        compartment_combo <- parsed_compartments[j, ]

        # Add subsamples for each combination
        for (k in 1:length(subsamples)) {
          combination <- tibble::tibble(
            SITE_CODE = base_combo$SITE_CODE,
            PARAMETER_NAME = base_combo$PARAMETER_NAME,
            ENVIRON_COMPARTMENT = compartment_combo$ENVIRON_COMPARTMENT,
            ENVIRON_COMPARTMENT_SUB = compartment_combo$ENVIRON_COMPARTMENT_SUB,
            MEASURED_CATEGORY = compartment_combo$MEASURED_CATEGORY,
            SAMPLING_DATE = base_combo$SAMPLING_DATE, #
            SUBSAMPLE = subsamples[[k]]
          )

          # Check if this exact combination (including subsample) exists
          if (
            !combination_exists_with_components(
              existing_data,
              combination$SITE_CODE,
              combination$PARAMETER_NAME,
              combination$ENVIRON_COMPARTMENT,
              combination$ENVIRON_COMPARTMENT_SUB,
              combination$MEASURED_CATEGORY,
              combination$SAMPLING_DATE,
              subsamples[[k]]
            )
          ) {
            date_combinations <- rbind(date_combinations, combination)
          } else {
            skipped_for_date <- skipped_for_date + 1
          }
        }
      }
    }

    # Return list with combinations and skipped count for this date
    return(list(combinations = date_combinations, skipped = skipped_for_date))
  })

  # Combine results from all dates
  all_combinations <- do.call(
    rbind,
    map(all_combinations_list, ~ .$combinations)
  )
  total_skipped <- sum(map_dbl(all_combinations_list, ~ .$skipped))

  if (nrow(all_combinations) > 0) {
    # Populate SITE_NAME from available_sites if provided
    if (!is.null(available_sites) && "SITE_NAME" %in% names(available_sites)) {
      site_lookup <- setNames(
        available_sites$SITE_NAME,
        available_sites$SITE_CODE
      )
      all_combinations$SITE_NAME <- site_lookup[all_combinations$SITE_CODE]
      all_combinations$SITE_NAME[is.na(all_combinations$SITE_NAME)] <- ""
    } else {
      all_combinations$SITE_NAME <- ""
    }

    # Populate PARAMETER_TYPE from available_parameters if provided
    if (
      !is.null(available_parameters) &&
        all(
          c("PARAMETER_NAME", "PARAMETER_TYPE") %in% names(available_parameters)
        )
    ) {
      param_lookup <- setNames(
        available_parameters$PARAMETER_TYPE,
        available_parameters$PARAMETER_NAME
      )
      all_combinations$PARAMETER_TYPE <- param_lookup[
        all_combinations$PARAMETER_NAME
      ]
      all_combinations$PARAMETER_TYPE[is.na(
        all_combinations$PARAMETER_TYPE
      )] <- ""
    } else {
      all_combinations$PARAMETER_TYPE <- ""
    }

    # Create SUBSAMPLE ID
    # Helper function to create clean abbreviations ----
    create_abbrev <- function(text, max_length) {
      substr(gsub("[^A-Za-z0-9]", "", text), 1, max_length)
    }

    # Create SUBSAMPLE_ID using glue ----
    all_combinations$SUBSAMPLE_ID <- glue(
      "{all_combinations$SITE_CODE}",
      "_{create_abbrev(all_combinations$PARAMETER_NAME, 8)}",
      "_{create_abbrev(all_combinations$ENVIRON_COMPARTMENT, 6)}",
      "_{create_abbrev(all_combinations$ENVIRON_COMPARTMENT_SUB, 6)}",
      "_{gsub('-', '', all_combinations$SAMPLING_DATE)}",
      "{glue('_R{sprintf(\"%02s\", all_combinations$SUBSAMPLE)}')}"
    )

    # Generate sample IDs using vectorized function
    all_combinations$SAMPLE_ID <- generate_sample_id_with_components(
      all_combinations$SITE_CODE,
      all_combinations$PARAMETER_NAME,
      all_combinations$ENVIRON_COMPARTMENT,
      all_combinations$ENVIRON_COMPARTMENT_SUB,
      all_combinations$SAMPLING_DATE,
      all_combinations$SUBSAMPLE
    )

    # Reorder columns to match expected structure
    all_combinations <- all_combinations[, c(
      "SITE_CODE",
      "SITE_NAME",
      "PARAMETER_NAME",
      "PARAMETER_TYPE",
      "ENVIRON_COMPARTMENT",
      "ENVIRON_COMPARTMENT_SUB",
      "MEASURED_CATEGORY",
      "SAMPLING_DATE",
      "SUBSAMPLE",
      "SAMPLE_ID"
    )]
  }

  return(list(combinations = all_combinations, skipped = total_skipped))
}

#' Update Combination Preview ----
#' @param sites_count Number of selected sites
#' @param params_count Number of selected parameters
#' @param comps_count Number of selected compartments
#' @param dates_count Number of selected dates
#' @param subsamples_count Number of subsamples per combination
#' @noRd
update_combination_preview <- function(
  sites_count,
  params_count,
  comps_count,
  dates_count,
  subsamples_count = 1
) {
  base_combinations <- sites_count * params_count * comps_count * dates_count
  total_samples <- base_combinations * subsamples_count

  div(
    strong("Preview: "),
    sprintf(
      "%d sites × %d parameters × %d compartments × %d dates × %d subsamples = %d total samples",
      sites_count,
      params_count,
      comps_count,
      dates_count,
      subsamples_count,
      total_samples
    )
  )
}

# Dummy data for standalone testing ----

#' dummy_sites ----
#' @noRd
dummy_sites <- tibble::tibble(
  SITE_CODE = c("SITE_001", "SITE_002", "SITE_003"),
  SITE_NAME = c("River Site A", "Lake Site B", "Coastal Site C")
)

#' dummy_parameters ----
#' @noRd
dummy_parameters <- tibble::tibble(
  PARAMETER_NAME = c("Copper", "Lead", "pH", "Dissolved oxygen"),
  PARAMETER_TYPE = c(
    "Stressor",
    "Stressor",
    "Quality parameter",
    "Quality parameter"
  )
)

#' dummy_compartments ----
#' @noRd
dummy_compartments <- tibble::tibble(
  ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Terrestrial"),
  ENVIRON_COMPARTMENT_SUB = c(
    "Freshwater",
    "Marine/Salt Water",
    "Soil A Horizon (Topsoil)"
  ),
  MEASURED_CATEGORY = c("External", "External", "External")
)
