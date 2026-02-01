# Helper mapping functions for parameters ----

#' Generic string-to-vocabulary mapper using CSV lookup
#' @description Maps input strings to controlled vocabulary using pattern matching from CSV
#' @param input_value Character string to map
#' @param variable_name Name of the mapping variable to use from CSV
#' @param default_value Default return value if no matches found
#' @param case_sensitive Logical, whether pattern matching should be case sensitive
#' @importFrom readr read_csv
#' @noRd
# ! FORMAT-BASED
map_to_vocabulary <- function(
  input_value,
  variable_name,
  default_value,
  case_sensitive = FALSE
) {
  if (is.null(input_value) || input_value == "") {
    return(default_value)
  }

  mapping_table <- read_csv(
    system.file(
      "extdata/clean",
      "llm_vocabulary_mapping.csv",
      package = "STOPeData"
    ),
    show_col_types = FALSE
  )

  # Filter for the specific variable
  variable_mappings <- mapping_table[
    mapping_table$VARIABLE_NAME == variable_name,
  ]

  if (nrow(variable_mappings) == 0) {
    warning("No mappings found for variable: ", variable_name)
    return(default_value)
  }

  # Prepare input for matching
  search_value <- if (case_sensitive) input_value else tolower(input_value)

  # Sort by priority if column exists (lower numbers = higher priority)
  if ("priority" %in% names(variable_mappings)) {
    variable_mappings <- variable_mappings[order(variable_mappings$priority), ]
  }

  # Find first matching pattern
  for (i in seq_len(nrow(variable_mappings))) {
    pattern <- if (case_sensitive) {
      variable_mappings$pattern[i]
    } else {
      tolower(variable_mappings$pattern[i])
    }

    if (grepl(pattern, search_value)) {
      return(variable_mappings$output_value[i])
    }
  }

  return(default_value)
}

#' Map LLM parameter type to strict controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_parameter_type_strict <- function(param_type) {
  map_to_vocabulary(
    input_value = param_type,
    variable_name = "parameter_type",
    default_value = "Stressor"
  )
}

#' Map measured category to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
# ! FORMAT-BASED
map_measured_category_strict <- function(category) {
  map_to_vocabulary(
    input_value = category,
    variable_name = "measured_category",
    default_value = "External"
  )
}

#' Map compartment to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
# ! FORMAT-BASED
map_compartment_strict <- function(compartment) {
  map_to_vocabulary(
    input_value = compartment,
    variable_name = "compartment",
    default_value = "Other"
  )
}


#' Map LLM geographic feature to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in sites module
#' @noRd
# ! FORMAT-BASED
map_geographic_feature_strict <- function(feature) {
  map_to_vocabulary(
    input_value = feature,
    variable_name = "geographic_feature",
    default_value = "Other"
  )
}

# Validation functions ----

#' Validate latitude value
#' @noRd
validate_latitude <- function(lat) {
  if (is.null(lat) || is.na(lat)) {
    return(NA)
  }
  lat_num <- as.numeric(lat)
  if (is.na(lat_num) || lat_num < -90 || lat_num > 90) {
    return(NA)
  }
  return(lat_num)
}

#' Validate longitude value
#' @noRd
validate_longitude <- function(lon) {
  if (is.null(lon) || is.na(lon)) {
    return(NA)
  }
  lon_num <- as.numeric(lon)
  if (is.na(lon_num) || lon_num < -180 || lon_num > 180) {
    return(NA)
  }
  return(lon_num)
}

#' Validate Parameters Against Database
#'
#' @description Validates parameters against the chemical_parameters database
#' @param parameters_data Parameters data frame (in module format with PARAMETER_NAME, CAS_RN columns)
#' @param chemical_parameters Reference database
#' @return List with validation results and formatted text output
#' @importFrom glue glue
#' @noRd
# ! FORMAT-BASED
validate_parameters_against_database <- function(
  parameters_data,
  chemical_parameters
) {
  # Early return handling ----
  if (is.null(parameters_data) || nrow(parameters_data) == 0) {
    return(list(
      validation_text = "No parameters to validate.",
      has_warnings = FALSE
    ))
  }

  # Initialize tracking variables ----
  has_warnings <- FALSE

  # Header section ----
  header_section <- glue(
    "Parameter Database Validation Results:",
    "",
    .sep = "\n"
  )

  # Parameter validation sections ----
  parameter_sections <- c()

  for (i in seq_len(nrow(parameters_data))) {
    param_name <- parameters_data[i, "PARAMETER_NAME"]
    cas_rn <- parameters_data[[i, "CAS_RN"]]

    # Parameter header ----
    param_header <- glue(
      "Row {i}: \"{param_name}\"",
      "{if (!is.na(cas_rn) && cas_rn != '') glue(' (CAS: {cas_rn})') else ''}"
    )

    # Name validation ----
    name_validation_lines <- c()
    name_found <- FALSE

    if (!is.na(param_name) && param_name != "") {
      exact_matches <- chemical_parameters[
        tolower(chemical_parameters$PARAMETER_NAME) == tolower(param_name),
      ]

      if (nrow(exact_matches) > 0) {
        name_found <- TRUE
        name_validation_lines <- c(
          name_validation_lines,
          "  \u2713 Name found in database"
        )

        # Suggest CAS if missing
        if (
          (is.na(cas_rn) || cas_rn == "") && !is.na(exact_matches$CAS_RN[1])
        ) {
          name_validation_lines <- c(
            name_validation_lines,
            glue("  \u2192 Suggested CAS: {exact_matches$CAS_RN[1]}")
          )
        }
      } else {
        name_validation_lines <- c(
          name_validation_lines,
          "  \u26A0 Name not found in database"
        )
        has_warnings <- TRUE
      }
    } else {
      name_validation_lines <- c(
        name_validation_lines,
        "  \u26A0 No parameter name provided"
      )
      has_warnings <- TRUE
    }

    # CAS validation ----
    cas_validation_lines <- c()
    cas_found <- FALSE

    if (!is.na(cas_rn) && cas_rn != "") {
      cas_matches <- chemical_parameters |>
        filter(!is.na(CAS_RN) & CAS_RN == cas_rn)

      if (nrow(cas_matches) > 0) {
        cas_found <- TRUE
        cas_validation_lines <- c(
          cas_validation_lines,
          "  \u2713 CAS number found in database"
        )

        # Check name-CAS consistency
        if (
          name_found &&
            !tolower(param_name) %in% tolower(cas_matches$PARAMETER_NAME)
        ) {
          cas_validation_lines <- c(
            cas_validation_lines,
            glue(
              "  \u26A0 Name-CAS mismatch. CAS {cas_rn} belongs to: {cas_matches$PARAMETER_NAME[1]}"
            )
          )
          has_warnings <- TRUE
        }
      } else {
        cas_validation_lines <- c(
          cas_validation_lines,
          "  \u26A0 CAS number not found in database"
        )
        has_warnings <- TRUE
      }
    }

    # Overall parameter status ----
    overall_status_lines <- c()
    if (!name_found && !cas_found) {
      overall_status_lines <- c(
        overall_status_lines,
        "  \u2192 Recommendation: Verify parameter identity"
      )
    }

    # Combine this parameter's validation ----
    parameter_section <- glue(
      param_header,
      paste(name_validation_lines, collapse = "\n"),
      paste(cas_validation_lines, collapse = "\n"),
      paste(overall_status_lines, collapse = "\n"),
      "", # Blank line between parameters
      .sep = "\n"
    )

    parameter_sections <- c(parameter_sections, parameter_section)
  }

  # Calculate summary statistics ----
  total_params <- nrow(parameters_data)

  name_matches <- sum(
    !is.na(parameters_data$PARAMETER_NAME) &
      parameters_data$PARAMETER_NAME != "" &
      sapply(1:nrow(parameters_data), function(i) {
        param_name <- parameters_data[[i, "PARAMETER_NAME"]]
        nrow(chemical_parameters[
          tolower(chemical_parameters$PARAMETER_NAME) == tolower(param_name),
        ]) >
          0
      })
  )

  cas_matches <- sum(
    !is.na(parameters_data$CAS_RN) &
      parameters_data$CAS_RN != "" &
      sapply(1:nrow(parameters_data), function(i) {
        cas_rn <- parameters_data[i, "CAS_RN"]
        nrow(
          chemical_parameters |> filter(!is.na(CAS_RN) & CAS_RN %in% cas_rn)
        ) >
          0
      })
  )

  # Summary section ----
  summary_section <- glue(
    "Summary:",
    "  Parameters processed: {total_params}",
    "  Names found in database: {name_matches}/{total_params}",
    "  CAS numbers found in database: {cas_matches}/{total_params}",
    .sep = "\n"
  )

  # Final status section ----
  final_status_section <- if (has_warnings) {
    glue(
      "",
      "\u26A0 Some parameters need review. Check suggestions above.",
      .sep = "\n"
    )
  } else {
    glue(
      "",
      "\u2713 All parameters found in database!",
      .sep = "\n"
    )
  }

  # Combine all sections ----
  validation_text <- glue(
    header_section,
    paste(parameter_sections, collapse = ""),
    summary_section,
    final_status_section,
    .sep = "\n"
  )

  return(list(
    validation_text = validation_text,
    has_warnings = has_warnings
  ))
}

# Biota mapping functions ----

#' Map species group to controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_species_group_strict <- function(species_group) {
  map_to_vocabulary(
    input_value = species_group,
    variable_name = "species_group",
    default_value = "Other"
  )
}

#' Map tissue type to controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_tissue_type_strict <- function(tissue_type) {
  map_to_vocabulary(
    input_value = tissue_type,
    variable_name = "tissue_type",
    default_value = "Whole organism"
  )
}

#' Map life stage to controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_lifestage_strict <- function(lifestage) {
  map_to_vocabulary(
    input_value = lifestage,
    variable_name = "lifestage",
    default_value = "Adult"
  )
}

#' Map gender to controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_gender_strict <- function(gender) {
  map_to_vocabulary(
    input_value = gender,
    variable_name = "gender",
    default_value = "Not determined"
  )
}

# Methods mapping functions ----

#' Map protocol category to controlled vocabulary
#' @noRd
# ! FORMAT-BASED
map_protocol_category_strict <- function(protocol_category) {
  map_to_vocabulary(
    input_value = protocol_category,
    variable_name = "protocol_category",
    default_value = "Analytical Protocol"
  )
}
