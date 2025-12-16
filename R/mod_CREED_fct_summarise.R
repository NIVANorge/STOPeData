# Functions that are designed to summarise unique values of various variables and return them as text strings for CREED assessments

#' @title Summarise compartments data
#' @description Creates a summary string of compartments from session data
#' @param compartmentsData The compartments dataset
#' @return Character string summarising compartments, or "Relevant data not found"
#' @export
summarise_compartments <- function(compartmentsData) {
  # Helper function to check if a dataset exists and has content
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  if (!dataset_exists(compartmentsData)) {
    return("Relevant data not found")
  }

  valid_comps <- compartmentsData[
    !is.na(compartmentsData$ENVIRON_COMPARTMENT) &
      compartmentsData$ENVIRON_COMPARTMENT != "",
  ]

  if (nrow(valid_comps) == 0) {
    return("Relevant data not found")
  }

  comp_strings <- sapply(seq_len(nrow(valid_comps)), function(i) {
    comp <- valid_comps$ENVIRON_COMPARTMENT[i]
    subcomp <- valid_comps$ENVIRON_COMPARTMENT_SUB[i]
    if (!is.na(subcomp) && subcomp != "") {
      glue("{comp} ({subcomp})")
    } else {
      comp
    }
  })

  glue(
    "{nrow(valid_comps)} compartment{if(nrow(valid_comps) > 1) 's' else ''}: ",
    "{paste(comp_strings, collapse = '; ')}"
  )
}

#' @title Summarise biota data
#' @description Creates a summary string of biota including species, tissues, and life stages
#' @param biotaData The biota dataset
#' @param SPECIES_GROUP Logical. Include species group summary?
#' @param SAMPLE_SPECIES Logical. Include sample species summary?
#' @param SAMPLE_TISSUE Logical. Include tissue type summary?
#' @param SAMPLE_SPECIES_LIFESTAGE Logical. Include life stage summary?
#' @param SAMPLE_SPECIES_GENDER Logical. Include gender summary?
#' @return Character string summarising biota, or "Relevant data not found"
#' @export
summarise_biota <- function(
  biotaData,
  SPECIES_GROUP = FALSE,
  SAMPLE_SPECIES = FALSE,
  SAMPLE_TISSUE = FALSE,
  SAMPLE_SPECIES_LIFESTAGE = FALSE,
  SAMPLE_SPECIES_GENDER = FALSE
) {
  # Helper function to check if a dataset exists and has content ----
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  if (!dataset_exists(biotaData)) {
    return("Relevant data not found")
  }

  # Build summary components ----
  summary_parts <- character(0)

  # Always include number of biota samples
  n_biota <- nrow(biotaData)
  summary_parts <- c(summary_parts, glue("{n_biota} biota samples"))

  # Conditional summaries ----
  if (SPECIES_GROUP && "SPECIES_GROUP" %in% names(biotaData)) {
    species_groups <- summarise_multiple(
      biotaData$SPECIES_GROUP,
      "Species groups"
    )
    summary_parts <- c(summary_parts, species_groups)
  }

  if (SAMPLE_SPECIES && "SAMPLE_SPECIES" %in% names(biotaData)) {
    species <- summarise_multiple(biotaData$SAMPLE_SPECIES, "Species")
    summary_parts <- c(summary_parts, species)
  }

  if (SAMPLE_TISSUE && "SAMPLE_TISSUE" %in% names(biotaData)) {
    tissues <- summarise_multiple(biotaData$SAMPLE_TISSUE, "Tissue types")
    summary_parts <- c(summary_parts, tissues)
  }

  if (
    SAMPLE_SPECIES_LIFESTAGE && "SAMPLE_SPECIES_LIFESTAGE" %in% names(biotaData)
  ) {
    lifestages <- summarise_multiple(
      biotaData$SAMPLE_SPECIES_LIFESTAGE,
      "Life stages"
    )
    summary_parts <- c(summary_parts, lifestages)
  }

  if (SAMPLE_SPECIES_GENDER && "SAMPLE_SPECIES_GENDER" %in% names(biotaData)) {
    genders <- summarise_multiple(biotaData$SAMPLE_SPECIES_GENDER, "Genders")
    summary_parts <- c(summary_parts, genders)
  }

  # Combine all parts ----
  paste(summary_parts, collapse = ". ")
}

#' @title Summarise protocols data
#' @description Creates a summary string of protocols by category
#' @param methodsData The methods/protocols dataset
#' @param categories Character vector of protocol categories to include (e.g., c("Sampling Protocol", "Analytical Protocol"))
#' @return Character string summarising protocols, or "Relevant data not found"
#' @export
summarise_protocols <- function(methodsData, categories) {
  # Helper function to check if a dataset exists and has content
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  if (!dataset_exists(methodsData)) {
    return("Relevant data not found")
  }

  protocols <- methodsData[
    methodsData$PROTOCOL_CATEGORY %in%
      categories &
      !is.na(methodsData$PROTOCOL_NAME) &
      methodsData$PROTOCOL_NAME != "",
  ]

  if (nrow(protocols) == 0) {
    return("Relevant data not found")
  }

  protocol_strings <- sapply(seq_len(nrow(protocols)), function(i) {
    name <- protocols$PROTOCOL_NAME[i]
    comment <- protocols$PROTOCOL_COMMENT[i]
    category <- protocols$PROTOCOL_CATEGORY[i]

    if (!is.na(comment) && comment != "") {
      glue("{category} - {name}: ({comment})")
    } else {
      glue("{category} - {name}")
    }
  })

  glue(
    "{nrow(protocols)} protocol{if(nrow(protocols) > 1) 's' else ''}:\n",
    "{paste(protocol_strings, collapse = '\n')}"
  )
}

#' @title Summarise sites data
#' @description Creates a summary string of sites including countries and areas
#' @param sitesData The sites dataset
#' @param COUNTRY_ISO Logical. Include country summary?
#' @param OCEAN_IHO Logical. Include area summary?
#' @param SITE_GEOGRAPHIC_FEATURE Logical. Include geographic feature summary?
#' @param SITE_GEOGRAPHIC_FEATURE_SUB Logical. Include geographic feature sub summary?
#' @param PRECISION Logical. Include coordinate precision?
#' @return Character string summarising sites, or "Relevant data not found"
#' @export
summarise_sites <- function(
  sitesData,
  COUNTRY_ISO = FALSE,
  OCEAN_IHO = FALSE,
  SITE_GEOGRAPHIC_FEATURE = FALSE,
  SITE_GEOGRAPHIC_FEATURE_SUB = FALSE,
  PRECISION = FALSE
) {
  # Helper function to check if a dataset exists and has content ----
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  if (!dataset_exists(sitesData)) {
    return("Relevant data not found")
  }

  # Build summary components ----
  summary_parts <- character(0)

  # Always include number of sites
  n_sites <- nrow(sitesData)
  summary_parts <- c(summary_parts, glue("{n_sites} sites"))

  # Conditional summaries
  if (COUNTRY_ISO && "COUNTRY_ISO" %in% names(sitesData)) {
    countries <- summarise_multiple(sitesData$COUNTRY_ISO, "Countries")
    summary_parts <- c(summary_parts, countries)
  }

  if (OCEAN_IHO && "OCEAN_IHO" %in% names(sitesData)) {
    areas <- summarise_multiple(sitesData$OCEAN_IHO, "Areas")
    summary_parts <- c(summary_parts, areas)
  }

  # Geographic features section ----
  geo_parts <- character(0)

  if (
    SITE_GEOGRAPHIC_FEATURE && "SITE_GEOGRAPHIC_FEATURE" %in% names(sitesData)
  ) {
    geo_features <- summarise_multiple(
      sitesData$SITE_GEOGRAPHIC_FEATURE,
      "Types"
    )
    geo_parts <- c(geo_parts, geo_features)
  }

  if (
    SITE_GEOGRAPHIC_FEATURE_SUB &&
      "SITE_GEOGRAPHIC_FEATURE_SUB" %in% names(sitesData)
  ) {
    geo_features_sub <- summarise_multiple(
      sitesData$SITE_GEOGRAPHIC_FEATURE_SUB,
      "Subtypes"
    )
    geo_parts <- c(geo_parts, geo_features_sub)
  }

  if (length(geo_parts) > 0) {
    summary_parts <- c(
      summary_parts,
      glue("Distinct features: {paste(geo_parts, collapse = ', ')}")
    )
  }

  # Coordinate precision ----
  if (PRECISION && all(c("LATITUDE", "LONGITUDE") %in% names(sitesData))) {
    coord_precision <- calculate_coordinate_precision(
      sitesData$LATITUDE,
      sitesData$LONGITUDE
    )
    summary_parts <- c(
      summary_parts,
      glue("Lowest coordinate precision: {coord_precision}")
    )
  }

  # Combine all parts ----
  paste(summary_parts, collapse = ". ")
}


#' @title Calculate coordinate precision
#' @description Determines the minimum decimal places across latitude and longitude
#' @param latitude Numeric vector of latitudes
#' @param longitude Numeric vector of longitudes
#' @return Integer representing minimum decimal places, or "N/A" if no valid coords
#' @export
calculate_coordinate_precision <- function(latitude, longitude) {
  # Helper to count decimal places ----
  count_decimals <- function(x) {
    # Remove NAs
    x <- x[!is.na(x)]

    if (length(x) == 0) {
      return(NA_integer_)
    }

    # Convert to character and count digits after decimal point
    x_char <- sapply(x, FUN = function(x) {
      format(x, scientific = FALSE, trim = TRUE)
    })
    decimal_parts <- sub("^[^.]*\\.?", "", x_char)

    # Count characters (handling cases with no decimal point)
    sapply(decimal_parts, function(d) {
      if (d == "" || !grepl("\\.", format(x[1], scientific = FALSE))) {
        return(0L)
      }
      nchar(d)
    })
  }

  lat_decimals <- count_decimals(latitude)
  lon_decimals <- count_decimals(longitude)

  all_decimals <- c(lat_decimals, lon_decimals)

  if (all(is.na(all_decimals)) || length(all_decimals) == 0) {
    return("N/A")
  }

  min(all_decimals, na.rm = TRUE)
}

#' @title Summarise LOD/LOQ data
#' @description Creates a summary string of limit of detection and quantification values
#' @param measurementsData The measurements dataset
#' @return Character string summarising LOD/LOQ, or "Relevant data not found"
#' @export
summarise_lod_loq <- function(measurementsData) {
  # Helper function to check if a dataset exists and has content
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  if (!dataset_exists(measurementsData)) {
    return("Relevant data not found")
  }

  loq_values <- measurementsData$LOQ_VALUE[
    !is.na(measurementsData$LOQ_VALUE)
  ]
  lod_values <- measurementsData$LOD_VALUE[
    !is.na(measurementsData$LOD_VALUE)
  ]

  info_parts <- c()
  if (length(loq_values) > 0) {
    loq_range <- glue("{min(loq_values)} to {max(loq_values)}")
    loq_unit <- measurementsData$LOQ_UNIT[
      !is.na(measurementsData$LOQ_UNIT)
    ][1]
    info_parts <- c(info_parts, glue("LOQ: {loq_range} {loq_unit}"))
  }
  if (length(lod_values) > 0) {
    lod_range <- glue("{min(lod_values)} to {max(lod_values)}")
    lod_unit <- measurementsData$LOD_UNIT[
      !is.na(measurementsData$LOD_UNIT)
    ][1]
    info_parts <- c(info_parts, glue("LOD: {lod_range} {lod_unit}"))
  }

  if (length(info_parts) > 0) {
    paste(info_parts, collapse = "; ")
  } else {
    "Relevant data not found"
  }
}

#' @title Return manual completion message
#' @description Returns a standard message for fields not collected by the app
#' @return Character string indicating manual completion required
#' @export
manual_completion_message <- function() {
  "Relevant data not collected by app. Please complete manually."
}

#' @title Summarise uncertainty and measurement comments
#' @description Creates a summary of uncertainty types and measurement comments
#' @param measurementsData The measurements dataset
#' @return Character string summarising uncertainty info, or "Relevant data not found"
#' @export
summarise_uncertainty_comments <- function(measurementsData) {
  # Helper function to check if a dataset exists and has content
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  if (!dataset_exists(measurementsData)) {
    return("Relevant data not found")
  }

  uncertainty_types <- measurementsData$UNCERTAINTY_TYPE[
    !is.na(measurementsData$UNCERTAINTY_TYPE) &
      measurementsData$UNCERTAINTY_TYPE != ""
  ]
  measurement_comments <- measurementsData$MEASUREMENT_COMMENT[
    !is.na(measurementsData$MEASUREMENT_COMMENT) &
      measurementsData$MEASUREMENT_COMMENT != ""
  ]

  parts <- c()
  if (length(uncertainty_types) > 0) {
    unique_types <- unique(uncertainty_types)
    parts <- c(
      parts,
      glue("Uncertainty types: {paste(unique_types, collapse = ', ')}")
    )
  }
  if (length(measurement_comments) > 0) {
    unique_comments <- unique(measurement_comments)
    parts <- c(
      parts,
      glue("Measurement comments: {paste(unique_comments, collapse = ', ')}")
    )
  }

  if (length(parts) > 0) {
    paste(parts, collapse = "; ")
  } else {
    "Relevant data not found"
  }
}

#' @title Summarise significant figures in numeric column
#' @description Calculates and summarises the significant figures in a numeric vector
#' @param values Numeric vector to analyze
#' @return Character string describing significant figures, or "Relevant data not found"
#' @importFrom tibble tribble
#' @export
summarise_sig_figs <- function(values) {
  if (length(values) == 0 || all(is.na(values))) {
    return("Relevant data not found")
  }

  values <- values[!is.na(values)]

  # Calculate significant figures for each value
  sig_figs <- sapply(values, function(x) {
    # Convert to character to count significant figures
    x_char <- as.character(x)
    # Remove leading zeros and decimal point for counting
    x_char <- gsub("^0+\\.", "0.", x_char)
    x_char <- gsub("\\.", "", x_char)
    x_char <- gsub("^0+", "", x_char)
    nchar(x_char)
  })

  min_sf <- min(sig_figs)
  max_sf <- max(sig_figs)

  if (min_sf == max_sf) {
    glue("Measured parameter values reported to {min_sf} significant figures")
  } else {
    glue(
      "Measured parameter values reported to {min_sf} to {max_sf} significant figures"
    )
  }
}

# Non-reactive data processing ----

#' Summarise mod_reference data into a single string
#'
#' @description Creates a formatted bibliographic reference from reference data
#' @param ref_data Reference data frame
#' @return Character string with formatted reference
#' @export
summarise_reference <- function(ref_data) {
  if (is.null(ref_data) || nrow(ref_data) == 0) {
    return("Relevant data not found")
  }

  # Basic format: Author (Year). Title. Journal/Source.
  ref_parts <- c()

  if (!is.na(ref_data$AUTHOR) && ref_data$AUTHOR != "") {
    ref_parts <- c(ref_parts, ref_data$AUTHOR)
  }

  if (!is.na(ref_data$YEAR)) {
    ref_parts <- c(ref_parts, paste0("(", ref_data$YEAR, ")"))
  }

  if (!is.na(ref_data$TITLE) && ref_data$TITLE != "") {
    ref_parts <- c(ref_parts, ref_data$TITLE)
  }

  if (
    !is.na(ref_data$PERIODICAL_JOURNAL) && ref_data$PERIODICAL_JOURNAL != ""
  ) {
    ref_parts <- c(ref_parts, ref_data$PERIODICAL_JOURNAL)
  } else if (!is.na(ref_data$PUBLISHER) && ref_data$PUBLISHER != "") {
    ref_parts <- c(ref_parts, ref_data$PUBLISHER)
  }

  if (!is.na(ref_data$DOI) && ref_data$DOI != "") {
    ref_parts <- c(ref_parts, paste0("(", ref_data$DOI, ")"))
  }

  if (length(ref_parts) > 0) {
    return(paste(ref_parts, collapse = ". "))
  } else {
    return("Reference data incomplete")
  }
}

#' Summarise Multiple Values
#'
#' @description Creates a summary string from multiple values
#' @param values Vector of values to summarise
#' @param prefix Optional prefix for output
#' @param max_display Maximum items to display before truncating
#' @return Character string summary
#' @export
summarise_multiple <- function(values, prefix = "", max_display = 10) {
  if (is.null(values) || length(values) == 0) {
    return("Relevant data not found")
  }

  unique_values <- unique(values[!is.na(values) & values != ""])

  if (length(unique_values) == 0) {
    return("Relevant data not found")
  }

  if (length(unique_values) <= max_display) {
    result <- paste(unique_values, collapse = ", ")
  } else {
    displayed <- paste(unique_values[1:max_display], collapse = ", ")
    result <- paste0(
      displayed,
      " (and ",
      length(unique_values) - max_display,
      " more)"
    )
  }

  if (prefix != "" && length(unique_values) > 1) {
    result <- paste0(prefix, " (", length(unique_values), "): ", result)
  } else if (prefix != "") {
    result <- paste0(prefix, ": ", result)
  }

  return(result)
}

#' Calculate Date Range
#'
#' @description Creates a date range string from vector of dates with count and interval
#' @param dates Vector of dates
#' @return Character string with date range, sample count, and interval in days
#' @export
summarise_date_range <- function(dates) {
  if (is.null(dates) || length(dates) == 0) {
    return("Relevant data not found")
  }

  valid_dates <- dates[!is.na(dates)]
  if (length(valid_dates) == 0) {
    return("Relevant data not found")
  }

  # Get count of unique dates
  n_unique <- length(unique(valid_dates))

  min_date <- min(valid_dates)
  max_date <- max(valid_dates)

  if (min_date == max_date) {
    return(paste0(as.character(min_date), " (n=", n_unique, ")"))
  } else {
    # Calculate interval in days
    interval_days <- as.numeric(difftime(max_date, min_date, units = "days")) |>
      round(digits = 0)

    return(paste0(
      min_date,
      " to ",
      max_date,
      " (n=",
      n_unique,
      ", ",
      interval_days,
      " days (to nearest day))"
    ))
  }
}

#' Generate Units Summary by Parameter
#'
#' @description Creates a summary of measurement units grouped by parameter name
#' @param measurement_data Measurement data frame
#' @param parameters_data Parameters data frame
#' @return Character string with units grouped by parameter
#' @export
summarise_measured_units <- function(measurement_data, parameters_data) {
  if (
    is.null(measurement_data) ||
      is.null(parameters_data) ||
      nrow(measurement_data) < 0 ||
      nrow(parameters_data) < 0
  ) {
    return("Relevant data not found")
  }

  # Join measurement data with parameter names
  merged_data <- merge(
    measurement_data,
    parameters_data,
    by = "PARAMETER_NAME",
    all.x = TRUE
  )

  # Group units by parameter
  param_units <- split(
    merged_data$MEASURED_UNIT[!is.na(merged_data$MEASURED_UNIT)],
    merged_data$PARAMETER_NAME[!is.na(merged_data$MEASURED_UNIT)]
  )

  if (length(param_units) == 0) {
    return("Relevant data not found")
  }

  # Create summary for each parameter
  param_summaries <- lapply(names(param_units), function(param) {
    unique_units <- unique(param_units[[param]])
    paste0(param, ": ", paste(unique_units, collapse = ", "))
  })

  return(paste(param_summaries, collapse = "; "))
}
