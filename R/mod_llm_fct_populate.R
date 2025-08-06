# LLM Form Population Helper Functions ----
# Functions to populate module forms from LLM extracted data

#' Populate Campaign Form from LLM Data
#'
#' @description Updates campaign module input fields with LLM extracted data
#' @param session Shiny session object
#' @param llm_campaign_data Campaign data extracted by LLM
#' @noRd
#' @importFrom shiny updateTextInput updateDateInput updateSelectInput updateTextAreaInput
populate_campaign_from_llm <- function(session, llm_campaign_data) {
  if (is.null(llm_campaign_data)) return()
  # Update campaign fields with extracted data
  if (!is.null(llm_campaign_data$campaign_name)) {
    updateTextInput(
      session,
      "CAMPAIGN_NAME",
      value = llm_campaign_data$campaign_name
    )
  }

  if (!is.null(llm_campaign_data$campaign_start_date)) {
    # Validate and parse date
    tryCatch(
      {
        start_date <- as.Date(llm_campaign_data$campaign_start_date)
        if (!is.na(start_date)) {
          updateDateInput(
            session,
            "CAMPAIGN_START_DATE",
            value = start_date
          )
        }
      },
      error = function(e) {
        print_dev(paste(
          "Invalid start date from LLM:",
          llm_campaign_data$campaign_start_date
        ))
      }
    )
  }

  if (!is.null(llm_campaign_data$campaign_end_date)) {
    tryCatch(
      {
        end_date <- as.Date(llm_campaign_data$campaign_end_date)
        if (!is.na(end_date)) {
          updateDateInput(
            session,
            "CAMPAIGN_END_DATE",
            value = end_date
          )
        }
      },
      error = function(e) {
        print_dev(paste(
          "Invalid end date from LLM:",
          llm_campaign_data$campaign_end_date
        ))
      }
    )
  }

  if (!is.null(llm_campaign_data$organisation)) {
    updateTextInput(
      session,
      "ORGANISATION",
      value = llm_campaign_data$organisation
    )
  }

  if (!is.null(llm_campaign_data$campaign_comment)) {
    updateTextAreaInput(
      session,
      "CAMPAIGN_COMMENT",
      value = llm_campaign_data$campaign_comment
    )
  }

  # Set today's date for ENTERED_DATE and current user for ENTERED_BY
  updateDateInput(session, "ENTERED_DATE", value = Sys.Date())

  print_dev("Campaign form populated from LLM data")
}

#' Populate References Form from LLM Data
#'
#' @description Updates references module input fields with LLM extracted data
#' @param session Shiny session object
#' @param llm_references_data References data extracted by LLM
#' @noRd
populate_references_from_llm <- function(session, llm_references_data) {
  if (is.null(llm_references_data)) return()
  # Determine reference type based on available fields
  ref_type <- "journal" # Default
  if (!is.null(llm_references_data$periodical_journal)) {
    ref_type <- "journal"
  } else if (!is.null(llm_references_data$publisher)) {
    ref_type <- "book"
  }

  updateSelectInput(
    session,
    "REFERENCE_TYPE",
    selected = ref_type
  )

  if (!is.null(llm_references_data$author)) {
    updateTextAreaInput(
      session,
      "AUTHOR",
      value = llm_references_data$author
    )
  }

  if (!is.null(llm_references_data$title)) {
    updateTextAreaInput(
      session,
      "TITLE",
      value = llm_references_data$title
    )
  }

  if (!is.null(llm_references_data$year)) {
    # Validate year is within acceptable range
    year <- as.numeric(llm_references_data$year)
    if (!is.na(year) && year >= 1800 && year <= 2026) {
      updateNumericInput(
        session,
        "YEAR",
        value = year
      )
    }
  }

  if (!is.null(llm_references_data$periodical_journal)) {
    updateTextInput(
      session,
      "PERIODICAL_JOURNAL",
      value = llm_references_data$periodical_journal
    )
  }

  if (!is.null(llm_references_data$volume)) {
    updateNumericInput(
      session,
      "VOLUME",
      value = as.numeric(llm_references_data$volume)
    )
  }

  if (!is.null(llm_references_data$issue)) {
    updateNumericInput(
      session,
      "ISSUE",
      value = as.numeric(llm_references_data$issue)
    )
  }

  if (!is.null(llm_references_data$publisher)) {
    updateTextInput(
      session,
      "PUBLISHER",
      value = llm_references_data$publisher
    )
  }

  if (!is.null(llm_references_data$doi)) {
    updateTextInput(
      session,
      "DOI",
      value = llm_references_data$doi
    )
  }

  # Set today's date for ACCESS_DATE
  updateDateInput(session, "ACCESS_DATE", value = Sys.Date())

  print_dev("References form populated from LLM data")
}

#' Populate Sites Data from LLM Data
#'
#' @description Creates sites data frame from LLM extracted sites array
#' @param llm_sites_data Sites array extracted by LLM
#' @return Data frame in sites module format
#' @noRd
create_sites_from_llm <- function(llm_sites_data) {
  if (is.null(llm_sites_data) || length(llm_sites_data) == 0) {
    return(data.frame())
  }

  sites_df <- data.frame()

  # Handle array of site objects
  for (i in seq_along(llm_sites_data)) {
    site <- llm_sites_data[[i]]

    # Handle the case where site might be a list or atomic vector
    if (is.list(site)) {
      site_data <- site
    } else {
      # Skip if not a proper site object
      next
    }

    # Create site row with LLM data, filling in defaults where needed
    site_row <- data.frame(
      SITE_CODE = safe_extract_field(
        site_data,
        "site_code",
        paste0("SITE_", sprintf("%03d", i))
      ),
      SITE_NAME = safe_extract_field(site_data, "site_name", ""),
      SITE_GEOGRAPHIC_FEATURE = map_geographic_feature(safe_extract_field(
        site_data,
        "site_geographic_feature",
        ""
      )),
      SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported", # LLM doesn't extract this level of detail
      SITE_COORDINATE_SYSTEM = "WGS 84", # Assume WGS84 for LLM coordinates
      LATITUDE = validate_latitude(safe_extract_field(
        site_data,
        "latitude",
        NA
      )),
      LONGITUDE = validate_longitude(safe_extract_field(
        site_data,
        "longitude",
        NA
      )),
      COUNTRY = safe_extract_field(site_data, "country", "Not reported"),
      AREA = "Not reported", # LLM doesn't typically extract area info
      ALTITUDE_VALUE = 0, # Default altitude, LLM rarely has this
      ALTITUDE_UNIT = "m",
      ENTERED_BY = "", # Will be filled by module
      ENTERED_DATE = as.character(Sys.Date()),
      SITE_COMMENT = "",
      stringsAsFactors = FALSE
    )

    sites_df <- rbind(sites_df, site_row)
  }

  print_dev(glue("Created {nrow(sites_df)} sites from LLM data"))
  return(sites_df)
}

#' Populate Parameters Data from LLM Data
#'
#' @description Creates parameters data frame from LLM extracted parameters array
#' @param llm_parameters_data Parameters array extracted by LLM
#' @return Data frame in parameters module format
#' @noRd
create_parameters_from_llm <- function(llm_parameters_data) {
  if (is.null(llm_parameters_data) || length(llm_parameters_data) == 0) {
    return(data.frame())
  }

  params_df <- data.frame()

  # Handle array of parameter objects
  for (i in seq_along(llm_parameters_data)) {
    param <- llm_parameters_data[[i]]

    # Handle the case where param might be a list or atomic vector
    if (is.list(param)) {
      param_data <- param
    } else {
      # Skip if not a proper parameter object
      next
    }

    # Map parameter type to controlled vocabulary
    param_type <- map_parameter_type(safe_extract_field(
      param_data,
      "parameter_type",
      ""
    ))

    param_row <- data.frame(
      PARAMETER_TYPE = param_type,
      PARAMETER_TYPE_SUB = infer_parameter_subtype(
        param_type,
        safe_extract_field(param_data, "parameter_name", "")
      ),
      MEASURED_TYPE = "Concentration", # Default assumption
      PARAMETER_NAME = safe_extract_field(param_data, "parameter_name", ""),
      PARAMETER_NAME_SUB = "",
      INCHIKEY_SD = "",
      PUBCHEM_CID = "",
      CAS_RN = safe_extract_field(param_data, "cas_rn", ""),
      stringsAsFactors = FALSE
    )

    params_df <- rbind(params_df, param_row)
  }

  print_dev(glue("Created {nrow(params_df)} parameters from LLM data"))
  return(params_df)
}

#' Populate Compartments Data from LLM Data
#'
#' @description Creates compartments data frame from LLM extracted compartments array
#' @param llm_compartments_data Compartments array extracted by LLM
#' @return Data frame in compartments module format
#' @noRd
create_compartments_from_llm <- function(llm_compartments_data) {
  if (is.null(llm_compartments_data) || length(llm_compartments_data) == 0) {
    return(data.frame())
  }

  comps_df <- data.frame()

  # Handle array of compartment objects
  for (i in seq_along(llm_compartments_data)) {
    comp <- llm_compartments_data[[i]]

    # Handle the case where comp might be a list or atomic vector
    if (is.list(comp)) {
      comp_data <- comp
    } else {
      # Skip if not a proper compartment object
      next
    }

    comp_row <- data.frame(
      ENVIRON_COMPARTMENT = map_compartment(safe_extract_field(
        comp_data,
        "environ_compartment",
        ""
      )),
      ENVIRON_COMPARTMENT_SUB = map_compartment_sub(safe_extract_field(
        comp_data,
        "environ_compartment_sub",
        ""
      )),
      MEASURED_CATEGORY = map_measured_category(safe_extract_field(
        comp_data,
        "measured_category",
        ""
      )),
      stringsAsFactors = FALSE
    )

    comps_df <- rbind(comps_df, comp_row)
  }

  print_dev(glue("Created {nrow(comps_df)} compartments from LLM data"))
  return(comps_df)
}

# Helper functions ----

#' Safely extract field from LLM data object
#' @param data_obj The data object (list or atomic)
#' @param field_name Name of field to extract
#' @param default Default value if field missing or null
#' @noRd
safe_extract_field <- function(data_obj, field_name, default = NA) {
  tryCatch(
    {
      if (is.list(data_obj) && field_name %in% names(data_obj)) {
        value <- data_obj[[field_name]]
        if (
          is.null(value) || is.na(value) || (is.character(value) && value == "")
        ) {
          return(default)
        }
        return(value)
      } else {
        return(default)
      }
    },
    error = function(e) {
      return(default)
    }
  )
}

# Helper mapping functions ----

#' Map LLM geographic feature to controlled vocabulary
#' @noRd
map_geographic_feature <- function(feature) {
  if (is.null(feature) || feature == "") return("Not reported")

  feature_lower <- tolower(feature)

  if (grepl("river|stream|canal", feature_lower)) return("River, stream, canal")
  if (grepl("lake|pond|reservoir", feature_lower))
    return("Lake, pond, pool, reservoir")
  if (grepl("ocean|sea", feature_lower))
    return("Ocean, sea, territorial waters")
  if (grepl("coast|fjord", feature_lower)) return("Coastal, fjord")
  if (grepl("estuary", feature_lower)) return("Estuary")
  if (grepl("ground|aquifer", feature_lower)) return("Groundwater, aquifer")
  if (grepl("crop|farm", feature_lower)) return("Cropland")
  if (grepl("forest|wood", feature_lower)) return("Woodland, forest")
  if (grepl("grass", feature_lower)) return("Grassland")

  return("Other")
}

#' Map LLM parameter type to controlled vocabulary
#' @noRd
map_parameter_type <- function(param_type) {
  if (is.null(param_type) || param_type == "") return("Stressor")

  type_lower <- tolower(param_type)

  if (grepl("stress", type_lower)) return("Stressor")
  if (grepl("quality", type_lower)) return("Quality parameter")
  if (grepl("normal", type_lower)) return("Normalization")
  if (grepl("background", type_lower)) return("Background")

  return("Stressor") # Default assumption
}

#' Infer parameter subtype based on type and name
#' @noRd
infer_parameter_subtype <- function(param_type, param_name) {
  if (param_type != "Stressor") return("Not reported")
  if (is.null(param_name) || param_name == "") return("Not reported")

  name_lower <- tolower(param_name)

  # Simple chemical classification TODO: This needs to be much better!
  metals <- c(
    "mercury",
    "lead",
    "cadmium",
    "copper",
    "zinc",
    "arsenic",
    "chromium"
  )
  if (any(sapply(metals, function(x) grepl(x, name_lower))))
    return("Metal/metalloid")

  if (grepl("ph", name_lower)) return("pH")
  if (grepl("temperature|temp", name_lower)) return("Temperature")
  if (grepl("oxygen|o2", name_lower)) return("Dissolved oxygen")

  return("Organic chemical") # Default for stressors
}

#' Map compartment to controlled vocabulary TODO: This needs to be much better!
#' @noRd
map_compartment <- function(compartment) {
  if (is.null(compartment) || compartment == "") return("Not reported")

  comp_lower <- tolower(compartment)

  if (grepl("aquatic|water", comp_lower)) return("Aquatic")
  if (grepl("atmosph|air", comp_lower)) return("Atmospheric")
  if (grepl("terrestrial|soil|land", comp_lower)) return("Terrestrial")
  if (grepl("biota|organism", comp_lower)) return("Biota")

  return("Other")
}

#' Map compartment sub to controlled vocabulary TODO: This needs to be much better!
#' @noRd
map_compartment_sub <- function(compartment_sub) {
  if (is.null(compartment_sub) || compartment_sub == "") return("Not reported")

  sub_lower <- tolower(compartment_sub)

  if (grepl("fresh", sub_lower)) return("Freshwater")
  if (grepl("marine|salt|sea", sub_lower)) return("Marine/Salt Water")
  if (grepl("brackish", sub_lower)) return("Brackish/Transitional Water")
  if (grepl("ground", sub_lower)) return("Groundwater")
  if (grepl("waste", sub_lower)) return("Wastewater")
  if (grepl("indoor", sub_lower)) return("Indoor Air")
  if (grepl("outdoor", sub_lower)) return("Outdoor Air")
  if (grepl("topsoil|a horizon", sub_lower)) return("Soil A Horizon (Topsoil)")
  if (grepl("organic|o horizon", sub_lower)) return("Soil O Horizon (Organic)")
  if (grepl("terrestrial", sub_lower)) return("Biota, Terrestrial")
  if (grepl("aquatic", sub_lower)) return("Biota, Aquatic")

  return("Other")
}

#' Map measured category to controlled vocabulary
#' @noRd
map_measured_category <- function(category) {
  if (is.null(category) || category == "") return("External")

  cat_lower <- tolower(category)

  if (grepl("external", cat_lower)) return("External")
  if (grepl("internal", cat_lower)) return("Internal")
  if (grepl("surface", cat_lower)) return("Surface")

  return("External") # Default assumption
}

#' Validate latitude value
#' @noRd
validate_latitude <- function(lat) {
  if (is.null(lat) || is.na(lat)) return(NA)
  lat_num <- as.numeric(lat)
  if (is.na(lat_num) || lat_num < -90 || lat_num > 90) return(NA)
  return(lat_num)
}

#' Validate longitude value
#' @noRd
validate_longitude <- function(lon) {
  if (is.null(lon) || is.na(lon)) return(NA)
  lon_num <- as.numeric(lon)
  if (is.na(lon_num) || lon_num < -180 || lon_num > 180) return(NA)
  return(lon_num)
}
