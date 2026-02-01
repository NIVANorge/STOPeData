# LLM Form Population Helper Functions
# Functions to populate module forms from LLM extracted data

# mod_campaign ----

#' Populate Campaign Form from LLM Data
#'
#' @description Updates campaign module input fields with LLM extracted data
#' @param session Shiny session object
#' @param llm_campaign_data Campaign data extracted by LLM
#' @noRd
#' @importFrom shiny updateTextInput updateDateInput updateSelectInput updateTextAreaInput
# ! FORMAT-BASED
# TODO: Rewrite to account for the fact that it now is used for save imports as well as the LLM
# Should probably accept tibbles as an input too
populate_campaign_from_llm <- function(session, llm_campaign_data) {
  if (is.null(llm_campaign_data)) {
    return()
  }
  # Update campaign fields with extracted data
  if (!is.null(llm_campaign_data$campaign_name)) {
    updateTextInput(
      session,
      "CAMPAIGN_NAME",
      value = llm_campaign_data$campaign_name
    )
  }

  if (!is.null(llm_campaign_data$campaign_name_short)) {
    updateTextInput(
      session,
      "CAMPAIGN_NAME_SHORT",
      value = llm_campaign_data$campaign_name_short
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

# mod_references ----

#' Populate References Form from LLM Data
#'
#' @description Updates references module input fields with LLM extracted data
#' @param session Shiny session object
#' @param llm_references_data References data extracted by LLM
#' @importFrom dplyr rename_with
#' @noRd
# ! FORMAT-BASED
populate_references_from_llm <- function(session, llm_references_data) {
  if (is.null(llm_references_data) || nrow(llm_references_data) < 1) {
    return()
  }

  # make the tibble use upper case titles if it doesn't already
  llm_references_data <- llm_references_data |>
    rename_with(.fn = function(x) {
      toupper(x)
    })

  # Determine reference type based on available fields
  ref_type <- "journal" # Default
  if (!is.null(llm_references_data$PERIODICAL_JOURNAL)) {
    ref_type <- "journal"
  } else if (!is.null(llm_references_data$PUBLISHER)) {
    ref_type <- "book"
  }

  updateSelectInput(
    session,
    "REFERENCE_TYPE",
    selected = ref_type
  )

  # doing this here is a little crude but currently the above
  # observer isn't triggered by llm extract

  reference_id <- if (
    !is.null(llm_references_data$REFERENCE_ID) &&
      nchar(llm_references_data$REFERENCE_ID) > 0
  ) {
    llm_references_data$REFERENCE_ID
  } else {
    tryCatch(
      {
        generate_reference_id(
          date = llm_references_data$YEAR,
          author = llm_references_data$AUTHOR,
          title = llm_references_data$TITLE
        )
      },
      error = function(e) {
        showNotification(
          paste0("Error generating reference ID: ", e$message),
          type = "error"
        )
      }
    )
  }

  if (!is.null(reference_id)) {
    updateTextInput(
      session,
      "REFERENCE_ID_DISPLAY",
      value = reference_id
    )
  }

  if (!is.null(llm_references_data$AUTHOR)) {
    updateTextAreaInput(
      session,
      "AUTHOR",
      value = llm_references_data$AUTHOR
    )
  }

  if (!is.null(llm_references_data$TITLE)) {
    updateTextAreaInput(
      session,
      "TITLE",
      value = llm_references_data$TITLE
    )
  }

  if (!is.null(llm_references_data$YEAR)) {
    # Validate year is within acceptable range
    year <- as.numeric(llm_references_data$YEAR)
    if (!is.na(year) && year >= 1800 && year <= 2026) {
      updateNumericInput(
        session,
        "YEAR",
        value = year
      )
    }
  }

  if (!is.null(llm_references_data$PERIODICAL_JOURNAL)) {
    updateTextInput(
      session,
      "PERIODICAL_JOURNAL",
      value = llm_references_data$PERIODICAL_JOURNAL
    )
  }

  if (!is.null(llm_references_data$VOLUME)) {
    updateNumericInput(
      session,
      "VOLUME",
      value = as.numeric(llm_references_data$VOLUME)
    )
  }

  if (!is.null(llm_references_data$ISSUE)) {
    updateNumericInput(
      session,
      "ISSUE",
      value = as.numeric(llm_references_data$ISSUE)
    )
  }

  if (!is.null(llm_references_data$PUBLISHER)) {
    updateTextInput(
      session,
      "PUBLISHER",
      value = llm_references_data$PUBLISHER
    )
  }

  if (!is.null(llm_references_data$DOI)) {
    updateTextInput(
      session,
      "DOI",
      value = llm_references_data$DOI
    )
  }

  # Set today's date for ACCESS_DATE
  updateDateInput(session, "ACCESS_DATE", value = Sys.Date())

  print_dev("References form populated from LLM data")
}

# mod_sites ----

#' Populate Sites Data from LLM Data
#'
#' @description Creates sites data frame from LLM extracted sites data frame
#' @param llm_sites_data Sites data frame extracted by LLM
#' @param llm_campaign_data Campaign data frame extracted by LLM (used for generating sites codes)
#' @param session get session object for ENTERED_BY
#' @return Data frame in sites module format
#' @noRd
# ! FORMAT-BASED
create_sites_from_llm <- function(
  llm_sites_data,
  llm_campaign_data,
  session
) {
  if (is.null(llm_sites_data) || nrow(llm_sites_data) == 0) {
    return(tibble(NULL))
  }

  sites_tibble <- tibble()
  # Process each row of the sites data frame
  for (i in seq_len(nrow(llm_sites_data))) {
    site <- llm_sites_data[i, ]

    # Create site row with LLM data, filling in defaults where needed
    site_row <- tibble(
      # add llm_campaign_data$campaign_name to start of site code
      SITE_CODE = paste0(
        llm_campaign_data$campaign_name_short,
        "_",
        safe_extract_field(
          site,
          "site_code",
          paste0("SITE_", sprintf("%03d", i))
        )
      ),
      SITE_NAME = safe_extract_field(site, "site_name", ""),
      SITE_GEOGRAPHIC_FEATURE = map_geographic_feature_strict(safe_extract_field(
        site,
        "site_geographic_feature",
        ""
      )),
      SITE_GEOGRAPHIC_FEATURE_SUB = safe_extract_field(
        site,
        "site_geographic_feature_sub",
        ""
      ),
      SITE_COORDINATE_SYSTEM = "WGS 84", # Assume WGS84 for LLM coordinates
      LATITUDE = validate_latitude(safe_extract_field(
        site,
        "latitude",
        NA
      )),
      LONGITUDE = validate_longitude(safe_extract_field(
        site,
        "longitude",
        NA
      )),
      COUNTRY_ISO = safe_extract_field(site, "country_iso", "Not reported"),
      OCEAN_IHO = safe_extract_field(site, "ocean_iho", "Not reported"),
      ALTITUDE_VALUE = 0, # not currently extracted
      ALTITUDE_UNIT = "m",
      ENTERED_BY = session$userData$reactiveValues$ENTERED_BY %|truthy|% "",
      ENTERED_DATE = as.character(Sys.Date()),
      SITE_COMMENT = safe_extract_field(site, "site_comment", "")
    )

    sites_tibble <- rbind(sites_tibble, site_row)
  }

  print_dev(glue("Created {nrow(sites_tibble)} sites from LLM data"))
  return(sites_tibble)
}


# mod_parameters ----

#' Create Parameters Data from LLM with Database Lookup
#'
#' @description Creates parameters data frame from LLM extracted parameters with database lookup
#' @param llm_parameters_data Parameters data frame extracted by LLM
#' @param chemical_parameters Reference database for lookups (optional)
#' @param session get session object for ENTERED_BY
#' @return Data frame in parameters module format
#' @importFrom purrr map_dfr
#' @noRd
# ! FORMAT-BASED
create_parameters_from_llm <- function(
  llm_parameters_data,
  chemical_parameters = NULL,
  session
) {
  if (is.null(llm_parameters_data) || nrow(llm_parameters_data) == 0) {
    return(tibble())
  }

  # Helper function to find database match ----
  find_db_match <- function(param_name, cas_rn, chemical_parameters) {
    if (is.null(chemical_parameters)) {
      return(NULL)
    }

    # Try exact name match first
    if (param_name != "") {
      exact_match <- chemical_parameters[
        tolower(chemical_parameters$PARAMETER_NAME) == tolower(param_name),
      ]
      if (nrow(exact_match) > 0) {
        return(exact_match[1, ]) # Take first match
      }
    }

    # Try CAS lookup if no name match
    if (cas_rn != "") {
      cas_match <- chemical_parameters[
        !is.na(chemical_parameters$CAS_RN) &
          chemical_parameters$CAS_RN == cas_rn,
      ]
      if (nrow(cas_match) > 0) {
        return(cas_match[1, ]) # Take first match
      }
    }

    return(NULL)
  }

  # Process each parameter row ----
  params_tibble <- map_dfr(seq_len(nrow(llm_parameters_data)), function(i) {
    param <- llm_parameters_data[i, ]

    param_name <- safe_extract_field(param, "parameter_name", "")
    cas_rn <- safe_extract_field(param, "cas_rn", "")
    param_comment <- safe_extract_field(param, "parameter_comment", "")

    # Try to get data from chemical database
    db_match <- find_db_match(param_name, cas_rn, chemical_parameters)

    # Build parameter row
    tibble(
      PARAMETER_TYPE = if (!is.null(db_match)) {
        db_match$PARAMETER_TYPE
      } else {
        map_parameter_type_strict(safe_extract_field(
          param,
          "parameter_type",
          ""
        ))
      },
      PARAMETER_TYPE_SUB = if (!is.null(db_match)) {
        db_match$PARAMETER_TYPE_SUB
      } else {
        "Not reported"
      },
      MEASURED_TYPE = if (!is.null(db_match)) {
        db_match$MEASURED_TYPE
      } else {
        "Concentration"
      },
      PARAMETER_NAME = param_name,
      PARAMETER_NAME_SUB = "",
      INCHIKEY_SD = if (!is.null(db_match)) db_match$INCHIKEY_SD else "",
      PUBCHEM_CID = if (!is.null(db_match)) db_match$PUBCHEM_CID else "",
      CAS_RN = cas_rn,
      ENTERED_BY = session$userData$reactiveValues$ENTERED_BY %|truthy|% "",
      PARAMETER_COMMENT = param_comment,
    )
  })

  print_dev(glue("Created {nrow(params_tibble)} parameters from LLM data"))
  return(params_tibble)
}

## mod_parameters helper functions ----

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

# mod_compartment ----

#' Populate Compartments Data from LLM Data
#'
#' @description Creates compartments data frame from LLM extracted compartments data frame
#' @param llm_compartments_data Compartments data frame extracted by LLM
#' @return Data frame in compartments module format
#' @noRd
# ! FORMAT-BASED
create_compartments_from_llm <- function(llm_compartments_data) {
  if (is.null(llm_compartments_data) || nrow(llm_compartments_data) == 0) {
    return(tibble())
  }

  compartments_tibble <- tibble()

  # Process each row of the compartments data frame
  for (i in seq_len(nrow(llm_compartments_data))) {
    comp <- llm_compartments_data[i, ]

    comp_row <- tibble(
      ENVIRON_COMPARTMENT = map_compartment_strict(safe_extract_field(
        comp,
        "environ_compartment",
        ""
      )),
      ENVIRON_COMPARTMENT_SUB = map_compartment_sub_strict(safe_extract_field(
        comp,
        "environ_compartment_sub",
        ""
      )),
      MEASURED_CATEGORY = map_measured_category_strict(safe_extract_field(
        comp,
        "measured_category",
        ""
      ))
    )

    compartments_tibble <- rbind(compartments_tibble, comp_row)
  }

  print_dev(glue(
    "Created {nrow(compartments_tibble)} compartments from LLM data"
  ))
  return(compartments_tibble)
}

## mod_compartment helper functions ----

#' Map compartment to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
map_compartment_strict <- function(compartment) {
  if (is.null(compartment) || compartment == "") {
    return("Not reported")
  }

  comp_lower <- tolower(compartment)

  if (grepl("aquatic|water", comp_lower)) {
    return("Aquatic")
  }
  if (grepl("atmosph|air", comp_lower)) {
    return("Atmospheric")
  }
  if (grepl("terrestrial|soil|land", comp_lower)) {
    return("Terrestrial")
  }
  if (grepl("biota|organism", comp_lower)) {
    return("Biota")
  }

  return("Other")
}

#' Map compartment sub to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
map_compartment_sub_strict <- function(compartment_sub) {
  if (is.null(compartment_sub) || compartment_sub == "") {
    return("Not reported")
  }

  sub_lower <- tolower(compartment_sub)

  # Aquatic sub-compartments
  if (grepl("fresh", sub_lower)) {
    return("Freshwater")
  }
  if (grepl("marine|salt|sea", sub_lower)) {
    return("Marine/Salt Water")
  }
  if (grepl("brackish", sub_lower)) {
    return("Brackish/Transitional Water")
  }
  if (grepl("ground", sub_lower)) {
    return("Groundwater")
  }
  if (grepl("waste", sub_lower)) {
    return("Wastewater")
  }
  if (grepl("growth.*medium", sub_lower)) {
    return("Liquid Growth Medium")
  }
  if (grepl("rain", sub_lower)) {
    return("Rainwater")
  }
  if (grepl("storm", sub_lower)) {
    return("Stormwater")
  }
  if (grepl("leachate", sub_lower)) {
    return("Leachate")
  }
  if (grepl("sediment", sub_lower)) {
    return("Aquatic Sediment")
  }

  # Atmospheric sub-compartments
  if (grepl("indoor", sub_lower)) {
    return("Indoor Air")
  }
  if (grepl("outdoor", sub_lower)) {
    return("Outdoor Air")
  }

  # Terrestrial sub-compartments
  if (grepl("biological.*residue", sub_lower)) {
    return("Terrestrial Biological Residue")
  }
  if (grepl("h.*horizon|peat", sub_lower)) {
    return("Soil H Horizon (Peat)")
  }
  if (grepl("o.*horizon|organic", sub_lower)) {
    return("Soil O Horizon (Organic)")
  }
  if (grepl("a.*horizon|topsoil", sub_lower)) {
    return("Soil A Horizon (Topsoil)")
  }
  if (grepl("e.*horizon.*mineral", sub_lower)) {
    return("Soil E Horizon (Mineral)")
  }
  if (grepl("s.*horizon.*mineral", sub_lower)) {
    return("Soil S Horizon (Mineral)")
  }
  if (grepl("c.*horizon|parent.*material", sub_lower)) {
    return("Soil C Horizon (Parent Material)")
  }
  if (grepl("r.*horizon|bedrock", sub_lower)) {
    return("Soil R Horizon (Bedrock)")
  }

  # Biota sub-compartments
  if (grepl("biota.*terrestrial", sub_lower)) {
    return("Biota, Terrestrial")
  }
  if (grepl("biota.*aquatic", sub_lower)) {
    return("Biota, Aquatic")
  }
  if (grepl("biota.*atmospheric", sub_lower)) {
    return("Biota, Atmospheric")
  }
  if (grepl("biota.*other", sub_lower)) {
    return("Biota, Other")
  }

  return("Other")
}

#' Map measured category to strict controlled vocabulary
#' @description Maps to the exact controlled vocabulary used in compartments module
#' @noRd
map_measured_category_strict <- function(category) {
  if (is.null(category) || category == "") {
    return("External")
  }

  cat_lower <- tolower(category)

  if (grepl("external", cat_lower)) {
    return("External")
  }
  if (grepl("internal", cat_lower)) {
    return("Internal")
  }
  if (grepl("surface", cat_lower)) {
    return("Surface")
  }

  return("External") # Default assumption
}

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

# mod_biota ----

#' Create Biota Data from LLM Data
#'
#' @description Creates biota data frame from LLM extracted biota data frame
#' @param llm_biota_data Biota data frame extracted by LLM
#' @return Data frame in biota module format
#' @noRd
# ! FORMAT-BASED
create_biota_from_llm <- function(llm_biota_data) {
  if (is.null(llm_biota_data) || nrow(llm_biota_data) == 0) {
    return(tibble())
  }

  biota_tibble <- tibble()

  # Process each row of the biota data frame
  for (i in seq_len(nrow(llm_biota_data))) {
    biota_entry <- llm_biota_data[i, ]

    # Create biota row with LLM data
    biota_row <- tibble(
      SAMPLE_ID = safe_extract_field(
        biota_entry,
        "sample_id",
        paste0("BIO_", sprintf("%03d", i))
      ),
      SITE_CODE = "SITE_001", # Default - will be updated when samples are created
      PARAMETER_NAME = "Parameter_001", # Default - will be updated when samples are created
      ENVIRON_COMPARTMENT = "Biota",
      ENVIRON_COMPARTMENT_SUB = "Biota Aquatic", # Default assumption
      MEASURED_CATEGORY = "Internal", # Default for biota
      SAMPLING_DATE = as.character(Sys.Date()), # Default
      REP = 1L, # Default
      SPECIES_GROUP = map_species_group_strict(safe_extract_field(
        biota_entry,
        "species_group",
        ""
      )),
      SAMPLE_SPECIES = safe_extract_field(biota_entry, "sample_species", ""),
      SAMPLE_TISSUE = map_tissue_type_strict(safe_extract_field(
        biota_entry,
        "sample_tissue",
        ""
      )),
      SAMPLE_SPECIES_LIFESTAGE = map_lifestage_strict(safe_extract_field(
        biota_entry,
        "sample_species_lifestage",
        ""
      )),
      SAMPLE_SPECIES_GENDER = map_gender_strict(safe_extract_field(
        biota_entry,
        "sample_species_gender",
        ""
      ))
    )

    biota_tibble <- rbind(biota_tibble, biota_row)
  }

  print_dev(glue("Created {nrow(biota_tibble)} biota entries from LLM data"))
  return(biota_tibble)
}
#' Validate Species Against Database
#'
#' @description Validates species names against database with scientific name priority
#' @param biota_data Biota data frame with SAMPLE_SPECIES column
#' @param species_database Species reference database with SPECIES_NAME and SPECIES_COMMON_NAME columns
#' @return List with validation results and formatted text output
#' @noRd
# ! FORMAT-BASED
validate_species_against_database <- function(biota_data, species_database) {
  if (is.null(biota_data) || nrow(biota_data) == 0) {
    return(list(
      validation_text = "No biota samples to validate.",
      has_warnings = FALSE
    ))
  }

  output_lines <- c("Species Database Validation Results:", "")
  has_warnings <- FALSE

  # Process each unique species name
  unique_species <- unique(biota_data$SAMPLE_SPECIES)
  unique_species <- unique_species[
    !is.na(unique_species) & unique_species != ""
  ]

  if (length(unique_species) == 0) {
    return(list(
      validation_text = "No species names provided for validation.",
      has_warnings = TRUE
    ))
  }

  for (species_name in unique_species) {
    output_lines <- c(output_lines, paste0("Species: \"", species_name, "\""))

    # Try exact scientific name match first (case insensitive)
    scientific_matches <- species_database[
      tolower(species_database$SPECIES_NAME) == tolower(species_name),
    ]

    if (nrow(scientific_matches) > 0) {
      output_lines <- c(output_lines, "  \u2713 Found as scientific name")
      if (nrow(scientific_matches) > 1) {
        output_lines <- c(
          output_lines,
          paste0(
            "    \u2192 Multiple entries found (",
            nrow(scientific_matches),
            " matches)"
          )
        )
        has_warnings <- TRUE
      }
      # Show the canonical scientific name
      output_lines <- c(
        output_lines,
        paste0("    \u2192 Canonical: ", scientific_matches$SPECIES_NAME[1])
      )
    } else {
      # Try common name match
      common_matches <- species_database[
        tolower(species_database$SPECIES_COMMON_NAME) == tolower(species_name),
      ]

      if (nrow(common_matches) > 0) {
        output_lines <- c(output_lines, "  \u26A0 Found as common name")
        has_warnings <- TRUE

        if (nrow(common_matches) > 1) {
          output_lines <- c(
            output_lines,
            paste0(
              "    \u2192 Multiple species match this common name (",
              nrow(common_matches),
              " matches)"
            )
          )
          output_lines <- c(output_lines, "    \u2192 Manual review required:")

          # List all matching scientific names (limit to first 10)
          display_count <- min(10, nrow(common_matches))
          for (i in 1:display_count) {
            output_lines <- c(
              output_lines,
              paste0("       \u2022 ", common_matches$SPECIES_NAME[i])
            )
          }
          if (nrow(common_matches) > 10) {
            output_lines <- c(
              output_lines,
              paste0(
                "       ... and ",
                nrow(common_matches) - 10,
                " more matches"
              )
            )
          }
        } else {
          output_lines <- c(
            output_lines,
            paste0(
              "    \u2192 Scientific name: ",
              common_matches$SPECIES_NAME[1]
            )
          )
        }
      } else {
        output_lines <- c(
          output_lines,
          "  \u26A0 Species not found in database"
        )
        output_lines <- c(
          output_lines,
          "    \u2192 Manual verification required"
        )
        has_warnings <- TRUE
      }
    }

    output_lines <- c(output_lines, "") # Blank line between species
  }

  # Summary
  total_species <- length(unique_species)
  scientific_found <- sum(sapply(unique_species, function(sp) {
    nrow(species_database[
      tolower(species_database$SPECIES_NAME) == tolower(sp),
    ]) >
      0
  }))
  common_found <- sum(sapply(unique_species, function(sp) {
    nrow(species_database[
      tolower(species_database$SPECIES_COMMON_NAME) == tolower(sp),
    ]) >
      0 &&
      nrow(species_database[
        tolower(species_database$SPECIES_NAME) == tolower(sp),
      ]) ==
        0
  }))
  not_found <- total_species - scientific_found - common_found

  output_lines <- c(output_lines, "Summary:")
  output_lines <- c(
    output_lines,
    paste0("  Species processed: ", total_species)
  )
  output_lines <- c(
    output_lines,
    paste0("  Found as scientific names: ", scientific_found)
  )
  output_lines <- c(
    output_lines,
    paste0("  Found as common names: ", common_found)
  )
  output_lines <- c(
    output_lines,
    paste0("  Not found in database: ", not_found)
  )

  if (has_warnings) {
    output_lines <- c(output_lines, "")
    output_lines <- c(
      output_lines,
      "\u26A0 Manual review recommended for flagged species."
    )
  } else {
    output_lines <- c(output_lines, "")
    output_lines <- c(
      output_lines,
      "\u2713 All species found as scientific names!"
    )
  }

  return(list(
    validation_text = paste(output_lines, collapse = "\n"),
    has_warnings = has_warnings
  ))
}

# mod_methods ----

#' Create Methods Data from LLM Data
#'
#' @description Creates methods data frame from LLM extracted methods data frame.
#' Ensures all four protocol categories are present, adding "Not reported" entries for missing categories.
#' @param llm_methods_data Methods data frame extracted by LLM
#' @return Data frame in methods module format
#' @noRd
# ! FORMAT-BASED
create_methods_from_llm <- function(llm_methods_data, llm_campaign_data) {
  # Define all required protocol categories ----
  required_categories <- c(
    "Sampling Protocol",
    "Fractionation Protocol",
    "Extraction Protocol",
    "Analytical Protocol"
  )

  methods_tibble <- initialise_methods_tibble()

  # Process LLM extracted methods if available ----
  if (!is.null(llm_methods_data) && nrow(llm_methods_data) > 0) {
    for (i in seq_len(nrow(llm_methods_data))) {
      method_entry <- llm_methods_data[i, ]

      # Create method row with LLM data
      method_row <- tibble(
        PROTOCOL_ID = "",
        CAMPAIGN_NAME = "",
        PROTOCOL_CATEGORY = map_protocol_category_strict(safe_extract_field(
          method_entry,
          "protocol_category",
          ""
        )),
        PROTOCOL_NAME = safe_extract_field(method_entry, "protocol_name", ""),
        PROTOCOL_COMMENT = safe_extract_field(
          method_entry,
          "protocol_comment",
          ""
        )
      )

      methods_tibble <- add_row(methods_tibble, method_row)
    }
  }

  # Identify missing protocol categories ----
  extracted_categories <- unique(methods_tibble$PROTOCOL_CATEGORY)
  extracted_categories <- extracted_categories[extracted_categories != ""] # Remove empty strings

  missing_categories <- setdiff(required_categories, extracted_categories)

  # Add "Not reported" entries for missing categories ----
  if (length(missing_categories) > 0) {
    for (category in missing_categories) {
      missing_row <- tibble(
        PROTOCOL_ID = "",
        CAMPAIGN_NAME = "",
        PROTOCOL_CATEGORY = category,
        PROTOCOL_NAME = "Not reported",
        PROTOCOL_COMMENT = ""
      )

      methods_tibble <- add_row(methods_tibble, missing_row)
    }

    print_dev(glue(
      "Added 'Not reported' entries for missing categories: {paste(missing_categories, collapse = ', ')}"
    ))
  }

  # Sort by protocol category for consistency ----
  methods_tibble <- methods_tibble[
    order(match(methods_tibble$PROTOCOL_CATEGORY, required_categories)),
  ]

  # and add IDs
  methods_tibble <- methods_tibble |>
    group_by(PROTOCOL_CATEGORY) |>
    mutate(sequence = row_number()) |>
    ungroup() |>
    mutate(
      PROTOCOL_ID = generate_protocol_id(
        PROTOCOL_CATEGORY,
        PROTOCOL_NAME,
        sequence,
        llm_campaign_data$campaign_name_short
      ),
      CAMPAIGN_NAME = llm_campaign_data$campaign_name
    ) |>
    select(-sequence) |>
    relocate(PROTOCOL_ID)

  print_dev(glue(
    "Created {nrow(methods_tibble)} methods from LLM data (including missing categories)"
  ))
  return(methods_tibble)
}

create_samples_from_llm <- function(llm_samples_data) {
  # Currently we only send SAMPLING_DATE to the samples module
  if (is.null(llm_samples_data) || length(llm_samples_data) == 0) {
    return(c())
  } else {
    dates_vector <- c()

    for (row in 1:nrow(llm_samples_data)) {
      date <- llm_samples_data$sampling_date[row]
      print(date)
      dates_vector <- append(dates_vector, date)
    }
    return(dates_vector)
  }

  # if (is.null(llm_biota_data) || nrow(llm_biota_data) == 0) {
  #   return(tibble())
  # }

  # samples_tibble <- tibble()

  # Process each row of the biota data frame
  # tryCatch(
  #   {
  #     for (i in seq_len(nrow(llm_samples_data))) {
  #       samples_entry <- llm_samples_data[i, ]

  #       # Create biota row with LLM data
  #       samples_row <- tibble(
  #         SAMPLING_DATE = safe_extract_field(
  #           samples,
  #           "sampling_dates",
  #           as.Date(NA)
  #         ),
  #         SITE_CODE = "SITE_001", # Default - will be updated when samples are created
  #         PARAMETER_NAME = "Parameter_001", # Default - will be updated when samples are created
  #         ENVIRON_COMPARTMENT = "Biota",
  #         ENVIRON_COMPARTMENT_SUB = "Biota Aquatic", # Default assumption
  #         MEASURED_CATEGORY = "Internal", # Default for biota
  #         SAMPLING_DATE = as.character(Sys.Date()), # Default
  #         REP = 1L, # Default
  #         SPECIES_GROUP = map_species_group_strict(safe_extract_field(
  #           biota_entry,
  #           "species_group",
  #           ""
  #         )),
  #         SAMPLE_SPECIES = safe_extract_field(
  #           biota_entry,
  #           "sample_species",
  #           ""
  #         ),
  #         SAMPLE_TISSUE = map_tissue_type_strict(safe_extract_field(
  #           biota_entry,
  #           "sample_tissue",
  #           ""
  #         )),
  #         SAMPLE_SPECIES_LIFESTAGE = map_lifestage_strict(safe_extract_field(
  #           biota_entry,
  #           "sample_species_lifestage",
  #           ""
  #         )),
  #         SAMPLE_SPECIES_GENDER = map_gender_strict(safe_extract_field(
  #           biota_entry,
  #           "sample_species_gender",
  #           ""
  #         ))
  #       )

  #       samples_tibble <- rbind(samples_tibble, samples_row)
  #     }
  #   },
  #   error = function(msg) {
  #     glue("Error populating samples data: {llm_samples_data[i, ]}")
  #   }
  # )

  # print_dev(glue(
  #   "Created {nrow(samples_tibble)} samples entries from LLM data"
  # ))
  return(samples_tibble)
}
