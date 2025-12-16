### -----------------------------------------
### ----------- CREED Functions -------------
### -----------------------------------------

#' Get All Gateway Data Summaries
#'
#' @description Gets all gateway criteria data summaries
#' @param module_data List containing all module data
#' @return Named list of gateway summaries
#' @noRd
get_gateway_summaries <- function(module_data) {
  list(
    medium = if (!is.null(module_data$compartments)) {
      summarise_multiple(
        module_data$compartments$ENVIRON_COMPARTMENT,
        "Compartments"
      )
    } else {
      "Relevant data not found"
    },

    analyte = if (!is.null(module_data$parameters)) {
      summarise_multiple(module_data$parameters$PARAMETER_NAME, "Parameters")
    } else {
      "Relevant data not found"
    },

    location = if (!is.null(module_data$sites)) {
      countries <- summarise_multiple(
        module_data$sites$COUNTRY_ISO,
        "Countries"
      )
      areas <- summarise_multiple(module_data$sites$OCEAN_IHO, "Areas")
      paste(countries, areas, sep = "; ")
    } else {
      "Relevant data not found"
    },

    year = if (!is.null(module_data$samples)) {
      summarise_date_range(module_data$samples$SAMPLING_DATE)
    } else {
      "Relevant data not found"
    },

    units = summarise_measured_units(
      module_data$measurements,
      module_data$parameters
    ),

    citation = if (!is.null(module_data$references)) {
      summarise_reference(module_data$references)
    } else {
      "Relevant data not found"
    }
  )
}

#' Check Gateway Criteria Availability
#'
#' @description Checks if data is available for each gateway criterion
#' @param module_data List containing all module data
#' @return Named list of TRUE/FALSE values
#' @noRd
check_gateway_availability <- function(module_data) {
  list(
    medium = !is.null(module_data$compartments) &&
      any(
        !is.na(module_data$compartments$ENVIRON_COMPARTMENT) &
          module_data$compartments$ENVIRON_COMPARTMENT != ""
      ),

    analyte = !is.null(module_data$parameters) &&
      any(
        !is.na(module_data$parameters$PARAMETER_NAME) &
          module_data$parameters$PARAMETER_NAME != ""
      ),

    location = !is.null(module_data$sites) &&
      (any(
        !is.na(module_data$sites$COUNTRY_ISO) &
          module_data$sites$COUNTRY_ISO != ""
      ) ||
        any(
          !is.na(module_data$sites$OCEAN_IHO) &
            module_data$sites$OCEAN_IHO != ""
        )),

    year = !is.null(module_data$samples) &&
      any(!is.na(module_data$samples$SAMPLING_DATE)),

    units = !is.null(module_data$measurements) &&
      any(
        !is.na(module_data$measurements$MEASURED_UNIT) &
          module_data$measurements$MEASURED_UNIT != ""
      ),

    citation = !is.null(module_data$references) &&
      (any(
        !is.na(module_data$references$AUTHOR) &
          module_data$references$AUTHOR != ""
      ) ||
        any(
          !is.na(module_data$references$TITLE) &
            module_data$references$TITLE != ""
        ) ||
        any(
          !is.na(module_data$references$DOI) & module_data$references$DOI != ""
        ))
  )
}

#' @title summarise user-entered data for the CREED dataset details reporting
#' @description summarise session user data into pretty strings for CREED "Dataset Details" reporting
#' @param sessionData session$userData$reactiveValues object
#' @return tibble of Dataset Details fields ("field") and pretty strings ("value")
#' @export
summarise_CREED_details <- function(sessionData) {
  # Helper function to check if a dataset exists and has content
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  # Check existence of each dataset once
  has_reference <- dataset_exists(sessionData$referenceData)
  has_parameters <- dataset_exists(sessionData$parametersData)
  has_compartments <- dataset_exists(sessionData$compartmentsData)
  has_sites <- dataset_exists(sessionData$sitesData)
  has_samples <- dataset_exists(sessionData$samplesData)
  has_methods <- dataset_exists(sessionData$methodsData)
  has_measurements <- dataset_exists(sessionData$measurementsData)

  # Extract values into intermediate variables
  source_value <- if (has_reference) {
    summarise_reference(sessionData$referenceData)
  } else {
    "Relevant data not found"
  }

  analytes_value <- if (has_parameters) {
    summarise_multiple(
      sessionData$parametersData$PARAMETER_NAME,
      "Parameters"
    )
  } else {
    "Relevant data not found"
  }

  medium_value <- if (has_compartments) {
    summarise_multiple(
      sessionData$compartmentsData$ENVIRON_COMPARTMENT,
      "Compartments"
    )
  } else {
    "Relevant data not found"
  }

  study_area_value <- if (has_sites) {
    countries <- summarise_multiple(
      sessionData$sitesData$COUNTRY_ISO,
      "Countries"
    )
    areas <- summarise_multiple(sessionData$sitesData$OCEAN_IHO, "Areas")
    paste(countries, areas, sep = "; ")
  } else {
    "Relevant data not found"
  }

  num_sites_value <- if (has_sites) {
    as.character(nrow(sessionData$sitesData))
  } else {
    "Relevant data not found"
  }

  site_types_value <- if (has_sites) {
    summarise_multiple(
      sessionData$sitesData$SITE_GEOGRAPHIC_FEATURE,
      "Site Types"
    )
  } else {
    "Relevant data not found"
  }

  num_samples_value <- if (has_samples) {
    as.character(nrow(sessionData$samplesData))
  } else {
    "Relevant data not found"
  }

  sampling_period_value <- if (has_samples) {
    summarise_date_range(sessionData$samplesData$SAMPLING_DATE)
  } else {
    "Relevant data not found"
  }

  sampling_methods_value <- if (has_methods) {
    sampling_only <- sessionData$methodsData[
      sessionData$methodsData$PROTOCOL_CATEGORY == "Sampling Protocol",
    ]
    if (nrow(sampling_only) > 0) {
      summarise_multiple(sampling_only$PROTOCOL_NAME, "Sampling Protocols")
    } else {
      "Relevant data not found"
    }
  } else {
    "Relevant data not found"
  }

  analytical_methods_value <- if (has_methods) {
    analytical_only <- sessionData$methodsData[
      sessionData$methodsData$PROTOCOL_CATEGORY == "Analytical Protocol",
    ]
    if (nrow(analytical_only) > 0) {
      summarise_multiple(
        analytical_only$PROTOCOL_NAME,
        "Analytical Protocols"
      )
    } else {
      "Relevant data not found"
    }
  } else {
    "Relevant data not found"
  }

  loq_info_value <- if (has_measurements) {
    loq_values <- sessionData$measurementsData$LOQ_VALUE[
      !is.na(sessionData$measurementsData$LOQ_VALUE)
    ]
    lod_values <- sessionData$measurementsData$LOD_VALUE[
      !is.na(sessionData$measurementsData$LOD_VALUE)
    ]

    info_parts <- c()
    if (length(loq_values) > 0) {
      loq_range <- paste(min(loq_values), "to", max(loq_values))
      loq_unit <- sessionData$measurementsData$LOQ_UNIT[
        !is.na(sessionData$measurementsData$LOQ_UNIT)
      ][1]
      info_parts <- c(info_parts, paste("LOQ:", loq_range, loq_unit))
    }
    if (length(lod_values) > 0) {
      lod_range <- paste(min(lod_values), "to", max(lod_values))
      lod_unit <- sessionData$measurementsData$LOD_UNIT[
        !is.na(sessionData$measurementsData$LOD_UNIT)
      ][1]
      info_parts <- c(info_parts, paste("LOD:", lod_range, lod_unit))
    }

    if (length(info_parts) > 0) {
      paste(info_parts, collapse = "; ")
    } else {
      "Relevant data not found"
    }
  } else {
    "Relevant data not found"
  }

  # Build tibble from extracted values
  tibble(
    field = c(
      "source",
      "analytes",
      "medium",
      "study_area",
      "num_sites",
      "site_types",
      "num_samples",
      "sampling_period",
      "sampling_methods",
      "analytical_methods",
      "loq_info"
    ),
    value = c(
      source_value,
      analytes_value,
      medium_value,
      study_area_value,
      num_sites_value,
      site_types_value,
      num_samples_value,
      sampling_period_value,
      sampling_methods_value,
      analytical_methods_value,
      loq_info_value
    )
  )
}

############################
# Reactive/UI Functions ----
############################

#' Create CREED Criterion Section
#'
#' @description Creates a standardized UI section for CREED assessment criteria
#' @param ns Namespace function from the calling module
#' @param criterion_id Character string ID for the criterion (e.g., "RV01", "RB1")
#' @param title Character string title for the criterion
#' @param type Character string type, either "required" or "recommended"
#' @param description Character string description of the criterion
#' @return Shiny div element containing the complete criterion section
#' @noRd
#' @importFrom shiny div selectInput p strong h4 HTML
#' @importFrom bslib layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom tools toTitleCase
create_criterion_section <- function(
  ns,
  criterion_id,
  title,
  type,
  description
) {
  icon_class <- if (type == "required") {
    "CREED-required"
  } else {
    "CREED-recommended"
  }
  type_text <- if (type == "required") "Required" else "Recommended"

  div(
    style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",

    # Header with title and dropdown ----
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
      div(
        style = "flex-grow: 1; margin-right: 20px;",
        h6(
          HTML(paste(
            bs_icon("award-fill", class = icon_class),
            glue(
              "{criterion_id}: {title} ({tools::toTitleCase(type_text)})"
            )
          ))
        ),
        p(
          strong("Criterion: "),
          description
        )
      ),
      div(
        style = "min-width: 150px;",

        selectInput(
          inputId = ns(paste0(criterion_id, "_score")),
          label = "Score:",
          choices = CREED_choices_vocabulary(),
          width = "150px",
        )
      )
    ),

    # Two-column layout for data and justification ----
    layout_column_wrap(
      width = "400px",
      create_relevant_data_input(ns, criterion_id),
      create_limitations_input(ns, criterion_id)
    )
  )
}

#' Create Threshold Display Boxes
#'
#' @description Creates threshold display boxes for CREED purpose criteria
#' @param criterion_id Character string ID for the criterion (e.g., "RV1")
#' @param creed_purpose List containing CREED purpose YAML data
#' @return Shiny layout_column_wrap element with threshold boxes, or NULL if data not found
#' @noRd
#' @importFrom shiny div p h5
#' @importFrom bslib layout_column_wrap
create_threshold_boxes <- function(criterion_id, creed_purpose) {
  # Validate inputs
  if (
    is.null(creed_purpose) ||
      length(creed_purpose) == 0 ||
      is.null(creed_purpose$thresholds) ||
      length(creed_purpose$thresholds) == 0
  ) {
    return("couldn't find creed_purpose data")
  }

  # Find the criterion in the YAML data
  criterion_data <- creed_purpose$thresholds[[criterion_id]]

  # Return NULL if criterion not found
  if (is.null(criterion_data)) {
    return("couldn't find criterion_data")
  }

  # Build threshold display boxes
  layout_column_wrap(
    width = "400px",
    fillable = FALSE,
    fill = FALSE,

    # Partly Met box ----
    div(
      class = "alert alert-secondary",
      h5("Partly Met:"),
      HTML(gsub("\n", "<br>", criterion_data$partly_met)),
      style = "margin-bottom: -20px !important;"
    ),

    # Fully Met box ----
    div(
      class = "alert alert-secondary",
      h5("Fully Met:"),
      HTML(gsub("\n", "<br>", criterion_data$fully_met))
    )
  )
}

#' Create Relevant Data Input Field
#'
#' @description Creates a text area input for relevant data extraction
#' @param ns Namespace function from the calling module
#' @param criterion_id Character string ID for the criterion
#' @param autofill Boolean for presence of blue autofill tooltip/icon
#' @return Shiny div element containing the relevant data input
#' @noRd
#' @importFrom shiny div textAreaInput
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
create_relevant_data_input <- function(ns, criterion_id, autofill = TRUE) {
  div(
    textAreaInput(
      inputId = ns(paste0(criterion_id, "_relevant_data")),
      label = tooltip(
        list(
          "Relevant Data",
          if (isTRUE(autofill)) {
            bs_icon("arrow-down-circle-fill", class = "text-primary")
          } else {
            ""
          }
        ),
        "Data extracted from your dataset that is relevant to this criterion."
      ),
      value = "",
      rows = 5,
      width = "100%"
    )
  )
}

#' Create Limitations Input Field
#'
#' @description Creates a text area input for relevance limitations/restrictions
#' @param ns Namespace function from the calling module
#' @param criterion_id Character string ID for the criterion
#' @return Shiny text area input for limitations
#' @noRd
#' @importFrom shiny textAreaInput
create_limitations_input <- function(ns, criterion_id) {
  textAreaInput(
    inputId = ns(paste0(criterion_id, "_limitations")),
    label = "Relevance Limitations/Restrictions (free text)",
    placeholder = "Describe any limitations or restrictions relevant to this criterion...",
    rows = 5,
    width = "100%"
  )
}


#' Create Justification Input Field
#'
#' @description Creates a text area input for criterion scoring justification
#' @param ns Namespace function from the calling module
#' @param criterion_id Character string ID for the criterion
#' @return Shiny text area input for justification
#' @noRd
#' @importFrom shiny textAreaInput
create_justification_input <- function(ns, criterion_id) {
  textAreaInput(
    inputId = ns(paste0(criterion_id, "_justification")),
    label = "Justification (free text)",
    placeholder = "Provide justification for your scoring...",
    rows = 3,
    width = "100%"
  )
}

#' Create Conditional Criterion Section
#'
#' @description Creates a criterion section that can be conditionally disabled
#' @param ns Namespace function from the calling module
#' @param criterion_id Character string ID for the criterion
#' @param title Character string title for the criterion
#' @param type Character string type, either "required" or "recommended"
#' @param description Character string description of the criterion
#' @param note Optional character string note for additional context
#' @return Shiny div element with conditional functionality
#' @noRd
#' @importFrom shiny div
create_conditional_criterion <- function(
  ns,
  criterion_id,
  title,
  type,
  description,
  note = NULL
) {
  div(
    id = ns(paste0(criterion_id, "_container")),
    style = "margin: 5px 0; padding: 15px 0; border-bottom: 1px solid #dee2e6;",
    create_criterion_section(
      ns,
      criterion_id,
      title,
      type,
      description
      # note,
      # is_conditional = TRUE
    )
  )
}

# Data Helpers/Storage Functions ----

#' @importFrom yaml read_yaml
copper_CREED_purpose_statement <- function() {
  read_yaml(system.file(
    "app/www/md/",
    "CREED_Copper_Purpose.yml",
    package = "STOPeData"
  ))
}


#' @title summarise user-entered data for the CREED reliability criteria reporting
#' @description summarise session user data into pretty strings for CREED reliability criteria
#' @param sessionData session$userData$reactiveValues object
#' @return tibble of Reliability criteria fields ("field") and pretty strings ("value")
#' @export
summarise_CREED_reliability <- function(sessionData) {
  # Helper function to check if a dataset exists and has content
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  # Check existence of each dataset once ----
  has_compartments <- dataset_exists(sessionData$compartmentsData)
  has_methods <- dataset_exists(sessionData$methodsData)
  has_sites <- dataset_exists(sessionData$sitesData)
  has_samples <- dataset_exists(sessionData$samplesData)
  has_biota <- dataset_exists(sessionData$samplesDataWithBiota)
  has_measurements <- dataset_exists(sessionData$measurementsData)

  # RB1: Compartments and sampling/fractionation protocols ----
  RB1_value <- if (has_compartments || has_methods) {
    parts <- c()

    # Compartments section
    if (has_compartments) {
      comp_summary <- summarise_compartments(sessionData$compartmentsData)
      if (comp_summary != "Relevant data not found") {
        parts <- c(parts, comp_summary)
      }
    }

    # Biota section
    if (has_biota) {
      biota_summary <- summarise_biota(
        sessionData$samplesDataWithBiota,
        SPECIES_GROUP = FALSE,
        SAMPLE_SPECIES = TRUE,
        SAMPLE_TISSUE = TRUE,
        SAMPLE_SPECIES_LIFESTAGE = TRUE,
        SAMPLE_SPECIES_GENDER = TRUE
      )
      if (biota_summary != "Relevant data not found") {
        parts <- c(parts, biota_summary)
      }
    }

    # Sampling, extraction, fractionation protocols
    if (has_methods) {
      protocol_summary <- summarise_protocols(
        sessionData$methodsData,
        c("Sampling Protocol", "Fractionation Protocol", "Extraction Protocol")
      )
      if (protocol_summary != "Relevant data not found") {
        parts <- c(parts, protocol_summary)
      }
    }

    if (length(parts) > 0) {
      paste(parts, collapse = "\n\n")
    } else {
      "Relevant data not found"
    }
  } else {
    "Relevant data not found"
  }

  # RB2 & & RB13: Sampling Protocols ----
  sampling_protocols_value <- summarise_protocols(
    sessionData$methodsData,
    "Sampling Protocol"
  )

  # RB: Sampling & Extraction Protocols ----
  RB3_value <- summarise_protocols(
    sessionData$methodsData,
    c("Sampling Protocol", "Extraction Protocol")
  )

  # RB4: Summary of all sites ----
  RB4_value <- summarise_sites(
    sessionData$sitesData,
    COUNTRY_ISO = TRUE,
    OCEAN_IHO = TRUE,
    SITE_GEOGRAPHIC_FEATURE = TRUE,
    SITE_GEOGRAPHIC_FEATURE_SUB = TRUE,
    PRECISION = TRUE
  )

  # RB5: Sampling dates ----
  RB5_value <- if (has_samples) {
    summarise_date_range(sessionData$samplesData$SAMPLING_DATE)
  } else {
    "Relevant data not found"
  }

  # RB6, RB10, RB11, RB12: Analytical Protocols ----
  analysis_protocols_value <- summarise_protocols(
    sessionData$methodsData,
    "Analytical Protocol"
  )

  # RB7: LOD/LOQ information ----
  RB7_value <- summarise_lod_loq(sessionData$measurementsData)

  # RB8: Manual completion ----
  RB8_value <- manual_completion_message()

  # RB9: All protocols ----
  RB9_value <- summarise_protocols(
    sessionData$methodsData,
    c(
      "Sampling Protocol",
      "Fractionation Protocol",
      "Analytical Protocol",
      "Extraction Protocol"
    )
  )

  # RB14: UNCERTAINTY_TYPE and MEASUREMENT_COMMENT ----
  RB14_value <- summarise_uncertainty_comments(sessionData$measurementsData)

  # RB15: Significant figures ----
  RB15_value <- summarise_sig_figs(sessionData$measurementsData$MEASURED_VALUE)

  # RB16, RB17, RB19: Manual completion ----
  RB16_value <- manual_completion_message()
  RB17_value <- manual_completion_message()
  RB19_value <- manual_completion_message()

  # RB18: UNCERTAINTY_TYPE only ----
  RB18_value <- if (has_measurements) {
    uncertainty_types <- sessionData$measurementsData$UNCERTAINTY_TYPE[
      !is.na(sessionData$measurementsData$UNCERTAINTY_TYPE) &
        sessionData$measurementsData$UNCERTAINTY_TYPE != ""
    ]

    if (length(uncertainty_types) > 0) {
      unique_types <- unique(uncertainty_types)
      glue("Uncertainty types: {paste(unique_types, collapse = ', ')}")
    } else {
      "Relevant data not found"
    }
  } else {
    "Relevant data not found"
  }

  # Build tibble from extracted values ----
  tribble(
    ~field , ~value                   ,
    "RB1"  , RB1_value                ,
    "RB2"  , sampling_protocols_value ,
    "RB3"  , RB3_value                ,
    "RB4"  , RB4_value                ,
    "RB5"  , RB5_value                ,
    "RB6"  , analysis_protocols_value ,
    "RB7"  , RB7_value                ,
    "RB8"  , RB8_value                ,
    "RB9"  , RB9_value                ,
    "RB10" , analysis_protocols_value ,
    "RB11" , analysis_protocols_value ,
    "RB12" , analysis_protocols_value ,
    "RB13" , sampling_protocols_value ,
    "RB14" , RB14_value               ,
    "RB15" , RB15_value               ,
    "RB16" , RB16_value               ,
    "RB17" , RB17_value               ,
    "RB18" , RB18_value               ,
    "RB19" , RB19_value
  )
}

#' @title summarise user-entered data for the CREED relevance criteria reporting
#' @description summarise session user data into pretty strings for CREED relevance criteria
#' @param sessionData session$userData$reactiveValues object
#' @return tibble of Relevance criteria fields ("field") and pretty strings ("value")
#' @export
summarise_CREED_relevance <- function(sessionData) {
  # Helper function to check if a dataset exists and has content
  dataset_exists <- function(dataset) {
    isTruthy(dataset) && !all(is.na(dataset))
  }

  # Check existence of each dataset once ----
  has_compartments <- dataset_exists(sessionData$compartmentsData)
  has_methods <- dataset_exists(sessionData$methodsData)
  has_sites <- dataset_exists(sessionData$sitesData)
  has_samples <- dataset_exists(sessionData$samplesData)
  has_measurements <- dataset_exists(sessionData$measurementsData)

  # RV1 Compartments & subcompartments
  RV1_value <- if (has_compartments) {
    parts <- c()
    comp_summary <- summarise_compartments(sessionData$compartmentsData)
    if (comp_summary != "Relevant data not found") {
      parts <- c(parts, comp_summary)
    }
  }

  # RV2 Sampling Protocols
  RV2_value <- if (has_methods) {
    parts <- c()
    protocol_summary <- summarise_protocols(
      sessionData$methodsData,
      c("Sampling Protocol")
    )
    if (protocol_summary != "Relevant data not found") {
      parts <- c(parts, protocol_summary)
    }
  }

  # RV3: Site number, country, area, lat/long precision
  RV3_value <- summarise_sites(
    sessionData$sitesData,
    COUNTRY_ISO = TRUE,
    OCEAN_IHO = TRUE,
    PRECISION = TRUE
  )

  # RV4: Site geo feature
  RV4_value <- summarise_sites(
    sessionData$sitesData,
    SITE_GEOGRAPHIC_FEATURE = TRUE,
    SITE_GEOGRAPHIC_FEATURE_SUB = TRUE
  )

  # RV5: Range of sampling dates
  RV5_value <- if (has_samples) {
    summarise_date_range(sessionData$samplesData$SAMPLING_DATE)
  } else {
    "Relevant data not found"
  }

  # RV6: Sampling frequency. Folded into RV5.
  RV6_value <- if (has_samples) {
    summarise_date_range(sessionData$samplesData$SAMPLING_DATE)
  } else {
    "Relevant data not found"
  }

  # RV7: Temporal conditions. Not recorded in app.
  RV7_value <- manual_completion_message()

  # RV8: Stressors, fractionation protocols
  RV8_value <- summarise_protocols(
    sessionData$methodsData,
    categories = "Fractionation Protocol"
  )

  # RV9: LODs, LOQs, Analytical Protocols
  RV9_value <- if (has_measurements || has_methods) {
    parts <- c()

    # Compartments section
    if (has_measurements) {
      lodloq_summary <- summarise_lod_loq(sessionData$measurementsData)
      if (lodloq_summary != "Relevant data not found") {
        parts <- c(parts, lodloq_summary)
      }
    }

    # Analytical protocols
    if (has_methods) {
      protocol_summary <- summarise_protocols(
        sessionData$methodsData,
        "Analytical Protocol"
      )
      if (protocol_summary != "Relevant data not found") {
        parts <- c(parts, protocol_summary)
      }
    }

    if (length(parts) > 0) {
      paste(parts, collapse = "\n\n")
    } else {
      "Relevant data not found"
    }
  } else {
    "Relevant data not found"
  }

  # RV10: UNCERTAINTY_TYPE
  RV10_value <- if (has_measurements) {
    uncertainty_types <- sessionData$measurementsData$UNCERTAINTY_TYPE[
      !is.na(sessionData$measurementsData$UNCERTAINTY_TYPE) &
        sessionData$measurementsData$UNCERTAINTY_TYPE != ""
    ]

    if (length(uncertainty_types) > 0) {
      unique_types <- unique(uncertainty_types)
      glue("Uncertainty types: {paste(unique_types, collapse = ', ')}")
    } else {
      "Relevant data not found"
    }
  } else {
    "Relevant data not found"
  }

  # RV11: Fractionation protocol
  RV11_value <- summarise_protocols(
    sessionData$methodsData,
    "Fractionation Protocol"
  )

  # Build tibble from extracted values ----
  tribble(
    ~field , ~value     ,
    "RV1"  , RV1_value  ,
    "RV2"  , RV2_value  ,
    "RV3"  , RV3_value  ,
    "RV4"  , RV4_value  ,
    "RV5"  , RV5_value  ,
    "RV6"  , RV6_value  ,
    "RV7"  , RV7_value  ,
    "RV8"  , RV8_value  ,
    "RV9"  , RV9_value  ,
    "RV10" , RV10_value ,
    "RV11" , RV11_value
  )
}


#' Auto-populate Reliability Fields
#'
#' @description Creates named list for auto-populating reliability criteria fields
#' @param sessionData session$userData$reactiveValues object
#' @return Named list of reliability field data for updating inputs
#' @noRd
autopop_reliability_fields <- function(sessionData) {
  # Get summaries from the main function
  summaries <- summarise_CREED_reliability(sessionData)

  # Create named list for input updates
  # Only include non-empty fields (skip RB8, RB16, RB17, RB19)
  field_list <- list()

  for (i in seq_len(nrow(summaries))) {
    field_id <- summaries$field[i]
    value <- summaries$value[i]

    # Skip empty fields
    if (value != "") {
      input_name <- paste0(field_id, "_relevant_data")
      field_list[[input_name]] <- value
    }
  }

  return(field_list)
}

#' Auto-populate Reliability Fields
#'
#' @description Creates named list for auto-populating reliability criteria fields
#' @param sessionData session$userData$reactiveValues object
#' @return Named list of reliability field data for updating inputs
#' @noRd
autopop_relevance_fields <- function(sessionData) {
  # Get summaries from the main function
  summaries <- summarise_CREED_relevance(sessionData)

  # Create named list for input updates
  field_list <- list()

  for (i in seq_len(nrow(summaries))) {
    field_id <- summaries$field[i]
    value <- summaries$value[i]

    # Skip empty fields
    if (value != "") {
      input_name <- paste0(field_id, "_relevant_data")
      field_list[[input_name]] <- value
    }
  }

  return(field_list)
}

#' Collect CREED Scores from Input
#'
#' @description Collects CREED criterion scores, relevant data, and limitations
#' from Shiny inputs into a standardised tibble. Works for both reliability and
#' relevance criteria.
#'
#' @param criteria_config Named list of criteria configurations. Each element
#'   should be a list with `title` and `type` (Required/Recommended) fields.
#' @param input Shiny input object containing score, relevant_data, and
#'   limitations inputs for each criterion.
#'
#' @return A tibble with columns: criterion_id, criterion_title,
#'   required_recommended, relevant_data, score, limitations
#'
#' @details
#' Expects input IDs to follow the pattern:
#' - `{criterion_id}_score`
#' - `{criterion_id}_relevant_data`
#' - `{criterion_id}_limitations`
#'
#' Special case: RB8 uses `_justification` instead of `_limitations`,
#' and the value is prefixed with "Justification: " in the output.
#'
#' @importFrom tibble add_row
#' @importFrom shiny isTruthy
#' @export
collect_CREED_data <- function(criteria_config, input) {
  # Start from standardised empty structure
  data <- initialise_CREED_data_tibble()

  # Loop through all criteria ----
  for (criterion_id in names(criteria_config)) {
    golem::print_dev(criterion_id)

    # Get input values ----
    score_input <- input[[paste0(criterion_id, "_score")]]
    relevant_data_input <- input[[paste0(criterion_id, "_relevant_data")]]

    # Handle RB8 justification special case ----
    if (criterion_id == "RB8") {
      limitations_input <- input[[paste0(criterion_id, "_justification")]]
      if (isTruthy(limitations_input) && limitations_input != "") {
        limitations_input <- paste0("Justification: ", limitations_input)
      }
    } else {
      limitations_input <- input[[paste0(criterion_id, "_limitations")]]
    }

    # Only add row if score is provided ----
    if (isTruthy(score_input) && score_input != "") {
      data <- data |>
        add_row(
          criterion_id = criterion_id,
          criterion_title = criteria_config[[criterion_id]]$title,
          required_recommended = criteria_config[[criterion_id]]$type,
          relevant_data = relevant_data_input %||% "",
          score = score_input,
          limitations = limitations_input %||% ""
        )
    }
  }

  data
}
