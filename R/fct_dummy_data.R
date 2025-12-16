# Dummy Data Creation Functions ----
# Functions for creating test/demonstration data
# These functions depend on their initialise_* parents in fct_formats.R
# to ensure column structure consistency

# =========================================================================
# DUMMY TIBBLE FUNCTIONS
# =========================================================================
# Each function creates a small tibble with realistic test data
# using add_row() on the parent initialise_*_tibble() to ensure
# column structure compatibility
# =========================================================================

#' Create Dummy Campaign Tibble
#'
#' @description Creates a 1-row campaign tibble with test data.
#' Inherits column structure from initialise_campaign_tibble().
#'
#' @return A tibble with 1 row of dummy campaign data
#' @importFrom tibble add_row
#' @importFrom dplyr mutate across
#' @importFrom tidyselect where
#' @import eDataDRF
#' @export
dummy_campaign_tibble <- function() {
  initialise_campaign_tibble() |>
    add_row(
      CAMPAIGN_NAME_SHORT = "TestCamp2023",
      CAMPAIGN_NAME = "Test Campaign 2023: Heavy Metals in Coastal Sediments",
      CAMPAIGN_START_DATE = as.Date("2023-01-15"),
      CAMPAIGN_END_DATE = as.Date("2023-06-30"),
      RELIABILITY_SCORE = NA_character_,
      RELIABILITY_EVAL_SYS = NA_character_,
      CONFIDENTIALITY_EXPIRY_DATE = as.Date(NA),
      ORGANISATION = "NIVA",
      ENTERED_BY = "test_user",
      ENTERED_DATE = as.Date("2023-07-01"),
      CAMPAIGN_COMMENT = "Dummy campaign for testing purposes"
    )
}

#' Create Dummy References Tibble
#'
#' @description Creates a 1-row references tibble with test data.
#' Inherits column structure from initialise_references_tibble().
#'
#' @return A tibble with 1 row of dummy reference data
#' @importFrom tibble add_row
#' @import eDataDRF
#' @export
dummy_references_tibble <- function() {
  initialise_references_tibble() |>
    add_row(
      REFERENCE_ID = "REF-001",
      REFERENCE_TYPE = "Journal Article",
      DATA_SOURCE = "Primary",
      AUTHOR = "Smith, J.; Jones, A.; Williams, B.",
      TITLE = "Heavy metal contamination in Norwegian coastal sediments",
      YEAR = 2023L,
      ACCESS_DATE = as.Date(NA),
      PERIODICAL_JOURNAL = "Environmental Science & Technology",
      VOLUME = 57L,
      ISSUE = 12L,
      PUBLISHER = NA_character_,
      INSTITUTION = NA_character_,
      DOI = "10.1021/acs.est.2023.12345",
      URL = NA_character_,
      ISBN_ISSN = NA_character_,
      EDITION = NA_character_,
      DOCUMENT_NUMBER = NA_character_,
      REF_COMMENT = "Dummy reference for testing"
    )
}

#' Create Dummy Sites Tibble
#'
#' @description Creates a 2-row sites tibble with test data.
#' Inherits column structure from initialise_sites_tibble().
#'
#' @return A tibble with 2 rows of dummy site data
#' @importFrom tibble add_row
#' @import eDataDRF
#' @export
dummy_sites_tibble <- function() {
  initialise_sites_tibble() |>
    add_row(
      SITE_CODE = "SITE-001",
      SITE_NAME = "Oslofjord Inner",
      SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
      SITE_GEOGRAPHIC_FEATURE_SUB = "Water column",
      COUNTRY_ISO = "NO",
      OCEAN_IHO = "Skagerrak",
      LATITUDE = 59.9139,
      LONGITUDE = 10.7522,
      SITE_COORDINATE_SYSTEM = "WGS84",
      ALTITUDE_VALUE = NA_real_,
      ALTITUDE_UNIT = NA_character_,
      ENTERED_BY = "test_user",
      ENTERED_DATE = "2023-07-01",
      SITE_COMMENT = "Primary test site"
    ) |>
    add_row(
      SITE_CODE = "SITE-002",
      SITE_NAME = "Bergen Harbour",
      SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
      SITE_GEOGRAPHIC_FEATURE_SUB = "Sediment",
      COUNTRY_ISO = "NO",
      OCEAN_IHO = "North Sea",
      LATITUDE = 60.3913,
      LONGITUDE = 5.3221,
      SITE_COORDINATE_SYSTEM = "WGS84",
      ALTITUDE_VALUE = NA_real_,
      ALTITUDE_UNIT = NA_character_,
      ENTERED_BY = "test_user",
      ENTERED_DATE = "2023-07-01",
      SITE_COMMENT = "Secondary test site"
    )
}

#' Create Dummy Parameters Tibble
#'
#' @description Creates a 3-row parameters tibble with test data.
#' Inherits column structure from initialise_parameters_tibble().
#'
#' @return A tibble with 3 rows of dummy parameter data
#' @import eDataDRF
#' @importFrom tibble add_row
#' @export
dummy_parameters_tibble <- function() {
  initialise_parameters_tibble() |>
    add_row(
      PARAMETER_TYPE = "Stressor",
      PARAMETER_TYPE_SUB = "Inorganic",
      MEASURED_TYPE = "Total",
      PARAMETER_NAME = "Copper",
      PARAMETER_NAME_SUB = NA_character_,
      INCHIKEY_SD = NA_character_,
      PUBCHEM_CID = 23978L,
      CAS_RN = "7440-50-8",
      ENTERED_BY = "test_user",
      PARAMETER_COMMENT = NA_character_
    ) |>
    add_row(
      PARAMETER_TYPE = "Stressor",
      PARAMETER_TYPE_SUB = "Inorganic",
      MEASURED_TYPE = "Total",
      PARAMETER_NAME = "Lead",
      PARAMETER_NAME_SUB = NA_character_,
      INCHIKEY_SD = NA_character_,
      PUBCHEM_CID = 5352425L,
      CAS_RN = "7439-92-1",
      ENTERED_BY = "test_user",
      PARAMETER_COMMENT = NA_character_
    ) |>
    add_row(
      PARAMETER_TYPE = "Stressor",
      PARAMETER_TYPE_SUB = "Inorganic",
      MEASURED_TYPE = "Total",
      PARAMETER_NAME = "Zinc",
      PARAMETER_NAME_SUB = NA_character_,
      INCHIKEY_SD = NA_character_,
      PUBCHEM_CID = 23994L,
      CAS_RN = "7440-66-6",
      ENTERED_BY = "test_user",
      PARAMETER_COMMENT = NA_character_
    )
}

#' Create Dummy Compartments Tibble
#'
#' @description Creates a 2-row compartments tibble with test data.
#' Inherits column structure from initialise_compartments_tibble().
#'
#' @return A tibble with 2 rows of dummy compartment data
#' @import eDataDRF
#' @importFrom tibble add_row
#' @export
dummy_compartments_tibble <- function() {
  initialise_compartments_tibble() |>
    add_row(
      ENVIRON_COMPARTMENT = "Aquatic",
      ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
      MEASURED_CATEGORY = "External"
    ) |>
    add_row(
      ENVIRON_COMPARTMENT = "Biota",
      ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
      MEASURED_CATEGORY = "Internal"
    )
}

#' Create Dummy Methods Tibble
#'
#' @description Creates a 4-row methods tibble with test data (one per protocol category).
#' Inherits column structure from initialise_methods_tibble().
#'
#' @return A tibble with 4 rows of dummy methods data
#' @importFrom tibble add_row
#' @import eDataDRF
#' @export
dummy_methods_tibble <- function() {
  initialise_methods_tibble() |>
    add_row(
      PROTOCOL_ID = "PROT-001",
      CAMPAIGN_NAME = "Test Campaign 2023: Heavy Metals in Coastal Sediments",
      PROTOCOL_CATEGORY = "Sampling Protocol",
      PROTOCOL_NAME = "Grab sampling",
      PROTOCOL_COMMENT = "Surface sediment grab samples collected by Van Veen grab"
    ) |>
    add_row(
      PROTOCOL_ID = "PROT-002",
      CAMPAIGN_NAME = "Test Campaign 2023: Heavy Metals in Coastal Sediments",
      PROTOCOL_CATEGORY = "Extraction Protocol",
      PROTOCOL_NAME = "Acid digestion",
      PROTOCOL_COMMENT = "HNO3/HCl microwave-assisted digestion"
    ) |>
    add_row(
      PROTOCOL_ID = "PROT-003",
      CAMPAIGN_NAME = "Test Campaign 2023: Heavy Metals in Coastal Sediments",
      PROTOCOL_CATEGORY = "Fractionation Protocol",
      PROTOCOL_NAME = "Total",
      PROTOCOL_COMMENT = NA_character_
    ) |>
    add_row(
      PROTOCOL_ID = "PROT-004",
      CAMPAIGN_NAME = "Test Campaign 2023: Heavy Metals in Coastal Sediments",
      PROTOCOL_CATEGORY = "Analytical Protocol",
      PROTOCOL_NAME = "ICP-MS",
      PROTOCOL_COMMENT = "Inductively coupled plasma mass spectrometry"
    )
}

#' Create Dummy Samples Tibble
#'
#' @description Creates a 2-row samples tibble with test data.
#' Inherits column structure from initialise_samples_tibble().
#'
#' @return A tibble with 2 rows of dummy sample data
#' @importFrom tibble add_row
#' @import eDataDRF
#' @export
dummy_samples_tibble <- function() {
  initialise_samples_tibble() |>
    add_row(
      SITE_CODE = "SITE-001",
      SITE_NAME = "Oslofjord Inner",
      PARAMETER_NAME = "Copper",
      PARAMETER_TYPE = "Stressor",
      ENVIRON_COMPARTMENT = "Aquatic",
      ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
      MEASURED_CATEGORY = "External",
      SAMPLING_DATE = "2023-03-15",
      SUBSAMPLE = "1",
      SUBSAMPLE_ID = NA_character_,
      SAMPLE_ID = "SAMP-001"
    ) |>
    add_row(
      SITE_CODE = "SITE-002",
      SITE_NAME = "Bergen Harbour",
      PARAMETER_NAME = "Lead",
      PARAMETER_TYPE = "Stressor",
      ENVIRON_COMPARTMENT = "Aquatic",
      ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
      MEASURED_CATEGORY = "External",
      SAMPLING_DATE = "2023-04-20",
      SUBSAMPLE = "1",
      SUBSAMPLE_ID = NA_character_,
      SAMPLE_ID = "SAMP-002"
    )
}

#' Create Dummy Biota Tibble
#'
#' @description Creates a 1-row biota tibble with test data.
#' Inherits column structure from initialise_biota_tibble().
#'
#' @return A tibble with 1 row of dummy biota data
#' @importFrom tibble add_row
#' @import eDataDRF
#' @export
dummy_biota_tibble <- function() {
  initialise_biota_tibble() |>
    add_row(
      SAMPLE_ID = "SAMP-003",
      SITE_CODE = "SITE-001",
      PARAMETER_NAME = "Copper",
      ENVIRON_COMPARTMENT = "Biota",
      ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
      MEASURED_CATEGORY = "Internal",
      SAMPLING_DATE = "2023-05-10",
      SUBSAMPLE = "1",
      SPECIES_GROUP = "Fish",
      SAMPLE_SPECIES = "Gadus morhua",
      SAMPLE_TISSUE = "Liver",
      SAMPLE_SPECIES_LIFESTAGE = "Adult",
      SAMPLE_SPECIES_GENDER = "Female",
      BIOTA_COMMENT = "Atlantic cod liver sample"
    )
}

#' Create Dummy Measurements Tibble
#'
#' @description Creates a 3-row measurements tibble with test data.
#' Inherits column structure from initialise_measurements_tibble().
#'
#' @return A tibble with 3 rows of dummy measurement data
#' @import eDataDRF
#' @importFrom tibble add_row
#' @export
dummy_measurements_tibble <- function() {
  initialise_measurements_tibble() |>
    add_row(
      SITE_CODE = "SITE-001",
      PARAMETER_NAME = "Copper",
      SAMPLING_DATE = "2023-03-15",
      ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
      SUBSAMPLE = "1",
      MEASURED_FLAG = NA_character_,
      MEASURED_VALUE = 45.2,
      UNCERTAINTY_TYPE = "SD",
      UNCERTAINTY_UPPER = 3.1,
      UNCERTAINTY_LOWER = 3.1,
      MEASURED_UNIT = "mg/kg",
      MEASURED_N = 3,
      LOQ_VALUE = 0.5,
      LOQ_UNIT = "mg/kg",
      LOD_VALUE = 0.1,
      LOD_UNIT = "mg/kg",
      SAMPLING_PROTOCOL = "Grab sampling",
      EXTRACTION_PROTOCOL = "Acid digestion",
      FRACTIONATION_PROTOCOL = "Total",
      ANALYTICAL_PROTOCOL = "ICP-MS",
      REFERENCE_ID = "REF-001",
      SAMPLE_ID = "SAMP-001",
      CAMPAIGN_NAME_SHORT = "TestCamp2023",
      ENVIRON_COMPARTMENT = "Aquatic",
      PARAMETER_TYPE = "Stressor",
      MEASURED_TYPE = "Total",
      MEASUREMENT_COMMENT = NA_character_
    ) |>
    add_row(
      SITE_CODE = "SITE-002",
      PARAMETER_NAME = "Lead",
      SAMPLING_DATE = "2023-04-20",
      ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
      SUBSAMPLE = "1",
      MEASURED_FLAG = NA_character_,
      MEASURED_VALUE = 12.8,
      UNCERTAINTY_TYPE = "SD",
      UNCERTAINTY_UPPER = 1.2,
      UNCERTAINTY_LOWER = 1.2,
      MEASURED_UNIT = "mg/kg",
      MEASURED_N = 3,
      LOQ_VALUE = 1.0,
      LOQ_UNIT = "mg/kg",
      LOD_VALUE = 0.3,
      LOD_UNIT = "mg/kg",
      SAMPLING_PROTOCOL = "Grab sampling",
      EXTRACTION_PROTOCOL = "Acid digestion",
      FRACTIONATION_PROTOCOL = "Total",
      ANALYTICAL_PROTOCOL = "ICP-MS",
      REFERENCE_ID = "REF-001",
      SAMPLE_ID = "SAMP-002",
      CAMPAIGN_NAME_SHORT = "TestCamp2023",
      ENVIRON_COMPARTMENT = "Aquatic",
      PARAMETER_TYPE = "Stressor",
      MEASURED_TYPE = "Total",
      MEASUREMENT_COMMENT = NA_character_
    ) |>
    add_row(
      SITE_CODE = "SITE-001",
      PARAMETER_NAME = "Copper",
      SAMPLING_DATE = "2023-05-10",
      ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
      SUBSAMPLE = "1",
      MEASURED_FLAG = "< LOQ",
      MEASURED_VALUE = NA_real_,
      UNCERTAINTY_TYPE = NA_character_,
      UNCERTAINTY_UPPER = NA_real_,
      UNCERTAINTY_LOWER = NA_real_,
      MEASURED_UNIT = "mg/kg ww",
      MEASURED_N = 1,
      LOQ_VALUE = 0.05,
      LOQ_UNIT = "mg/kg ww",
      LOD_VALUE = 0.01,
      LOD_UNIT = "mg/kg ww",
      SAMPLING_PROTOCOL = "Grab sampling",
      EXTRACTION_PROTOCOL = "Acid digestion",
      FRACTIONATION_PROTOCOL = "Total",
      ANALYTICAL_PROTOCOL = "ICP-MS",
      REFERENCE_ID = "REF-001",
      SAMPLE_ID = "SAMP-003",
      CAMPAIGN_NAME_SHORT = "TestCamp2023",
      ENVIRON_COMPARTMENT = "Biota",
      PARAMETER_TYPE = "Stressor",
      MEASURED_TYPE = "Total",
      MEASUREMENT_COMMENT = "Below LOQ - biota sample"
    )
}


# =========================================================================
# SESSION DATA CREATION
# =========================================================================

#' Create Dummy Session Data
#'
#' @description Creates a complete userData-like list structure populated with
#' dummy data from all dummy_*_tibble() functions. This mirrors the structure
#' created by initialise_userData() but with test data instead of empty tibbles.
#'
#' Useful for testing outside of a reactive context.
#'
#' @return A list matching the structure of initialise_userData() with dummy data
#' @export
create_dummy_session_data <- function() {
  # Start from the proper userData structure
  userData <- initialise_userData()

  # Populate with dummy data ----
  userData$campaignData <- dummy_campaign_tibble()
  userData$referenceData <- dummy_references_tibble()
  userData$sitesData <- dummy_sites_tibble()
  userData$parametersData <- dummy_parameters_tibble()
  userData$compartmentsData <- dummy_compartments_tibble()
  userData$methodsData <- dummy_methods_tibble()
  userData$samplesData <- dummy_samples_tibble()
  userData$biotaData <- dummy_biota_tibble()
  userData$measurementsData <- dummy_measurements_tibble()

  # Set validity flags to TRUE since we have data ----
  userData$campaignDataValid <- TRUE
  userData$referenceDataValid <- TRUE
  userData$sitesDataValid <- TRUE
  userData$parametersDataValid <- TRUE
  userData$compartmentsDataValid <- TRUE
  userData$methodsDataValid <- TRUE
  userData$samplesDataValid <- TRUE
  userData$biotaDataValid <- TRUE
  userData$measurementsDataValid <- TRUE

  userData
}


# =========================================================================
# LEGACY FUNCTIONS (kept for backwards compatibility)
# =========================================================================

#' Create dummy data for testing and demonstration
#'
#' @description Creates a complete set of dummy environmental exposure data
#' that can be used for testing the application or demonstration purposes.
#' This includes campaign info, references, sites, parameters, compartments,
#' biota, and methods data. This returns a list because that's the format we
#' expect to get structured data back from the LLM in, even though we prefer
#' tibbles
#'
#' @param uppercase_columns Logical. If TRUE, converts data frame column names
#'   to uppercase for app data structures. If FALSE, keeps lowercase for LLM extraction.
#'
#' @importFrom tibble tibble is_tibble
#' @return List containing all dummy data structures
#' @export
create_dummy_data <- function(uppercase_columns = FALSE) {
  dummy_data <- list(
    campaign = tibble(
      # TODO: Actually, should this be a tibble? Need to coordinate with other stuff.
      campaign_name = "Dummy campaign, 1997",
      campaign_name_short = "DummyCampaign1997",
      campaign_start_date = "1997-01-01",
      campaign_end_date = "1997-03-31",
      organisation = "NIVA",
      campaign_comment = "A madeup NIVA study."
    ),
    references = tibble(
      author = "Welch, S.",
      title = "Study madeup",
      year = 1998L,
      periodical_journal = "Journal of NIVA",
      volume = 43L,
      issue = 2L,
      publisher = "NIVA Library",
      doi = NA_character_
    ),
    sites = tibble(
      site_code = "NIVA-001",
      site_name = "NIVA Office",
      latitude = 59.924634,
      longitude = 10.792297,
      country = "Norway",
      site_geographic_feature = "Coastal, fjord",
      site_geographic_feature_sub = "Water column"
    ),
    parameters = tibble(
      parameter_name = c("Silver"),
      parameter_type = rep("Stressor", 1),
      cas_rn = c(
        "7440-22-4"
      )
    ),
    compartments = tibble(
      environ_compartment = c("Aquatic", "Biota"),
      environ_compartment_sub = c("Marine/Salt Water", "Biota, Aquatic"),
      measured_category = c("External", "Internal")
    ),
    biota = tibble(
      sample_id = NA_character_,
      species_group = "Crustaceans",
      sample_species = "Daphnia magna",
      sample_tissue = "Whole body",
      sample_species_lifestage = "Adult",
      sample_species_gender = "Female"
    ),
    methods = tibble(
      protocol_category = c(
        "Sampling Protocol",
        "Analytical Protocol",
        "Extraction Protocol"
      ),
      protocol_name = c(
        "Grab sampling",
        "Other",
        "Not reported"
      ),
      protocol_comment = c(
        "Adult copepods collected between January and March 1997, acclimated to 15°C for 12 days",
        "Radioactivity measured with NaI(Tl) gamma detectors at specific energy levels for each isotope",
        ""
      )
    ),
    samples = tibble(
      sampling_date = c("2025-09-29", "2023-02-12")
    )
  )

  # Convert data frame column names to uppercase if requested
  if (uppercase_columns) {
    tibble_elements <- c(
      "sites",
      "parameters",
      "compartments",
      "biota",
      "methods"
    )
    for (element in tibble_elements) {
      if (!is.null(dummy_data[[element]]) && is_tibble(dummy_data[[element]])) {
        names(dummy_data[[element]]) <- toupper(names(dummy_data[[element]]))
      }
    }
  }

  return(dummy_data)
}

#' Populate session data directly with dummy data
#'
#' @description Stores dummy data directly into session reactiveValues.
#' This bypasses the LLM extraction process and populates all module
#' data objects immediately.
#'
#' @param session Shiny session object
#' @param navigate_to Optional tab to navigate to after loading data
#' @param parent_session Parent session for navigation (if different from session)
#'
#' @importFrom shiny showNotification updateNavbarPage
#' @importFrom golem print_dev
#' @export
populate_session_with_dummy_data <- function(
  session,
  navigate_to = NULL,
  parent_session = NULL
) {
  # Create dummy data with uppercase columns for app data structures
  dummy_data <- create_dummy_data(uppercase_columns = TRUE)

  # Store directly in session userData for immediate use
  # Campaign data
  if (!is.null(dummy_data$campaign)) {
    session$userData$reactiveValues$campaignData <- dummy_data$campaign
    print_dev("Populated campaign data with dummy data")
  }

  # References data
  if (!is.null(dummy_data$references)) {
    session$userData$reactiveValues$referenceData <- dummy_data$references
    print_dev("Populated references data with dummy data")
  }

  # Sites data
  if (!is.null(dummy_data$sites)) {
    session$userData$reactiveValues$sitesData <- dummy_data$sites
    print_dev("Populated sites data with dummy data")
  }

  # Parameters data
  if (!is.null(dummy_data$parameters)) {
    session$userData$reactiveValues$parametersData <- dummy_data$parameters
    print_dev("Populated parameters data with dummy data")
  }

  # Compartments data
  if (!is.null(dummy_data$compartments)) {
    session$userData$reactiveValues$compartmentsData <- dummy_data$compartments
    print_dev("Populated compartments data with dummy data")
  }

  # Biota data
  if (!is.null(dummy_data$biota)) {
    session$userData$reactiveValues$biotaData <- dummy_data$biota
    print_dev("Populated biota data with dummy data")
  }

  # Methods data
  if (!is.null(dummy_data$methods)) {
    session$userData$reactiveValues$methodsData <- dummy_data$methods
    print_dev("Populated methods data with dummy data")
  }

  # Set status flags
  session$userData$reactiveValues$dummyDataLoaded <- TRUE

  showNotification(
    "Dummy data loaded successfully! All modules now contain test data.",
    type = "default"
  )

  # Navigate if requested
  if (!is.null(navigate_to) && !is.null(parent_session)) {
    updateNavbarPage(
      session = parent_session,
      inputId = "main-page",
      selected = navigate_to
    )
  }

  print_dev("Dummy data population complete")
}
