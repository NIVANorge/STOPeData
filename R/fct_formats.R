# Initialise tibbles for each "table" of data used across the modules.
# Doing this in one place makes it easier (but not easy) to keep things consistent.
# The plan is that eventually all functions that are dependent on table format will
# be based in some way on these functions. That may prove to be impractical. But it's a start.
# I also mention a bunch of database TABLES. These don't actually exist yet, but are
# useful for understanding the structure of the data
# eData DRF Version - Not Tracked
# Last Updated 2025.11.17

# -----------------------
# ---- userData ----
# -----------------------

# making this
initialise_userData <- function() {
  list(
    ENTERED_BY = character(0),
    autosave_enabled = FALSE,

    # Standard validated data ----
    # All userData and module_state$data data is stored in a tabular (tibble) format centrally, even for campaign and reference (which currently only have one row)
    # This means we can use a consistent set of functions to check for presence (nrow(tibble) > 0), and not have any nasty surprises when we expect one and get the other
    # Data entry modules
    sitesData = initialise_sites_tibble(),
    sitesDataValid = FALSE,
    parametersData = initialise_parameters_tibble(),
    parametersDataValid = FALSE,
    compartmentsData = initialise_compartments_tibble(),
    compartmentsDataValid = FALSE,
    referenceData = initialise_references_tibble(),
    referenceDataValid = FALSE,
    campaignData = initialise_campaign_tibble(),
    campaignDataValid = FALSE,
    methodsData = initialise_methods_tibble(),
    methodsDataValid = FALSE,
    samplesData = initialise_samples_tibble(),
    samplesDataValid = FALSE,
    biotaData = initialise_biota_tibble(),
    biotaDataValid = FALSE,
    samplesDataWithBiota = tibble(NULL),
    measurementsData = initialise_measurements_tibble(),
    measurementsDataValid = FALSE,

    # CREED Data
    datasetDetails = tibble(NULL),
    creedRelevance = initialise_CREED_data_tibble(),
    creedReliability = initialise_CREED_data_tibble(),
    creedScores = initialise_CREED_scores_tibble(),
    creedReport = "",

    # CREED reactive objects that just exist to trigger reactivity. Probably bad coding!
    creedGetData = 0, # watched by multiple observers in nested CREED modules. +1 every time we input$get_data in mod_CREED
    creedCalculateScores = 0, # same

    # LLM extracted data and metadata ----
    schemaLLM = "",
    promptLLM = "",
    rawLLM = "",
    pdfPath = NULL,
    campaignDataLLM = tibble(NULL),
    referenceDataLLM = tibble(NULL),
    sitesDataLLM = tibble(NULL),
    parametersDataLLM = tibble(NULL),
    compartmentsDataLLM = tibble(NULL),
    methodsDataLLM = tibble(NULL),
    samplesDataLLM = tibble(NULL),
    biotaDataLLM = tibble(NULL),
    samplesDataLLM = tibble(NULL),

    # LLM extraction status flags ----
    llmExtractionComplete = FALSE,
    llmExtractionSuccessful = FALSE,
    llmExtractionComments = tibble(NULL),

    # Import data from save status flags ----
    saveExtractionComplete = FALSE,
    saveExtractionSuccessful = FALSE
  )
}


# -----------------------
# ---- TABLE FORMATS ----
# -----------------------

#' Initialise Campaign Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for campaign data.
#' Campaigns represent sampling projects or studies with metadata about timing,
#' organization, data quality evaluation, and confidentiality.
#'
#' @return A tibble with 0 rows and standardised campaign columns
#' @importFrom tibble tibble
#' @export
initialise_campaign_tibble <- function() {
  # Creates the CAMAPIGN table
  tibble(
    CAMPAIGN_NAME_SHORT = character(),
    CAMPAIGN_NAME = character(),
    CAMPAIGN_START_DATE = as.Date(character()),
    CAMPAIGN_END_DATE = as.Date(character()),
    RELIABILITY_SCORE = character(),
    RELIABILITY_EVAL_SYS = character(),
    CONFIDENTIALITY_EXPIRY_DATE = as.Date(character()),
    ORGANISATION = character(),
    ENTERED_BY = character(),
    ENTERED_DATE = as.Date(character()),
    CAMPAIGN_COMMENT = character()
  )
}

#' Initialise Biota Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for biota data.
#' Biota data extends sample information with species-specific details including
#' taxonomic classification, tissue type, life stage, and gender information.
#'
#' @return A tibble with 0 rows and standardised biota columns
#' @importFrom tibble tibble
#' @export
initialise_biota_tibble <- function() {
  # Used to construct the SAMPLES table if appropriate when biota are measured
  tibble(
    SAMPLE_ID = character(),
    SITE_CODE = character(),
    PARAMETER_NAME = character(),
    ENVIRON_COMPARTMENT = character(),
    ENVIRON_COMPARTMENT_SUB = character(),
    MEASURED_CATEGORY = character(),
    SAMPLING_DATE = character(),
    SUBSAMPLE = character(),
    SPECIES_GROUP = character(),
    SAMPLE_SPECIES = character(),
    SAMPLE_TISSUE = character(),
    SAMPLE_SPECIES_LIFESTAGE = character(),
    SAMPLE_SPECIES_GENDER = character(),
    BIOTA_COMMENT = character()
  )
}


#' Initialise Compartments Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for environmental
#' compartment data. Compartments define the environmental matrix and measurement
#' context for sampling activities.
#' One immediate child: create_compartment_combination() in mod_compartments_fct.R
#'
#' @return A tibble with 0 rows and standardised compartment columns
#' @importFrom tibble tibble
#' @export
initialise_compartments_tibble <- function() {
  # Used to construct the SAMPLES table
  tibble(
    ENVIRON_COMPARTMENT = character(),
    ENVIRON_COMPARTMENT_SUB = character(),
    MEASURED_CATEGORY = character()
  )
}

#' Initialise Methods Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for analytical
#' methods data. Methods describe the protocols used for sampling, extraction,
#' fractionation, and analysis procedures.
#'
#' @return A tibble with 0 rows and standardised methods columns
#' @importFrom tibble tibble
#' @export
initialise_methods_tibble <- function() {
  # METHODS table
  tibble(
    PROTOCOL_ID = character(), # Primary Key
    CAMPAIGN_NAME = character(),
    PROTOCOL_CATEGORY = character(),
    PROTOCOL_NAME = character(),
    PROTOCOL_COMMENT = character()
  )
}

#' Initialise Parameters Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for parameter data.
#' Parameters define chemical substances, physical properties, or biological markers
#' being measured, including classification and chemical identifiers.
#' Two immediate children: create_new_parameter() and create_existing_parameter() in mod_parameters_fct.R
#'
#' @return A tibble with 0 rows and standardised parameter columns
#' @importFrom tibble tibble
#' @export
initialise_parameters_tibble <- function() {
  # PARAMETERS table
  tibble(
    PARAMETER_TYPE = character(),
    PARAMETER_TYPE_SUB = character(),
    MEASURED_TYPE = character(),
    PARAMETER_NAME = character(), # Primary Key
    PARAMETER_NAME_SUB = character(),
    INCHIKEY_SD = character(),
    PUBCHEM_CID = integer(),
    CAS_RN = character(),
    ENTERED_BY = character(),
    PARAMETER_COMMENT = character()
  )
}

#' Initialise References Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for reference data.
#' References provide bibliographic information for data sources including journals,
#' reports, datasets, and other published materials.
#'
#' @return A tibble with 0 rows and standardised reference columns
#' @importFrom tibble tibble
#' @export
initialise_references_tibble <- function() {
  # REFERENCES table
  tibble(
    REFERENCE_ID = character(), # Primary Key
    REFERENCE_TYPE = character(),
    DATA_SOURCE = character(),
    AUTHOR = character(),
    TITLE = character(),
    YEAR = integer(),
    ACCESS_DATE = as.Date(character()),
    PERIODICAL_JOURNAL = character(),
    VOLUME = integer(),
    ISSUE = integer(),
    PUBLISHER = character(),
    INSTITUTION = character(),
    DOI = character(),
    URL = character(),
    ISBN_ISSN = character(),
    EDITION = character(),
    DOCUMENT_NUMBER = character(),
    REF_COMMENT = character()
  )
}

#' Initialise Samples Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for sample data.
#' Samples represent individual collections from sites with temporal, spatial,
#' and methodological information linking sites, parameters, and compartments.
#'
#' @return A tibble with 0 rows and standardised sample columns
#' @importFrom tibble tibble
#' @export
initialise_samples_tibble <- function() {
  # Used to construct MEASUREMENTS table
  tibble(
    SITE_CODE = character(),
    SITE_NAME = character(),
    PARAMETER_NAME = character(),
    PARAMETER_TYPE = character(),
    ENVIRON_COMPARTMENT = character(),
    ENVIRON_COMPARTMENT_SUB = character(),
    MEASURED_CATEGORY = character(),
    SAMPLING_DATE = character(),
    SUBSAMPLE = character(),
    SUBSAMPLE_ID = character(),
    SAMPLE_ID = character() # Key
  )
}

#' Initialise Sites Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for site data.
#' Sites represent sampling locations with geographic coordinates, administrative
#' boundaries, and descriptive metadata about the sampling location.
#'
#' @return A tibble with 0 rows and standardised site columns
#' @importFrom tibble tibble
#' @export
initialise_sites_tibble <- function() {
  # Used to construct SITES table
  tibble(
    SITE_CODE = character(), # Primary Key
    SITE_NAME = character(),
    SITE_GEOGRAPHIC_FEATURE = character(),
    SITE_GEOGRAPHIC_FEATURE_SUB = character(),
    COUNTRY_ISO = character(),
    OCEAN_IHO = character(),
    LATITUDE = numeric(),
    LONGITUDE = numeric(),
    SITE_COORDINATE_SYSTEM = character(),
    ALTITUDE_VALUE = numeric(),
    ALTITUDE_UNIT = character(),
    ENTERED_BY = character(),
    ENTERED_DATE = character(),
    SITE_COMMENT = character()
  )
}

#' Initialise Measurements Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for measurements data.
#'
#' @return A tibble with 0 rows and standardised measurement columns
#' @importFrom tibble tibble
#' @export
## Initialise measurement combinations data frame ----
initialise_measurements_tibble <- function() {
  # Used to construct MEASURMENTS table
  # Merges SAMPLES, COMPARTMENTS, BIOTA
  tibble(
    SITE_CODE = character(),
    PARAMETER_NAME = character(), # Foreign key
    SAMPLING_DATE = character(),
    ENVIRON_COMPARTMENT_SUB = character(),
    SUBSAMPLE = character(),
    MEASURED_FLAG = character(),
    MEASURED_VALUE = numeric(),
    UNCERTAINTY_TYPE = character(),
    UNCERTAINTY_UPPER = numeric(),
    UNCERTAINTY_LOWER = numeric(),
    MEASURED_UNIT = character(),
    MEASURED_N = numeric(),
    LOQ_VALUE = numeric(),
    LOQ_UNIT = character(),
    LOD_VALUE = numeric(),
    LOD_UNIT = character(),
    SAMPLING_PROTOCOL = character(), # Foreign key
    EXTRACTION_PROTOCOL = character(), # Foreign key
    FRACTIONATION_PROTOCOL = character(), # Foreign key
    ANALYTICAL_PROTOCOL = character(), # Foreign key
    # These variables are not immediately relevant to measurement entry, so we shuffle them to the back
    REFERENCE_ID = character(), # Foreign key
    SAMPLE_ID = character(),
    CAMPAIGN_NAME_SHORT = character(),
    ENVIRON_COMPARTMENT = character(),
    PARAMETER_TYPE = character(),
    MEASURED_TYPE = character(),
    MEASUREMENT_COMMENT = character()
  )
}

#' Initialise CREED Scores Tibble
#'
#' Creates an empty tibble with the standardised column structure for CREED scores.
#'
#' @return A tibble with 0 rows and standardised CREED columns
#' @importFrom tibble tibble
#' @export
initialise_CREED_scores_tibble <- function() {
  # Used to construct CREED SCORES table
  tibble(
    REFERENCE_ID = character(),
    SILVER_RELIABILITY = character(),
    SILVER_RELEVANCE = character(),
    GOLD_RELIABILITY = character(),
    GOLD_RELEVANCE = character()
  )
}

#' Initialise CREED Scores Tibble
#'
#' Creates an empty tibble with the standardised column structure for CREED
#' criterion scores. Used by both reliability and relevance modules to ensure
#' consistent data structure. This isn't a part of the externally-available
#' table structure, but we want to make sure it's harmonised locally anyway
#'
#' @return A tibble with 0 rows and standardised CREED score columns
#' @importFrom tibble tibble
#' @export
initialise_CREED_data_tibble <- function() {
  tibble(
    criterion_id = character(),
    criterion_title = character(),
    required_recommended = character(),
    relevant_data = character(),
    score = character(),
    limitations = character()
  )
}

# ------------------------
# ------ VOCABULARY ------
# ------------------------

#' Geographic Features Controlled Vocabulary
#'
#' Returns controlled vocabulary options for geographic features.
#'
#' @return A character vector of geographic feature options
#' @export
#' @import ISOcodes
geographic_features_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    "River, stream, canal",
    "Lake, pond, pool, reservoir",
    "Ocean, sea, territorial waters",
    "Coastal, fjord",
    "Estuary",
    "Drainage, sewer, artificial water",
    "Swamp, wetland",
    "Groundwater, aquifer",
    "WWTP",
    "Artificial Land/Urban Areas",
    "Landfills",
    "Cropland",
    "Woodland, forest",
    "Shrubland",
    "Grassland",
    "Bare land and lichen/moss",
    "Glacier",
    "Other"
  )
}

#' Geographic Features Sub Controlled Vocabulary
#'
#' Returns controlled vocabulary options for geographic feature subcategories.
#'
#' @return A character vector of geographic feature subcategory options
#' @export
geographic_features_sub_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    "Water surface",
    "Water column, pelagic zone",
    "Water benthos",
    "Other"
  )
}

#' Coordinate Systems Controlled Vocabulary
#'
#' Returns controlled vocabulary options for coordinate systems.
#'
#' @return A character vector of coordinate system options
#' @export
coordinate_systems_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    "WGS 84",
    "UTM 32",
    "UTM 33",
    "UTM 34",
    "UTM 35",
    "ETRS89",
    "Other"
  )
}

#' Countries Controlled Vocabulary
#'
#' Returns controlled vocabulary options for countries.
#'
#' @return A character vector of country options
#' @export
#' @import ISOcodes
countries_vocabulary <- function() {
  c(
    "Not relevant",
    "Not reported",
    ISOcodes::ISO_3166_1$Name
  )
}

#' Areas Controlled Vocabulary
#'
#' Returns controlled vocabulary options for areas.
#'
#' @return A character vector of area options
#' @export
#' @importFrom dplyr pull
areas_vocabulary <- function() {
  IHO_oceans <- readRDS("inst/data/clean/IHO_oceans.rds") |> pull(NAME)

  c(
    "Not relevant",
    "Not reported",
    "Other",
    IHO_oceans
  )
}

#' Altitude Units Controlled Vocabulary
#'
#' Returns controlled vocabulary options for altitude units.
#'
#' @return A character vector of altitude unit options
#' @export
altitude_units_vocabulary <- function() {
  c("km", "m", "cm", "mm")
}

#' Dummy Parameters Data
#'
#' Returns dummy parameter data combining quality parameters and chemical parameters.
#'
#' @return A data frame of dummy parameter data
#' @export
#' @import dplyr
#' @importFrom arrow read_parquet
# TODO: We call this "dummy" data for the simple reason that I've never looked for or made my own comprehensive list of quality parameters.
dummy_parameters_vocabulary <- function() {
  # Read dummy_parameters ----
  dummy_quality_params <- read_parquet(
    file = "inst/data/clean/dummy_quality_parameters.parquet"
  ) |>
    mutate(ENTERED_BY = "saw@niva.no")

  # Read and prepare chemical_parameters ----
  chemical_parameters <- read_parquet(
    file = "inst/data/clean/ClassyFire_Taxonomy_2025_02.parquet"
  ) |>
    mutate(
      MEASURED_TYPE = "Concentration",
      ENTERED_BY = "saw@niva.no"
    ) |>
    arrange(PARAMETER_NAME) |>
    mutate(
      PARAMETER_TYPE_SUB = case_when(
        PARAMETER_NAME == "Carbon" ~ "Carbon",
        TRUE ~ PARAMETER_TYPE_SUB
      ),
      PARAMETER_NAME_SUB = "",
      CAS_RN = as.character(CAS_RN)
    )

  # Merge datasets ----
  bind_rows(dummy_quality_params, chemical_parameters)
}

#' Parameter Types Controlled Vocabulary
#'
#' Returns controlled vocabulary options for parameter types.
#'
#' @return A character vector of parameter type options
#' @export
parameter_types_vocabulary <- function() {
  c(
    "Not relevant",
    "Stressor",
    "Quality parameter",
    "Normalization",
    "Background",
    "Ecological Indicator",
    "Other"
  )
}

#' Parameter Types Sub Controlled Vocabulary
#'
#' Returns controlled vocabulary options for parameter type subcategories.
#'
#' @return A character vector of parameter type subcategory options
#' @export
#' @import dplyr
#' @importFrom arrow read_parquet
parameter_types_sub_vocabulary <- function() {
  dummy_parameters <- dummy_parameters_vocabulary()

  dummy_parameters |>
    select(PARAMETER_TYPE_SUB) |>
    distinct() |>
    arrange(PARAMETER_TYPE_SUB) |>
    pull(PARAMETER_TYPE_SUB) |>
    append(c("Mixture", "Not reported"))
}

#' Measured Types Controlled Vocabulary
#'
#' Returns controlled vocabulary options for measured types.
#'
#' @return A character vector of measured type options
#' @export
measured_types_vocabulary <- function() {
  c(
    "Concentration",
    "Dose rate",
    "Dose",
    "Physical parameter",
    "Amount",
    "Volume",
    "Fraction of total",
    "Percent",
    "Irradiance",
    "Response",
    "Ecological Indicator",
    "Not relevant",
    "Other"
  )
}

#' Sub-compartment Options Mapping
#'
#' Returns controlled vocabulary mapping for environmental sub-compartments organized by main compartment.
#'
#' @return A named list of character vectors with sub-compartment options for each main compartment
#' @export
environ_compartments_sub_vocabulary <- function() {
  list(
    "Aquatic" = c(
      "Freshwater" = "Freshwater",
      "Marine/Salt Water" = "Marine/Salt Water",
      "Brackish/Transitional Water" = "Brackish/Transitional Water",
      "Groundwater" = "Groundwater",
      "Wastewater" = "Wastewater",
      "Liquid Growth Medium" = "Liquid Growth Medium",
      "Rainwater" = "Rainwater",
      "Stormwater" = "Stormwater",
      "Leachate" = "Leachate",
      "Aquatic Sediment" = "Aquatic Sediment",
      "Porewater" = "Porewater",
      "Sludge" = "Sludge"
    ),
    "Atmospheric" = c(
      "Indoor Air" = "Indoor Air",
      "Outdoor Air" = "Outdoor Air"
    ),
    "Terrestrial" = c(
      "Terrestrial Biological Residue" = "Terrestrial Biological Residue",
      "Soil H Horizon (Peat)" = "Soil H Horizon (Peat)",
      "Soil O Horizon (Organic)" = "Soil O Horizon (Organic)",
      "Soil A Horizon (Topsoil)" = "Soil A Horizon (Topsoil)",
      "Soil E Horizon (Mineral)" = "Soil E Horizon (Mineral)",
      "Soil S Horizon (Mineral)" = "Soil S Horizon (Mineral)",
      "Soil C Horizon (Parent Material)" = "Soil C Horizon (Parent Material)",
      "Soil R Horizon (Bedrock)" = "Soil R Horizon (Bedrock)"
    ),
    "Biota" = c(
      "Biota, Terrestrial" = "Biota, Terrestrial",
      "Biota, Aquatic" = "Biota, Aquatic",
      "Biota, Atmospheric" = "Biota, Atmospheric",
      "Biota, Other" = "Biota, Other"
    )
  )
}

#' Environmental Compartments Controlled Vocabulary
#'
#' Returns controlled vocabulary options for environmental compartments.
#'
#' @return A character vector of environmental compartment options
#' @export
environ_compartments_vocabulary <- function() {
  c(
    "Aquatic",
    "Atmospheric",
    "Terrestrial",
    "Biota",
    "Not relevant",
    "Not reported",
    "Other"
  )
}

#' Measured Categories Controlled Vocabulary
#'
#' Returns controlled vocabulary options for measured categories.
#'
#' @return A character vector of measured category options
#' @export
measured_categories_vocabulary <- function() {
  c(
    "External" = "External Media",
    "Internal" = "Internal to Organism",
    "Surface" = "Surface of Organism"
  )
}

#' Species Controlled Vocabulary
#'
#' Returns species name options drom ECOTOX data
#'
#' @return A character vector of parameter type subcategory options
#' @export
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom arrow read_parquet
species_names_vocabulary <- function() {
  read_parquet(
    "inst/data/clean/ecotox_2025_06_12_species.parquet"
  ) |>
    mutate(
      SPECIES_COMMON_NAME = common_name,
      SPECIES_NAME = latin_name,
      SPECIES_KINGDOM = kingdom,
      SPECIES_GROUP = species_group,
      .keep = "none"
    ) |>
    bind_rows(tibble(
      SPECIES_COMMON_NAME = c("Other", "Ecosystem"),
      SPECIES_NAME = c("Other", "Ecosystem"),
      SPECIES_KINGDOM = c("Other", "Ecosystem"),
      SPECIES_GROUP = c("Other", "Ecosystem")
    ))
}


#' Initialise Tissue Types Controlled Vocabulary
#'
#' Returns controlled vocabulary options for sample tissue types.
#'
#' @return A character vector of tissue type options
#' @export
tissue_types_vocabulary <- function() {
  c(
    "Not reported",
    "Not relevant",
    "Whole body",
    "Total soft tissues",
    "Muscle",
    "Liver",
    "Kidney",
    "Fat/Adipose",
    "Skin",
    "Bone",
    "Pyloric caeca",
    "Body wall",
    "Brain",
    "Heart",
    "Lung",
    "Gill",
    "Gonad",
    "Shell",
    "Carapace",
    "Blood",
    "Egg",
    "Larva",
    "Leaf",
    "Root",
    "Stem",
    "Fruit",
    "Seed",
    "Other"
  )
}

#' Initialise Life Stages Controlled Vocabulary
#'
#' Returns controlled vocabulary options for sample species life stages.
#'
#' @return A character vector of life stage options
#' @export
lifestage_vocabulary <- function() {
  c(
    "Not reported",
    "Not relevant",
    "Adult",
    "Juvenile",
    "Larva",
    "Embryo",
    "Egg",
    "Seedling",
    "Mature",
    "Young",
    "Mixed",
    "Other"
  )
}

#' Initialise Gender Controlled Vocabulary
#'
#' Returns controlled vocabulary options for sample species gender.
#'
#' @return A character vector of gender options
#' @export
gender_vocabulary <- function() {
  c(
    "Not reported",
    "Not relevant",
    "Male",
    "Female",
    "Mixed",
    "Hermaphrodite",
    "Other"
  )
}

#' Species Groups Controlled Vocabulary
#'
#' Returns controlled vocabulary options for species groups. Taken from EPA ECOTOX db.
#'
#' @return A character vector of species group options
#' @export
species_groups_vocabulary <- function() {
  c(
    "All",
    "Algae",
    "Amphibians",
    "Bacteria",
    "Birds",
    "Crustaceans",
    "Ecosystem",
    "Fish",
    "Fungi",
    "Insects/Spiders",
    "Invertebrates",
    "Mammals",
    "Molluscs",
    "Moss/Hornworts",
    "Plants",
    "Reptiles",
    "Worms",
    "Other"
  )
}

#' Uncertainty Types Controlled Vocabulary
#'
#' Returns controlled vocabulary options for uncertainty types commonly found
#' in scientific literature and databases.
#'
#' @return A character vector of uncertainty type options
#' @export
uncertainty_types_vocabulary <- function() {
  c(
    "Not Reported",
    "Not Relevant",
    "Arithmetic Mean",
    "Geometric Mean",
    "Standard Deviation",
    "Standard Error",
    "95% Confidence Interval",
    "90% Confidence Interval",
    "99% Confidence Interval",
    "Min-Max Range",
    "Interquartile Range (Q1-Q3)",
    "10th-90th Percentile",
    "5th-95th Percentile",
    "Coefficient of Variation (%)",
    "Median Absolute Deviation",
    "Minimum-Maximum",
    "Variance",
    "Standard Error of Mean",
    "Relative Standard Deviation (%)",
    "95% Credible Interval",
    "95% Prediction Interval",
    "95% Tolerance Interval",
    "95% Bootstrap CI",
    "Other"
  )
}

# ------------------------
# --- CHARACTER LIMITS ---
# ------------------------

### Character limit validations for optional fields ----
#' reference_character_limits
#'
#' @returns a list of character limits for fields in mod_references
#'
#' @export
reference_character_limits <- function() {
  list(
    # ACCESSION_NUMBER = 200,  # COMMENTED OUT
    # DB_NAME = 200,           # COMMENTED OUT
    # DB_PROVIDER = 200,       # COMMENTED OUT
    DOCUMENT_NUMBER = 200,
    DOI = 200,
    EDITION = 200,
    INSTITUTION = 200,
    ISBN_ISSN = 200,
    # NUMBER_OF_PAGES = 50,    # COMMENTED OUT
    # NUMBER_OF_VOLUMES = 100, # COMMENTED OUT
    # PAGES = 200,             # COMMENTED OUT
    PERIODICAL_JOURNAL = 200,
    # PMCID = 200,             # COMMENTED OUT
    # PUBLISHED_PLACE = 200,   # COMMENTED OUT
    PUBLISHER = 200,
    REF_COMMENT = 1000,
    # SERIES_EDITOR = 200,     # COMMENTED OUT
    # SERIES_TITLE = 200,      # COMMENTED OUT
    URL = 200
  )
}

# Protocol Vocabulary Functions ----
# Each protocol type creates its own tribble, then combined with bind_rows

#' Sampling Protocol Options Vocabulary
#'
#' Returns sampling protocol options as a tibble with Protocol_Type, Short_Name, and Long_Name columns.
#'
#' @return A tibble with sampling protocol options
#' @export
#' @importFrom tibble tribble
sampling_protocols_vocabulary <- function() {
  tribble(
    ~Protocol_Type      , ~Short_Name        , ~Long_Name                            ,
    "Sampling Protocol" , "Not relevant"     , "Not relevant"                        ,
    "Sampling Protocol" , "Not reported"     , "Not reported"                        ,
    "Sampling Protocol" , "Point"            , "Point sampling"                      ,
    "Sampling Protocol" , "Composite"        , "Composite sampling"                  ,
    "Sampling Protocol" , "Trawl"            , "Trawl sampling"                      ,
    "Sampling Protocol" , "Grab"             , "Grab sampling"                       ,
    "Sampling Protocol" , "Core"             , "Core sampling"                       ,
    "Sampling Protocol" , "Seine net"        , "Seine net sampling"                  ,
    "Sampling Protocol" , "Electrofishing"   , "Electrofishing"                      ,
    "Sampling Protocol" , "Plankton net"     , "Plankton net sampling"               ,
    "Sampling Protocol" , "Bailer"           , "Bailer sampling"                     ,
    "Sampling Protocol" , "Peristaltic pump" , "Peristaltic pump sampling"           ,
    "Sampling Protocol" , "Active air"       , "Active air sampling"                 ,
    "Sampling Protocol" , "Passive air"      , "Passive air sampling"                ,
    "Sampling Protocol" , "SPMD"             , "Semipermeable membrane device"       ,
    "Sampling Protocol" , "SPE"              , "Solid phase extraction device"       ,
    "Sampling Protocol" , "LVSPE"            , "Large volume solid phase extraction" ,
    "Sampling Protocol" , "DGT"              , "Diffusive gradients in thin films"   ,
    "Sampling Protocol" , "Caged organisms"  , "Caged organism deployment"           ,
    "Sampling Protocol" , "Blood sample"     , "Blood sample"                        ,
    "Sampling Protocol" , "Biopsy"           , "Biopsy"                              ,
    "Sampling Protocol" , "Other"            , "Other"
  )
}

#' Fractionation Protocol Options Vocabulary
#'
#' Returns fractionation protocol options as a tibble with Protocol_Type, Short_Name, and Long_Name columns.
#'
#' @return A tibble with fractionation protocol options
#' @export
#' @importFrom tibble tribble
fractionation_protocols_vocabulary <- function() {
  tribble(
    ~Protocol_Type           , ~Short_Name         , ~Long_Name                                  ,
    "Fractionation Protocol" , "Not relevant"      , "Not relevant"                              ,
    "Fractionation Protocol" , "Not reported"      , "Not reported"                              ,
    "Fractionation Protocol" , "Total"             , "Total fraction"                            ,
    "Fractionation Protocol" , "Particles"         , "Particulate fraction"                      ,
    "Fractionation Protocol" , "Colloidal"         , "Colloidal fraction"                        ,
    "Fractionation Protocol" , "LMM"               , "Low molecular mass fraction"               ,
    "Fractionation Protocol" , "Aqueous"           , "Aqueous fraction"                          ,
    "Fractionation Protocol" , "Filtered 0.45um"   , "Filtered through 0.45 micrometer membrane" ,
    "Fractionation Protocol" , "Filtered 0.2um"    , "Filtered through 0.2 micrometer membrane"  ,
    "Fractionation Protocol" , "Dissolved"         , "Dissolved fraction"                        ,
    "Fractionation Protocol" , "Filtered"          , "Filtered fraction"                         ,
    "Fractionation Protocol" , "Acid extractable"  , "Acid extractable fraction"                 ,
    "Fractionation Protocol" , "Reducible"         , "Reducible fraction"                        ,
    "Fractionation Protocol" , "Oxidisable"        , "Oxidisable fraction"                       ,
    "Fractionation Protocol" , "Residual"          , "Residual fraction"                         ,
    "Fractionation Protocol" , "Bioavailable"      , "Bioavailable fraction"                     ,
    "Fractionation Protocol" , "Free ion"          , "Free ion activity"                         ,
    "Fractionation Protocol" , "Size fractionated" , "Size fractionated"                         ,
    "Fractionation Protocol" , "Other"             , "Other"
  )
}

#' Extraction Protocol Options Vocabulary
#'
#' Returns extraction protocol options as a tibble with Protocol_Type, Short_Name, and Long_Name columns.
#'
#' @return A tibble with extraction protocol options
#' @export
#' @importFrom tibble tribble
extraction_protocols_vocabulary <- function() {
  tribble(
    ~Protocol_Type        , ~Short_Name                         , ~Long_Name                                           ,
    "Extraction Protocol" , "Not relevant"                      , "Not relevant"                                       ,
    "Extraction Protocol" , "Not reported"                      , "Not reported"                                       ,
    "Extraction Protocol" , "None"                              , "No extraction"                                      ,
    "Extraction Protocol" , "Methanol"                          , "Methanol extraction"                                ,
    "Extraction Protocol" , "Dichloromethane"                   , "Dichloromethane extraction"                         ,
    "Extraction Protocol" , "SPE Isolute Env+"                  , "Solid phase extraction with Isolute Env+ cartridge" ,
    "Extraction Protocol" , "Membrane filtration 0.45um"        , "Membrane filtration through 0.45 micrometer"        ,
    "Extraction Protocol" , "Membrane filtration 0.2um"         , "Membrane filtration through 0.2 micrometer"         ,
    "Extraction Protocol" , "Membrane filtration"               , "Membrane filtration"                                ,
    "Extraction Protocol" , "Filtration"                        , "Filtration"                                         ,
    "Extraction Protocol" , "Microwave-assisted acid digestion" , "Microwave-assisted acid digestion"                  ,
    "Extraction Protocol" , "Acid digestion"                    , "Acid digestion"                                     ,
    "Extraction Protocol" , "Pressurised liquid"                , "Pressurised liquid extraction"                      ,
    "Extraction Protocol" , "Ultrasonic"                        , "Ultrasonic extraction"                              ,
    "Extraction Protocol" , "Soxhlet"                           , "Soxhlet extraction"                                 ,
    "Extraction Protocol" , "QuEChERS"                          , "Quick easy cheap effective rugged safe extraction"  ,
    "Extraction Protocol" , "Accelerated solvent"               , "Accelerated solvent extraction"                     ,
    "Extraction Protocol" , "Sequential extraction"             , "Sequential extraction protocol"                     ,
    "Extraction Protocol" , "Other"                             , "Other"
  )
}

#' Analytical Protocol Options Vocabulary
#'
#' Returns analytical protocol options as a tibble with Protocol_Type, Short_Name, and Long_Name columns.
#'
#' @return A tibble with analytical protocol options
#' @export
#' @importFrom tibble tribble
analytical_protocols_vocabulary <- function() {
  tribble(
    ~Protocol_Type        , ~Short_Name          , ~Long_Name                                                 ,
    "Analytical Protocol" , "Not relevant"       , "Not relevant"                                             ,
    "Analytical Protocol" , "Not reported"       , "Not reported"                                             ,
    "Analytical Protocol" , "GC-MS"              , "Gas chromatography mass spectrometry"                     ,
    "Analytical Protocol" , "LC-MS"              , "Liquid chromatography mass spectrometry"                  ,
    "Analytical Protocol" , "LC-MS/MS"           , "Liquid chromatography tandem mass spectrometry"           ,
    "Analytical Protocol" , "GC-MS/MS"           , "Gas chromatography tandem mass spectrometry"              ,
    "Analytical Protocol" , "UPLC"               , "Ultra performance liquid chromatography"                  ,
    "Analytical Protocol" , "ICP-MS"             , "Inductively coupled plasma mass spectrometry"             ,
    "Analytical Protocol" , "ICP-OES"            , "Inductively coupled plasma optical emission spectroscopy" ,
    "Analytical Protocol" , "AAS"                , "Atomic absorption spectroscopy"                           ,
    "Analytical Protocol" , "XRF"                , "X-ray fluorescence spectroscopy"                          ,
    "Analytical Protocol" , "Ion chromatography" , "Ion chromatography"                                       ,
    "Analytical Protocol" , "Spectrophotometry"  , "Spectrophotometry"                                        ,
    "Analytical Protocol" , "Fluorescence"       , "Fluorescence spectroscopy"                                ,
    "Analytical Protocol" , "Other"              , "Other"
  )
}

#' Protocol Options Data
#'
#' Returns all protocol options data by combining all individual protocol vocabularies.
#'
#' @return A tibble with Protocol_Type, Short_Name, and Long_Name columns for all protocols
#' @export
#' @importFrom dplyr bind_rows
protocol_options_vocabulary <- function() {
  bind_rows(
    sampling_protocols_vocabulary(),
    fractionation_protocols_vocabulary(),
    extraction_protocols_vocabulary(),
    analytical_protocols_vocabulary()
  )
}

#' Protocol Categories Controlled Vocabulary
#'
#' Returns controlled vocabulary options for protocol categories.
#'
#' @return A character vector of protocol category options
#' @export
protocol_categories_vocabulary <- function() {
  c(
    "Sampling Protocol",
    "Fractionation Protocol",
    "Extraction Protocol",
    "Analytical Protocol"
  )
}


#' Read in ecotoxicological units and conversion factors from csv
#'
#' @param select_column name of column to pull ("MEASURED_UNIT", "BASE_SI_UNIT", "CONVERSION_FACTOR", "UNIT_COMMENTS")
#'
#' @returns a dataframe or a character vector
#'
#' @export
#' @importFrom readr read_csv
parameter_unit_vocabulary <- function(select_column = NULL) {
  units <- read_csv(
    file = "inst/data/clean/unit_conversion_factors.csv",
    col_names = TRUE,
    show_col_types = FALSE
  )
  if (is.null(select_column)) {
    return(units)
  }
  stopifnot(select_column %in% names(units))
  return(units[[select_column]])
}


#' Measurement Flags Controlled Vocabulary
#'
#' Returns measurement flag options.
#'
#' @return A character vector of measurement flag options
#' @export
measured_flags_vocabulary <- function() {
  c("", "< LOQ", "< LOD")
}


#' CREED Assessment Scoring Choices
#'
#' @description Returns the standardised CREED assessment scoring options
#' @return Named character vector with CREED scoring choices
# FIXME: Why isn't "Not Relevant" showing up as an option?
CREED_choices_vocabulary <- function() {
  c(
    "Not Met" = 4,
    "Fully Met" = 1,
    "Partly Met" = 2,
    "Not Reported" = 3,
    "Not Relevant" = 1
  )
}
