# Initialise tibbles for each "table" of data used across the modules.
# Doing this in one place makes it easier (but not easy) to keep things consistent.
# The plan is that eventually all functions that are dependent on table format will
# be based in some way on these functions. That may prove to be impractical. But it's a start.
# I also mention a bunch of database TABLES. These don't actually exist yet, but are
# useful for understanding the structure of the data
# eData DRF Version - Not Tracked
# Last Updated 2025.12.08 - R6 update

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
    campaignData = CampaignData_Table_R6_Generator$new()$data, # # New R6 object; just the tibble
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

# ---- Schema Definitions ----
# These functions define the structure of each table and are used by R6 generators
#' Get Campaign Data Schema
#'
#' @return A list with version and schema tibble for campaign data
#' @importFrom tibble tribble
#' @export
get_campaign_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name                  , ~data_type  , ~mandatory , ~description                    ,
      "CAMPAIGN_NAME_SHORT"         , "character" , TRUE       , "Short campaign identifier"     ,
      "CAMPAIGN_NAME"               , "character" , TRUE       , "Full campaign name"            ,
      "CAMPAIGN_START_DATE"         , "Date"      , TRUE       , "Campaign start date"           ,
      "CAMPAIGN_END_DATE"           , "Date"      , FALSE      , "Campaign end date"             ,
      "RELIABILITY_SCORE"           , "character" , FALSE      , "Data reliability score"        ,
      "RELIABILITY_EVAL_SYS"        , "character" , FALSE      , "Reliability evaluation system" ,
      "CONFIDENTIALITY_EXPIRY_DATE" , "Date"      , FALSE      , "When data becomes public"      ,
      "ORGANISATION"                , "character" , TRUE       , "Responsible organisation"      ,
      "ENTERED_BY"                  , "character" , TRUE       , "Data entry person"             ,
      "ENTERED_DATE"                , "Date"      , TRUE       , "Data entry date"               ,
      "CAMPAIGN_COMMENT"            , "character" , FALSE      , "Additional comments"
    )
  )
}

#' Get Reference Data Schema
#'
#' @return A list with version and schema tibble for reference data
#' @importFrom tibble tribble
#' @export
get_reference_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name         , ~data_type  , ~mandatory , ~description                  ,
      "REFERENCE_ID"       , "character" , TRUE       , "Unique reference identifier" ,
      "REFERENCE_TYPE"     , "character" , TRUE       , "Type of reference"           ,
      "DATA_SOURCE"        , "character" , TRUE       , "Source of the data"          ,
      "AUTHOR"             , "character" , TRUE       , "Author(s)"                   ,
      "TITLE"              , "character" , TRUE       , "Title of reference"          ,
      "YEAR"               , "integer"   , TRUE       , "Publication year"            ,
      "ACCESS_DATE"        , "Date"      , FALSE      , "Date accessed"               ,
      "PERIODICAL_JOURNAL" , "character" , FALSE      , "Journal or periodical name"  ,
      "VOLUME"             , "integer"   , FALSE      , "Volume number"               ,
      "ISSUE"              , "integer"   , FALSE      , "Issue number"                ,
      "PUBLISHER"          , "character" , FALSE      , "Publisher name"              ,
      "INSTITUTION"        , "character" , FALSE      , "Institution name"            ,
      "DOI"                , "character" , FALSE      , "Digital Object Identifier"   ,
      "URL"                , "character" , FALSE      , "URL"                         ,
      "ISBN_ISSN"          , "character" , FALSE      , "ISBN or ISSN"                ,
      "EDITION"            , "character" , FALSE      , "Edition"                     ,
      "DOCUMENT_NUMBER"    , "character" , FALSE      , "Document number"             ,
      "REF_COMMENT"        , "character" , FALSE      , "Additional comments"
    )
  )
}

#' Get Sites Data Schema
#'
#' @return A list with version and schema tibble for sites data
#' @importFrom tibble tribble
#' @export
get_sites_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name                  , ~data_type  , ~mandatory , ~description                  ,
      "SITE_CODE"                   , "character" , TRUE       , "Unique site identifier"      ,
      "SITE_NAME"                   , "character" , TRUE       , "Site name"                   ,
      "SITE_GEOGRAPHIC_FEATURE"     , "character" , TRUE       , "Geographic feature type"     ,
      "SITE_GEOGRAPHIC_FEATURE_SUB" , "character" , TRUE       , "Geographic feature subtype"  ,
      "COUNTRY_ISO"                 , "character" , TRUE       , "ISO country code"            ,
      "OCEAN_IHO"                   , "character" , TRUE       , "IHO ocean designation"       ,
      "LATITUDE"                    , "numeric"   , TRUE       , "Latitude (decimal degrees)"  ,
      "LONGITUDE"                   , "numeric"   , TRUE       , "Longitude (decimal degrees)" ,
      "SITE_COORDINATE_SYSTEM"      , "character" , TRUE       , "Coordinate reference system" ,
      "ALTITUDE_VALUE"              , "numeric"   , TRUE       , "Altitude value"              ,
      "ALTITUDE_UNIT"               , "character" , TRUE       , "Altitude unit"               ,
      "ENTERED_BY"                  , "character" , TRUE       , "Data entry person"           ,
      "ENTERED_DATE"                , "character" , TRUE       , "Data entry date"             ,
      "SITE_COMMENT"                , "character" , FALSE      , "Additional comments"
    )
  )
}

#' Get Biota Data Schema
#'
#' @return A list with version and schema tibble for biota data
#' @importFrom tibble tribble
#' @export
get_biota_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name               , ~data_type  , ~mandatory , ~description                    ,
      "SAMPLE_ID"                , "character" , TRUE       , "Unique sample identifier"      ,
      "SITE_CODE"                , "character" , TRUE       , "Site identifier"               ,
      "PARAMETER_NAME"           , "character" , TRUE       , "Parameter name"                ,
      "ENVIRON_COMPARTMENT"      , "character" , TRUE       , "Environmental compartment"     ,
      "ENVIRON_COMPARTMENT_SUB"  , "character" , FALSE      , "Environmental compartment sub" ,
      "MEASURED_CATEGORY"        , "character" , TRUE       , "Measured category"             ,
      "SAMPLING_DATE"            , "character" , TRUE       , "Sampling date"                 ,
      "SUBSAMPLE"                , "character" , TRUE       , "Subsample identifier"          ,
      "SPECIES_GROUP"            , "character" , TRUE       , "Species group"                 ,
      "SAMPLE_SPECIES"           , "character" , TRUE       , "Sample species"                ,
      "SAMPLE_TISSUE"            , "character" , TRUE       , "Sample tissue type"            ,
      "SAMPLE_SPECIES_LIFESTAGE" , "character" , TRUE       , "Species life stage"            ,
      "SAMPLE_SPECIES_GENDER"    , "character" , TRUE       , "Species gender"                ,
      "BIOTA_COMMENT"            , "character" , FALSE      , "Additional comments"
    )
  )
}

#' Get Samples Data Schema
#'
#' @return A list with version and schema tibble for samples data
#' @importFrom tibble tribble
#' @export
get_samples_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name              , ~data_type  , ~mandatory , ~description                    ,
      "SITE_CODE"               , "character" , TRUE       , "Site identifier"               ,
      "SITE_NAME"               , "character" , TRUE       , "Site name"                     ,
      "PARAMETER_NAME"          , "character" , TRUE       , "Parameter name"                ,
      "PARAMETER_TYPE"          , "character" , TRUE       , "Parameter type"                ,
      "ENVIRON_COMPARTMENT"     , "character" , TRUE       , "Environmental compartment"     ,
      "ENVIRON_COMPARTMENT_SUB" , "character" , TRUE       , "Environmental compartment sub" ,
      "MEASURED_CATEGORY"       , "character" , TRUE       , "Measured category"             ,
      "SAMPLING_DATE"           , "character" , TRUE       , "Sampling date"                 ,
      "SUBSAMPLE"               , "character" , TRUE       , "Subsample identifier"          ,
      "SUBSAMPLE_ID"            , "character" , FALSE      , "Subsample ID"                  ,
      "SAMPLE_ID"               , "character" , TRUE       , "Unique sample identifier"
    )
  )
}

#' Get Parameters Data Schema
#'
#' @return A list with version and schema tibble for parameters data
#' @importFrom tibble tribble
#' @export
get_parameters_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name         , ~data_type  , ~mandatory , ~description             ,
      "PARAMETER_TYPE"     , "character" , TRUE       , "Parameter type"         ,
      "PARAMETER_TYPE_SUB" , "character" , TRUE       , "Parameter subtype"      ,
      "MEASURED_TYPE"      , "character" , TRUE       , "Measured type"          ,
      "PARAMETER_NAME"     , "character" , TRUE       , "Unique parameter name"  ,
      "PARAMETER_NAME_SUB" , "character" , FALSE      , "Parameter name subtype" ,
      "INCHIKEY_SD"        , "character" , FALSE      , "Standard InChI key"     ,
      "PUBCHEM_CID"        , "integer"   , FALSE      , "PubChem compound ID"    ,
      "CAS_RN"             , "character" , FALSE      , "CAS Registry Number"    ,
      "ENTERED_BY"         , "character" , TRUE       , "Data entry person"      ,
      "PARAMETER_COMMENT"  , "character" , FALSE      , "Additional comments"
    )
  )
}

#' Get Methods Data Schema
#'
#' @return A list with version and schema tibble for methods data
#' @importFrom tibble tribble
#' @export
get_methods_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name        , ~data_type  , ~mandatory , ~description                 ,
      "PROTOCOL_ID"       , "character" , TRUE       , "Unique protocol identifier" ,
      "CAMPAIGN_NAME"     , "character" , TRUE       , "Campaign name"              ,
      "PROTOCOL_CATEGORY" , "character" , TRUE       , "Protocol category"          ,
      "PROTOCOL_NAME"     , "character" , TRUE       , "Protocol name"              ,
      "PROTOCOL_COMMENT"  , "character" , FALSE      , "Additional comments"
    )
  )
}

#' Get Compartments Data Schema
#'
#' @return A list with version and schema tibble for compartments data
#' @importFrom tibble tribble
#' @export
get_compartments_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name              , ~data_type  , ~mandatory , ~description                    ,
      "ENVIRON_COMPARTMENT"     , "character" , TRUE       , "Environmental compartment"     ,
      "ENVIRON_COMPARTMENT_SUB" , "character" , TRUE       , "Environmental compartment sub" ,
      "MEASURED_CATEGORY"       , "character" , TRUE       , "Measured category"
    )
  )
}

#' Get Measurements Data Schema
#'
#' @return A list with version and schema tibble for measurements data
#' @importFrom tibble tribble
#' @export
get_measurements_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name              , ~data_type  , ~mandatory , ~description                    ,
      "SITE_CODE"               , "character" , TRUE       , "Site identifier"               ,
      "PARAMETER_NAME"          , "character" , TRUE       , "Parameter name"                ,
      "SAMPLING_DATE"           , "character" , TRUE       , "Sampling date"                 ,
      "ENVIRON_COMPARTMENT_SUB" , "character" , FALSE      , "Environmental compartment sub" ,
      "SUBSAMPLE"               , "character" , FALSE      , "Subsample identifier"          ,
      "MEASURED_FLAG"           , "character" , FALSE      , "Measurement flag"              ,
      "MEASURED_VALUE"          , "numeric"   , TRUE       , "Measured value"                ,
      "UNCERTAINTY_TYPE"        , "character" , TRUE       , "Uncertainty type"              ,
      "UNCERTAINTY_UPPER"       , "numeric"   , FALSE      , "Upper uncertainty bound"       ,
      "UNCERTAINTY_LOWER"       , "numeric"   , FALSE      , "Lower uncertainty bound"       ,
      "MEASURED_UNIT"           , "character" , TRUE       , "Measurement unit"              ,
      "MEASURED_N"              , "numeric"   , FALSE      , "Number of measurements"        ,
      "LOQ_VALUE"               , "numeric"   , FALSE      , "Limit of quantification value" ,
      "LOQ_UNIT"                , "character" , FALSE      , "Limit of quantification unit"  ,
      "LOD_VALUE"               , "numeric"   , FALSE      , "Limit of detection value"      ,
      "LOD_UNIT"                , "character" , FALSE      , "Limit of detection unit"       ,
      "SAMPLING_PROTOCOL"       , "character" , TRUE       , "Sampling protocol"             ,
      "EXTRACTION_PROTOCOL"     , "character" , TRUE       , "Extraction protocol"           ,
      "FRACTIONATION_PROTOCOL"  , "character" , TRUE       , "Fractionation protocol"        ,
      "ANALYTICAL_PROTOCOL"     , "character" , TRUE       , "Analytical protocol"           ,
      "REFERENCE_ID"            , "character" , TRUE       , "Reference identifier"          ,
      "SAMPLE_ID"               , "character" , FALSE      , "Sample identifier"             ,
      "CAMPAIGN_NAME_SHORT"     , "character" , FALSE      , "Short campaign identifier"     ,
      "ENVIRON_COMPARTMENT"     , "character" , FALSE      , "Environmental compartment"     ,
      "PARAMETER_TYPE"          , "character" , FALSE      , "Parameter type"                ,
      "MEASURED_TYPE"           , "character" , FALSE      , "Measured type"                 ,
      "MEASUREMENT_COMMENT"     , "character" , FALSE      , "Additional comments"
    )
  )
}

#' Get CREED Scores Data Schema
#'
#' @return A list with version and schema tibble for CREED scores data
#' @importFrom tibble tribble
#' @export
get_creed_scores_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name         , ~data_type  , ~mandatory , ~description               ,
      "REFERENCE_ID"       , "character" , TRUE       , "Reference identifier"     ,
      "SILVER_RELIABILITY" , "character" , TRUE       , "Silver reliability score" ,
      "SILVER_RELEVANCE"   , "character" , TRUE       , "Silver relevance score"   ,
      "GOLD_RELIABILITY"   , "character" , TRUE       , "Gold reliability score"   ,
      "GOLD_RELEVANCE"     , "character" , TRUE       , "Gold relevance score"
    )
  )
}

#' Get CREED Data Schema
#'
#' Schema for CREED criterion scores used by reliability and relevance modules.
#' This is an internal data structure for consistency across CREED modules.
#'
#' @return A list with version and schema tibble for CREED data
#' @importFrom tibble tribble
#' @export
get_creed_data_schema <- function() {
  list(
    version = "0.3.5",
    schema = tribble(
      ~column_name           , ~data_type  , ~mandatory , ~description                ,
      "criterion_id"         , "character" , TRUE       , "Criterion identifier"      ,
      "criterion_title"      , "character" , TRUE       , "Criterion title"           ,
      "required_recommended" , "character" , TRUE       , "Required or recommended"   ,
      "relevant_data"        , "character" , TRUE       , "Relevant data description" ,
      "score"                , "character" , TRUE       , "Score value"               ,
      "limitations"          , "character" , FALSE      , "Limitations description"
    )
  )
}

#' Get All Schema Versions
#'
#' Returns a tibble with all schema names and their versions
#'
#' @return A tibble with columns: schema_name, version
#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @export
get_all_schema_versions <- function() {
  # Get all schema functions ----
  schema_functions <- ls(envir = .GlobalEnv, pattern = "^get_.*_schema$")

  # Extract versions ----
  versions <- map_chr(schema_functions, function(fn_name) {
    fn <- get(fn_name, envir = .GlobalEnv)
    fn()$version
  })

  # Clean up names ----
  schema_names <- gsub("^get_|_schema$", "", schema_functions)

  tibble(
    schema_name = schema_names,
    version = versions
  )
}

#' Export All Schemas to Excel
#'
#' Exports all schema definitions to an Excel workbook with one sheet per schema
#' plus a versions sheet
#'
#' @param path Path to save the Excel file
#' @importFrom writexl write_xlsx
#' @importFrom purrr map set_names
#' @export
export_schemas_to_excel <- function(path = "data_format_specification.xlsx") {
  # Get all schema functions ----
  schema_functions <- ls(envir = .GlobalEnv, pattern = "^get_.*_schema$")

  # Create list of schemas ----
  schema_list <- map(schema_functions, function(fn_name) {
    fn <- get(fn_name, envir = .GlobalEnv)
    fn()$schema
  })

  # Clean up names for sheet names ----
  schema_names <- gsub("^get_|_schema$", "", schema_functions) |>
    gsub("_", " ", x = _) |>
    tools::toTitleCase()

  names(schema_list) <- schema_names

  # Add versions sheet at the beginning ----
  versions_sheet <- get_all_schema_versions()

  output_list <- c(
    list(Versions = versions_sheet),
    schema_list
  )

  # Write to Excel ----
  write_xlsx(output_list, path = path)

  message("Schemas exported to: ", path)
  invisible(path)
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
  # FIXME: Currently a wrapper for our new R6 object
  CampaignData_Table_R6_Generator$new()$data
  # tibble(
  #   CAMPAIGN_NAME_SHORT = character(),
  #   CAMPAIGN_NAME = character(),
  #   CAMPAIGN_START_DATE = as.Date(character()),
  #   CAMPAIGN_END_DATE = as.Date(character()),
  #   RELIABILITY_SCORE = character(),
  #   RELIABILITY_EVAL_SYS = character(),
  #   CONFIDENTIALITY_EXPIRY_DATE = as.Date(character()),
  #   ORGANISATION = character(),
  #   ENTERED_BY = character(),
  #   ENTERED_DATE = as.Date(character()),
  #   CAMPAIGN_COMMENT = character()
  # )
}


add_campaign_rows <- function(existing_tibble, new_rows) {
  validator <- CampaignData_Table_R6_Generator$new()
  validator$data <- existing_tibble
  validator$add_rows(new_rows) # Validation happens here
  validator$data # Return validated tibble
}

# ---- Table Initialisation Functions ----
# These are wrappers around R6 generators for backwards compatibility

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
  CampaignData_Table_R6_Generator$new()$data
}


#' Initialise Reference Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for reference data.
#' References provide bibliographic information for data sources including journals,
#' reports, datasets, and other published materials.
#'
#' @return A tibble with 0 rows and standardised reference columns
#' @importFrom tibble tibble
#' @export
initialise_references_tibble <- function() {
  ReferenceData_Table_R6_Generator$new()$data
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
  SitesData_Table_R6_Generator$new()$data
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
  BiotaData_Table_R6_Generator$new()$data
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
  SamplesData_Table_R6_Generator$new()$data
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
  ParametersData_Table_R6_Generator$new()$data
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
  MethodsData_Table_R6_Generator$new()$data
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
  CompartmentsData_Table_R6_Generator$new()$data
}


#' Initialise Measurements Data Tibble
#'
#' Creates an empty tibble with the standardised column structure for measurements data.
#'
#' @return A tibble with 0 rows and standardised measurement columns
#' @importFrom tibble tibble
#' @export
initialise_measurements_tibble <- function() {
  MeasurementsData_Table_R6_Generator$new()$data
}


add_campaign_rows <- function(existing_tibble, new_rows) {
  validator <- CampaignData_Table_R6_Generator$new()
  validator$data <- existing_tibble
  validator$add_rows(new_rows) # Validation happens here
  validator$data # Return validated tibble
}

#' Initialise CREED Scores Tibble
#'
#' Creates an empty tibble with the standardised column structure for CREED scores.
#'
#' @return A tibble with 0 rows and standardised CREED columns
#' @importFrom tibble tibble
#' @export
initialise_CREED_scores_tibble <- function() {
  CREEDScoresData_Table_R6_Generator$new()$data
}

#' Initialise CREED Data Tibble
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
  CREEDData_Table_R6_Generator$new()$data
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
#' @importFrom dplyr mutate arrange bind_rows case_when
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
#' @importFrom dplyr select distinct arrange pull
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
#' @importFrom dplyr mutate bind_rows
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
    "First-Third Quartile Range",
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
