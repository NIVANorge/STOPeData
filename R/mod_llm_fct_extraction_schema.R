# LLM Extraction Schema Functions ----
# Uses vocabulary functions from fct_formats.R to reduce duplication
# Updated to use glue instead of paste0 for string construction

#' Create campaign schema
#' @importFrom glue glue
#' @noRd
create_campaign_schema <- function() {
  type_array(
    type_object(
      .description = "Basic study/campaign information",
      campaign_name = type_string(
        description = "Identifier for the study/campaign (max 100 chars)",
        required = FALSE
      ),
      campaign_name_short = type_string(
        description = "Abbreviated form of the Identifier for the study/campaign (max 20 char, no spaces or underscores). 
      Should be specific to location, study, and date as much as possible",
        required = FALSE
      ),
      campaign_start_date = type_string(
        description = "Study start date in YYYY-MM-DD format",
        required = FALSE
      ),
      campaign_end_date = type_string(
        description = "Study end date in YYYY-MM-DD format",
        required = FALSE
      ),
      organisation = type_string(
        description = "Organisation that conducted the study (max 50 chars)",
        required = FALSE
      ),
      campaign_comment = type_string(
        description = "Additional study details or notes (max 1000 chars)",
        required = FALSE
      ),
      campaign_rationale = type_string(
        description = "The rationale given for the study or sampling campaign. Max 500 chars.",
        required = FALSE
      )
    )
  )
}

#' Create references schema
#' @noRd
create_references_schema <- function() {
  type_array(
    type_object(
      .description = "Bibliographic information about this document. Return NA if not found.",
      author = type_string(
        description = "Authors in format: Last1, First1; Last2, First2 (max 1000 chars)",
        required = FALSE
      ),
      title = type_string(
        description = "Document title (max 1000 chars). Return NA if not found.",
        required = FALSE
      ),
      year = type_integer(
        description = "Publication year (1800-2026). Return NA if not found.",
        required = FALSE
      ),
      periodical_journal = type_string(
        description = "Journal name for articles. Return NA if not found.",
        required = FALSE
      ),
      volume = type_integer(
        description = "Journal volume number. Return NA if not found.",
        required = FALSE
      ),
      issue = type_integer(
        description = "Journal issue number. Return NA if not found.",
        required = FALSE
      ),
      publisher = type_string(
        description = "Publisher name. Return NA if not found.",
        required = FALSE
      ),
      doi = type_string(
        description = "Digital Object Identifier. Return NA if not found.",
        required = FALSE
      )
    )
  )
}

#' Create sites schema
#' @noRd
create_sites_schema <- function() {
  type_array(
    type_object(
      .description = "Information about a sampling site",
      site_code = type_string(
        description = "Short site identifier/code",
        required = FALSE
      ),
      site_name = type_string(
        description = "Descriptive site name. If many sampling sites are reported without specific coordinates, note in the name and site_comment that they have been merged into a single site for convenience.",
        required = FALSE
      ),
      latitude = type_number(
        description = "Latitude in decimal degrees (-90 to 90) - ONLY if explicitly stated in document",
        required = FALSE
      ),
      longitude = type_number(
        description = "Longitude in decimal degrees (-180 to 180) - ONLY if explicitly stated in document",
        required = FALSE
      ),
      country_iso = type_string(
        description = "ISO Country where (terrestrial) site is located. Use full name (ISO 3166 and not codes (3166-2)) 
        In case of site at the land-sea interface, return both country and IHO ocean/sea. If a purely oceanic sampling site, return Not relevant",
        required = FALSE
      ),
      ocean_iho = type_string(
        description = as.character(glue(
          "Ocean or sea where (marine) site is located. In case of site at the land-sea interface, return both country and IHO ocean/sea. 
          If purely terrestrial, return Not relevant..: {paste(areas_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      ),
      site_geographic_feature = type_string(
        description = as.character(glue(
          "Geographic feature type from: {paste(geographic_features_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      ),
      site_geographic_feature_sub = type_string(
        description = as.character(glue(
          "Geographic sub-feature type from: {paste(geographic_features_sub_vocabulary(), collapse = ', ')}. ",
          "As these are currently mostly water-based use other most of the time."
        )),
        required = FALSE
      ),
      site_comment = type_string(
        description = "Any additional details about the site not captured in the previous variables. If coordinates are converted from another CRS or from minute degrees to decimal degrees, report original figures and that a conversion was performed here.",
        required = FALSE
      )
    )
  )
}

#' Create parameters schema
#' @noRd
create_parameters_schema <- function() {
  type_array(
    type_object(
      .description = "A measured parameter/stressor",
      parameter_name = type_string(
        description = "Name of the parameter/chemical/stressor measured. If a parameter is reported under multiple names
         (e.g. Copper and Cu, Paracetamol and Acetaminophen), only return one entry. ONLY RETURN COPPER OR COPPER-CONTAINING MOLECULES", # FIXME: Obviously, remove this before trying to use it for other stuff
        required = FALSE
      ),
      parameter_type = type_string(
        description = "Type: Stressor, Quality parameter, Normalization, or Background",
        required = FALSE
      ),
      cas_rn = type_string(
        description = "CAS Registry Number if chemical",
        required = FALSE
      ),
      parameter_comment = type_string(
        description = "Any other comments relevant to undderstanding/interpreting measured parameters.",
        required = FALSE
      )
    )
  )
}

#' Create compartments schema
#' @noRd
create_compartments_schema <- function() {
  type_array(
    type_object(
      .description = "An environmental compartment sampled",
      environ_compartment = type_string(
        description = "Main compartment: Aquatic, Atmospheric, Terrestrial, or Biota",
        required = FALSE
      ),
      environ_compartment_sub = type_string(
        description = as.character(glue(
          "Sub-compartment: {paste(environ_compartments_sub_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      ),
      measured_category = type_string(
        description = as.character(glue(
          "Measurement category: {paste(measured_categories_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      )
    )
  )
}

#' Create biota schema
#' @noRd
create_biota_schema <- function() {
  type_array(
    type_object(
      .description = "Biological sampling information",
      sample_id = type_string(
        description = "Sample identifier",
        required = FALSE
      ),
      species_group = type_string(
        description = as.character(glue(
          "Taxonomic group: {paste(species_groups_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      ),
      sample_species = type_string(
        description = "Species name (scientific if reported otherwise common)",
        required = FALSE
      ),
      sample_tissue = type_string(
        description = as.character(glue(
          "Tissue type: {paste(tissue_types_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      ),
      sample_species_lifestage = type_string(
        description = as.character(glue(
          "Life stage: {paste(lifestage_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      ),
      sample_species_gender = type_string(
        description = as.character(glue(
          "Gender: {paste(gender_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      )
    )
  )
}

#' Create methods schema
#' @noRd
create_methods_schema <- function() {
  type_array(
    type_object(
      .description = "Analytical, extraction, fractioning and sampling methods used. Ensure fractionation protocol is included, even when it's very similar to extraction protocol.",
      protocol_category = type_string(
        description = as.character(glue(
          "Protocol type: {paste(protocol_categories_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      ),
      protocol_name = type_string(
        description = as.character(glue(
          "Protocol name is more like a rough grouping: {paste(protocol_options_vocabulary(), collapse = ', ')}"
        )),
        required = FALSE
      ),
      protocol_comment = type_string(
        description = "Additional details about the method, including a more specific description of the method (appliance name, reagents, use of SRM, lab spike samples, lab replicates, control recoveries, method blanks, field blanks, or field QC)., ideally transcribed from source without modification.",
        required = FALSE
      )
    )
  )
}

#' Create samples schema
#' @noRd
create_samples_schema <- function() {
  type_array(
    type_object(
      .description = "Information on the overall sampling strategy of the paper, a (potentially assymetrical) combination of sites, dates, compartments/biota, and measured parameters combined from schema already extracted. 
      Some of these may be replicated multiple times. It is important to ensure that all reported combinations actually occur in the paper.",
      sampling_dates = type_string(
        description = "Dates (YYYY-MM-DD) when samples were taken",
        required = TRUE
      ),
      sampling_site_code = type_string(
        description = "The {site_code} where samples were taken.",
        required = TRUE
      ),
      sampling_site_name = type_string(
        description = "The {site_name} where samples were taken.",
        required = TRUE
      ),
      sampling_compartment = type_string(
        description = "The {environ_compartment} > {environ_compartment_sub} > {measured_category} that was sampled.",
        required = TRUE
      ),
      sampling_parameters = type_string(
        description = "The {parameter_name} measured or analysed, based on the data extracted earlier in the schema.",
        required = TRUE
      ),
      subsample_indices = type_string(
        description = "Any other sampling dimensions not captured in the schema. this may include tissues, species, core depth, replicates, etc.
        return either short names or numbers for all valid subsamples per site/parameter/compartment/date combinations as a single comma-separated string (e.g. cod liver, trout liver, cod muscle, crab whole body).",
        required = TRUE
      )
    ),
    description = "Information on the overall sampling strategy of the paper"
  )
}

#' Create comemnts schema
#' @noRd
create_comments_schema <- function() {
  type_object(
    .description = "Commentary and metadata on the information quality of the paper, and of the LLM extraction. For scoring, 1 is worst, 5 is best.",
    paper_relevance = type_string(
      description = "A comment on the relevance of the paper to the questions posed in the prompt (2 sentences). Return in the format Score: {1-5}: {text}",
      required = TRUE
    ),
    paper_reliability = type_string(
      description = "A general assessment of the reliability of the paper. Return in the format Score: {1-5}: {text}",
      required = TRUE
    ),
    paper_data_source = type_string(
      description = "Where the paper's original data came from (e.g. does the paper describe the generation of data, or its aquisition from another source. Return in the format Score: {1-5}: {text}",
      required = TRUE
    ),
    paper_data_available = type_string(
      description = "Whether the data analysed in the paper is available, in particular in a good format (CSV), ok format (table with subgroups, summary statistics, etc.), or bad format (heavily transformed data, graphs) 
      Return in the format Score: {1-5}: {text}. 
      If the paper is missing any of the following minimum data standards, it should be scored as 1: Specific analysed medium/matrix, specified analyte, sampling location information to at least country/ocean level, sampling date to at least year level, units of measurement.
      Also if data may be available in supplementary information.",
      required = TRUE
    ),
    extraction_assessement = type_string(
      description = "An assessment of how confident you the LLM are in the quality of the data extraction process. 
      Has relevant data been lost? How confident are you that the information you returned matches that reported by the paper? Return in the format Score: {1-5}: {text}",
      required = TRUE
    )
  )
}

#' Create extraction schema with correct ellmer syntax
#' @noRd
create_extraction_schema <- function() {
  type_object(
    .description = "Extract environmental exposure study data from this document",
    campaign = create_campaign_schema(),
    references = create_references_schema(),
    sites = create_sites_schema(),
    parameters = create_parameters_schema(),
    compartments = create_compartments_schema(),
    biota = create_biota_schema(),
    methods = create_methods_schema(),
    samples = create_samples_schema(),
    comments = create_comments_schema()
  )
}

get_schema_display <- function() {
  tryCatch(
    {
      # Create the actual schema and capture its structure
      schema <- create_extraction_schema()

      # Convert to a readable format showing the actual ellmer object structure
      schema_str <- capture.output({
        print(schema, width = 1000)
      })

      # Join the output lines
      paste(schema_str, collapse = "\n")
    },
    error = function(e) {
      paste("Error displaying schema:", e$message)
    }
  )
}
