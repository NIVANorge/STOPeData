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

  # Populate CREED data ----
  userData$creedReliability <- dummy_CREED_reliability_tibble()
  userData$creedRelevance <- dummy_CREED_relevance_tibble()
  userData$creedScores <- dummy_CREED_scores_tibble()

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
  userData$creedReliabilityValid <- TRUE
  userData$creedRelevanceValid <- TRUE

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
        "Adult copepods collected between January and March 1997, acclimated to 15 degrees C for 12 days",
        "Radioactivity measured with NaI(Tl) gamma detectors at specific energy levels for each isotope",
        "Not sure."
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


#' Convert CREED Tibble to Mock Input List
#'
#' @description Converts a CREED data tibble (from dummy_CREED_reliability_tibble()
#' or dummy_CREED_relevance_tibble()) into a mock shiny input list object suitable for testing
#' collect_CREED_data().
#'
#' @param creed_tibble A tibble with columns: criterion_id, relevant_data, score, limitations
#'
#' @details
#' Score values are converted from numeric to text:
#' - 1 = "Fully Met" (or "Not Relevant")
#' - 2 = "Partly Met"
#' - 3 = "Not Reported"
#' - 4 = "Not Met"
#'
#' @export
creed_tibble_to_mock_input <- function(creed_tibble) {
  # Score numeric -> text lookup (inverse of CREED_choices_vocabulary) ----
  score_labels <- c(
    "1" = "Fully Met",
    "2" = "Partly Met",
    "3" = "Not Reported",
    "4" = "Not Met"
  )

  # Build mock input list ----
  mock_input <- list()

  for (i in seq_len(nrow(creed_tibble))) {
    row <- creed_tibble[i, ]
    criterion_id <- row$criterion_id

    # Convert numeric score to text label ----
    score_text <- score_labels[as.character(row$score)]

    # Add score and relevant_data ----
    mock_input[[paste0(criterion_id, "_score")]] <- score_text
    mock_input[[paste0(criterion_id, "_relevant_data")]] <- row$relevant_data

    # RB8 uses _justification; all others use _limitations ----
    if (criterion_id == "RB8") {
      # Strip "Justification: " prefix if present (it gets added back by collect_CREED_data)
      justification <- row$limitations
      justification <- sub("^Justification: ", "", justification)
      mock_input[[paste0(criterion_id, "_justification")]] <- justification
    } else {
      mock_input[[paste0(criterion_id, "_limitations")]] <- row$limitations
    }
  }

  mock_input
}
