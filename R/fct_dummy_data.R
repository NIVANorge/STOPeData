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
#' @import eDataDRF
#' @return A list matching the structure of initialise_userData() with dummy data
#' @export
create_dummy_session_data <- function() {
  # Start from the proper userData structure
  userData <- initialise_userData()

  # Populate with dummy data ----
  userData$campaignData <- example_campaign_tibble()
  userData$referenceData <- example_references_tibble()
  userData$sitesData <- example_sites_tibble()
  userData$parametersData <- example_parameters_tibble()
  userData$compartmentsData <- example_compartments_tibble()
  userData$methodsData <- example_methods_tibble()
  userData$samplesData <- example_samples_tibble()
  userData$biotaData <- example_biota_tibble()
  userData$measurementsData <- example_measurements_tibble()

  # Populate CREED data ----
  userData$creedReliability <- example_CREED_reliability_tibble()
  userData$creedRelevance <- example_CREED_relevance_tibble()
  userData$creedScores <- example_CREED_scores_tibble()

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
#' @return NULL invisibly.
#' @importFrom shiny showNotification updateNavbarPage
#' @import eDataDRF
#' @importFrom golem print_dev
#' @export
populate_session_with_dummy_data <- function(
  session,
  navigate_to = NULL,
  parent_session = NULL
) {
  # Store directly in session userData for immediate use

  # Campaign data
  session$userData$reactiveValues$campaignData <- eDataDRF::example_campaign_tibble()
  print_dev("Populated campaign data with dummy data")

  # References data
  session$userData$reactiveValues$referenceData <- eDataDRF::example_references_tibble()
  print_dev("Populated references data with dummy data")

  # Sites data
  session$userData$reactiveValues$sitesData <- eDataDRF::example_sites_tibble()
  print_dev("Populated sites data with dummy data")

  # Parameters data
  session$userData$reactiveValues$parametersData <- eDataDRF::example_parameters_tibble()
  print_dev("Populated parameters data with dummy data")

  # Compartments data
  session$userData$reactiveValues$compartmentsData <- eDataDRF::example_compartments_tibble()
  print_dev("Populated compartments data with dummy data")

  # Biota data # uses LLM data to trigger lookup
  session$userData$reactiveValues$biotaDataLLM <- eDataDRF::example_biota_tibble()
  print_dev("Populated biota data with dummy data")

  # Methods data
  session$userData$reactiveValues$methodsData <- eDataDRF::example_methods_tibble()
  print_dev("Populated methods data with dummy data")

  # Also signal that "save extraction" is complete so that campaign/references
  # form-population observers (which listen on saveExtractionComplete) fire
  session$userData$reactiveValues$saveExtractionComplete <- TRUE
  session$userData$reactiveValues$saveExtractionSuccessful <- TRUE

  # Navigate if requested
  if (!is.null(navigate_to) && !is.null(parent_session)) {
    updateNavbarPage(
      session = parent_session,
      inputId = "main-page",
      selected = navigate_to
    )
  }
  invisible(NULL)
}


#' Convert CREED Tibble to Mock Input List
#'
#' @description Converts a CREED data tibble (from dummy_CREED_reliability_tibble()
#' or dummy_CREED_relevance_tibble()) into a mock shiny input list object suitable for testing
#' collect_CREED_data().
#'
#' @param creed_tibble A tibble with columns: criterion_id, relevant_data, score, limitations
#' @return A named list mimicking the structure of a Shiny input object, with entries
#'   named `criterionId_score`, `criterionId_relevant_data`, and `criterionId_limitations`
#'   (or `criterionId_justification` for RB8).
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
