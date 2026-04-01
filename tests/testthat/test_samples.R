## Tests for mod_samples_fct.R
## Business logic for sample combination generation

# Helpers -----------------------------------------------------------------

# Compartment selections in the "merged" picker format (ENVIRON_COMPARTMENT | ENVIRON_COMPARTMENT_SUB)
aquatic_sediment_sel  <- "Aquatic | Aquatic Sediment"
biota_aquatic_sel     <- "Biota | Biota, Aquatic"


# parse_subsamples() ------------------------------------------------------

test_that("parse_subsamples splits a comma-separated string into a character vector", {
  expect_equal(parse_subsamples("1, 2, 3"), c("1", "2", "3"))
  expect_equal(parse_subsamples("A, B, C"), c("A", "B", "C"))
  expect_equal(parse_subsamples("1cm, 2cm, 3cm"), c("1cm", "2cm", "3cm"))
})

test_that("parse_subsamples trims whitespace around each element", {
  expect_equal(parse_subsamples("  1  ,  2  ,  3  "), c("1", "2", "3"))
  expect_equal(parse_subsamples("1,2,3"), c("1", "2", "3"))
})

test_that("parse_subsamples handles a single value", {
  expect_equal(parse_subsamples("1"),   c("1"))
  expect_equal(parse_subsamples("top"), c("top"))
})

test_that("parse_subsamples coerces numeric input to character", {
  expect_equal(parse_subsamples(1),      c("1"))
  expect_type(parse_subsamples(1),       "character")
  expect_type(parse_subsamples("1, 2"),  "character")
})


# parse_compartment_selections() ------------------------------------------

test_that("parse_compartment_selections returns an empty tibble on NULL input", {
  result <- parse_compartment_selections(NULL, eDataDRF::example_compartments_tibble())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(c("ENVIRON_COMPARTMENT", "ENVIRON_COMPARTMENT_SUB", "MEASURED_CATEGORY") %in% names(result)))
})

test_that("parse_compartment_selections returns an empty tibble on empty input", {
  result <- parse_compartment_selections(character(0), eDataDRF::example_compartments_tibble())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("parse_compartment_selections returns correct row for a single valid selection", {
  result <- parse_compartment_selections(
    aquatic_sediment_sel,
    eDataDRF::example_compartments_tibble()
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$ENVIRON_COMPARTMENT,     "Aquatic")
  expect_equal(result$ENVIRON_COMPARTMENT_SUB, "Aquatic Sediment")
  expect_equal(result$MEASURED_CATEGORY,       "External")
})

test_that("parse_compartment_selections returns correct rows for multiple selections", {
  result <- parse_compartment_selections(
    c(aquatic_sediment_sel, biota_aquatic_sel),
    eDataDRF::example_compartments_tibble()
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$ENVIRON_COMPARTMENT, c("Aquatic", "Biota"))
})

test_that("parse_compartment_selections warns on invalid selection format", {
  expect_warning(
    parse_compartment_selections("BadFormatNoSeparator", eDataDRF::example_compartments_tibble()),
    regexp = "Invalid compartment selection format"
  )
})

test_that("parse_compartment_selections warns when compartment combination is not found", {
  expect_warning(
    parse_compartment_selections("Aquatic | NonExistentSub", eDataDRF::example_compartments_tibble()),
    regexp = "Could not find compartment combination"
  )
})


# combination_exists_with_components() ------------------------------------

test_that("combination_exists_with_components returns FALSE on empty existing data", {
  result <- combination_exists_with_components(
    eDataDRF::initialise_samples_tibble(),
    site                   = "SITE-001",
    parameter              = "Copper",
    environ_compartment    = "Aquatic",
    environ_compartment_sub = "Aquatic Sediment",
    measured_category      = "External",
    date                   = "2023-03-15",
    subsample              = "1"
  )
  expect_false(result)
})

test_that("combination_exists_with_components returns TRUE for an exact match", {
  result <- combination_exists_with_components(
    eDataDRF::example_samples_tibble(),
    site                   = "SITE-001",
    parameter              = "Copper",
    environ_compartment    = "Aquatic",
    environ_compartment_sub = "Aquatic Sediment",
    measured_category      = "External",
    date                   = "2023-03-15",
    subsample              = "1"
  )
  expect_true(result)
})

test_that("combination_exists_with_components returns FALSE when date differs", {
  result <- combination_exists_with_components(
    eDataDRF::example_samples_tibble(),
    site                   = "SITE-001",
    parameter              = "Copper",
    environ_compartment    = "Aquatic",
    environ_compartment_sub = "Aquatic Sediment",
    measured_category      = "External",
    date                   = "2024-01-01",   # different date
    subsample              = "1"
  )
  expect_false(result)
})

test_that("combination_exists_with_components returns FALSE when subsample differs", {
  result <- combination_exists_with_components(
    eDataDRF::example_samples_tibble(),
    site                   = "SITE-001",
    parameter              = "Copper",
    environ_compartment    = "Aquatic",
    environ_compartment_sub = "Aquatic Sediment",
    measured_category      = "External",
    date                   = "2023-03-15",
    subsample              = "2"             # different subsample
  )
  expect_false(result)
})


# create_sample_combinations() --------------------------------------------

test_that("create_sample_combinations returns a list with 'combinations' tibble and 'skipped' count", {
  result <- create_sample_combinations(
    sites                  = "SITE-001",
    parameters             = "Copper",
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date("2024-01-15"),
    subsamples             = "1",
    existing_data          = eDataDRF::initialise_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble()
  )
  expect_type(result, "list")
  expect_named(result, c("combinations", "skipped"))
  expect_s3_class(result$combinations, "tbl_df")
  expect_type(result$skipped, "double")
})

test_that("create_sample_combinations generates correct number of combinations", {
  result <- create_sample_combinations(
    sites                  = c("SITE-001", "SITE-002"),
    parameters             = c("Copper", "Lead"),
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date("2024-01-15"),
    subsamples             = "1",
    existing_data          = eDataDRF::initialise_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble()
  )
  # 2 sites × 2 params × 1 compartment × 1 date × 1 subsample = 4
  expect_equal(nrow(result$combinations), 4)
  expect_equal(result$skipped, 0)
})

test_that("create_sample_combinations multiplies correctly across dates and subsamples", {
  result <- create_sample_combinations(
    sites                  = "SITE-001",
    parameters             = "Copper",
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date(c("2024-01-01", "2024-02-01")),
    subsamples             = "1, 2, 3",
    existing_data          = eDataDRF::initialise_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble()
  )
  # 1 × 1 × 1 × 2 dates × 3 subsamples = 6
  expect_equal(nrow(result$combinations), 6)
})

test_that("create_sample_combinations skips combinations already in existing data", {
  # SITE-001 / Copper / Aquatic Sediment / 2023-03-15 / subsample 1 is in example_samples_tibble()
  result <- create_sample_combinations(
    sites                  = "SITE-001",
    parameters             = "Copper",
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date("2023-03-15"),
    subsamples             = "1",
    existing_data          = eDataDRF::example_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble()
  )
  expect_equal(nrow(result$combinations), 0)
  expect_equal(result$skipped, 1)
})

test_that("create_sample_combinations only skips exact duplicates, adds new ones", {
  result <- create_sample_combinations(
    sites                  = "SITE-001",
    parameters             = "Copper",
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date(c("2023-03-15", "2024-06-01")),   # first is duplicate, second is new
    subsamples             = "1",
    existing_data          = eDataDRF::example_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble()
  )
  expect_equal(nrow(result$combinations), 1)
  expect_equal(result$skipped, 1)
  expect_equal(result$combinations$SAMPLING_DATE, "2024-06-01")
})

test_that("create_sample_combinations result contains required columns", {
  result <- create_sample_combinations(
    sites                  = "SITE-001",
    parameters             = "Copper",
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date("2024-01-15"),
    subsamples             = "1",
    existing_data          = eDataDRF::initialise_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble()
  )
  expected_cols <- c(
    "SITE_CODE", "SITE_NAME", "PARAMETER_NAME", "PARAMETER_TYPE",
    "ENVIRON_COMPARTMENT", "ENVIRON_COMPARTMENT_SUB", "MEASURED_CATEGORY",
    "SAMPLING_DATE", "SUBSAMPLE", "SAMPLE_ID"
  )
  expect_true(all(expected_cols %in% names(result$combinations)))
})

test_that("create_sample_combinations populates SITE_NAME and PARAMETER_TYPE from lookup tables", {
  result <- create_sample_combinations(
    sites                  = "SITE-001",
    parameters             = "Copper",
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date("2024-01-15"),
    subsamples             = "1",
    existing_data          = eDataDRF::initialise_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble(),
    available_sites        = eDataDRF::example_sites_tibble(),
    available_parameters   = eDataDRF::example_parameters_tibble()
  )
  expect_equal(unname(result$combinations$SITE_NAME),      "Oslofjord Inner")
  expect_equal(unname(result$combinations$PARAMETER_TYPE), "Stressor")
})

test_that("create_sample_combinations accepts Date objects and date strings equivalently", {
  result_date <- create_sample_combinations(
    sites                  = "SITE-001",
    parameters             = "Copper",
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date("2024-03-01"),
    subsamples             = "1",
    existing_data          = eDataDRF::initialise_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble()
  )
  expect_equal(result_date$combinations$SAMPLING_DATE, "2024-03-01")
})

test_that("create_sample_combinations generates a non-empty SAMPLE_ID for each row", {
  result <- create_sample_combinations(
    sites                  = c("SITE-001", "SITE-002"),
    parameters             = "Lead",
    compartment_selections = aquatic_sediment_sel,
    dates                  = as.Date("2024-01-15"),
    subsamples             = "1",
    existing_data          = eDataDRF::initialise_samples_tibble(),
    available_compartments = eDataDRF::example_compartments_tibble()
  )
  expect_false(any(is.na(result$combinations$SAMPLE_ID)))
  expect_false(any(result$combinations$SAMPLE_ID == ""))
  expect_equal(length(unique(result$combinations$SAMPLE_ID)), 2)  # each row gets a unique ID
})
