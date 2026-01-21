# Test: mod_CREED_fct_helpers.R ----
# Basic tests for CREED summary functions

# =========================================================================
# CREED SUMMARY FUNCTION TESTS
# =========================================================================
# Each test verifies:
# 1. Function executes without error or warning on dummy data
# 2. Returns a tibble
# 3. Has expected dimensions (rows x columns)
# 4. Has expected column names
# =========================================================================

test_that("summarise_CREED_reliability returns valid tibble", {
  dummy_data <- create_dummy_session_data()

  expect_no_error(result <- summarise_CREED_reliability(dummy_data))
  expect_no_warning(result <- summarise_CREED_reliability(dummy_data))
  expect_s3_class(result, "tbl_df")

  # Should have 19 rows (RB1-RB19) and 2 columns (field, value)
  expect_equal(nrow(result), 19)
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("field", "value"))
})

test_that("summarise_CREED_relevance returns valid tibble", {
  dummy_data <- create_dummy_session_data()

  expect_no_error(result <- summarise_CREED_relevance(dummy_data))
  expect_no_warning(result <- summarise_CREED_relevance(dummy_data))
  expect_s3_class(result, "tbl_df")

  # Should have 11 rows (RV1-RV11) and 2 columns (field, value)
  expect_equal(nrow(result), 11)
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("field", "value"))
})

# Test: mod_CREED_fct_helpers.R ----
# Tests for CREED helper functions

# =========================================================================
# CREED SUMMARY FUNCTION TESTS
# =========================================================================

test_that("summarise_CREED_reliability returns valid tibble", {
  dummy_data <- create_dummy_session_data()

  expect_no_error(result <- summarise_CREED_reliability(dummy_data))
  expect_no_warning(result <- summarise_CREED_reliability(dummy_data))
  expect_s3_class(result, "tbl_df")

  # Should have 19 rows (RB1-RB19) and 2 columns (field, value)
  expect_equal(nrow(result), 19)
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("field", "value"))
})

test_that("summarise_CREED_relevance returns valid tibble", {
  dummy_data <- create_dummy_session_data()

  expect_no_error(result <- summarise_CREED_relevance(dummy_data))
  expect_no_warning(result <- summarise_CREED_relevance(dummy_data))
  expect_s3_class(result, "tbl_df")

  # Should have 11 rows (RV1-RV11) and 2 columns (field, value)
  expect_equal(nrow(result), 11)
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("field", "value"))
})


# =========================================================================
# COLLECT_CREED_SCORES TESTS
# =========================================================================

test_that("collect_CREED_data returns valid tibble with correct structure", {
  # Minimal criteria config ----

  # Mock inputs  ----
  mock_relevance_config <- CREED_relevance_criteria_config()
  mock_relevance_input <- creed_tibble_to_mock_input(dummy_CREED_relevance_tibble())

  mock_reliability_config <- CREED_reliability_criteria_config()
  mock_reliability_input <- creed_tibble_to_mock_input(dummy_CREED_reliability_tibble())

  expect_no_error(
    result <- collect_CREED_data(mock_relevance_config, mock_relevance_input) |>
      add_row(collect_CREED_data(
        mock_reliability_config,
        mock_reliability_input
      ))
  )
  expect_s3_class(result, "tbl_df")

  # Check structure matches initialise_CREED_data_tibble ----
  expect_equal(names(result), names(initialise_CREED_data_tibble()))
  # check we have as many rows as we started with
  expect_equal(
    !!nrow(result),
    !!nrow(dummy_CREED_relevance_tibble()) +
      !!nrow(dummy_CREED_reliability_tibble())
  )
})


test_that("collect_CREED_data handles RB8 justification special case", {
  criteria_config <- list(
    RB7 = list(title = "LOD/LOQ", type = "Required"),
    RB8 = list(title = "Accreditation/QMS", type = "Required"),
    RB9 = list(title = "Method", type = "Required")
  )

  # Mock input with RB8 using _justification instead of _limitations ----
  mock_input <- list(
    RB7_score = "Fully Met",
    RB7_relevant_data = "LOD provided",
    RB7_limitations = "Some limitation",
    RB8_score = "Partly Met",
    RB8_relevant_data = "ISO 17025 certified",
    RB8_justification = "Lab is fully accredited",
    RB9_score = "Partly Met",
    RB9_relevant_data = "Method referenced",
    RB9_limitations = ""
  )

  result <- collect_CREED_data(criteria_config, mock_input)

  # Check RB8 row has prefixed justification ----
  rb8_row <- result[result$criterion_id == "RB8", ]
  expect_equal(rb8_row$limitations, "Justification: Lab is fully accredited")

  # Check other rows use limitations normally ----
  rb7_row <- result[result$criterion_id == "RB7", ]
  expect_equal(rb7_row$limitations, "Some limitation")
})

test_that("collect_CREED_data handles NULL and empty inputs gracefully", {
  criteria_config <- list(
    RV1 = list(title = "Test Criterion", type = "Required")
  )

  # Mock input with NULLs ----
  mock_input <- list(
    RV1_score = "Fully Met",
    RV1_relevant_data = NULL,
    RV1_limitations = NULL
  )

  expect_no_error(result <- collect_CREED_data(criteria_config, mock_input))
  expect_equal(nrow(result), 1)
  expect_equal(result$relevant_data, "")
  expect_equal(result$limitations, "")
})
