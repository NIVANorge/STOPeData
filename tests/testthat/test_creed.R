# test-creed-functions.R ----
# test big overarching CREED functions that a) get lots of data from the right sources and b) send it to the right sinks

# from fct_dummy_data.R
sessionData <- create_dummy_session_data()
# --------------------

# Tests: Main summary function ----
test_that("summarise_CREED_relevance doesn't error", {
  expect_no_error(result <- summarise_CREED_relevance(sessionData))
})

test_that("summarise_CREED_reliability doesn't error", {
  expect_no_error(result <- summarise_CREED_reliability(sessionData))
})


test_that("summarise_CREED_details returns tibble with correct structure", {
  result <- summarise_CREED_details(sessionData)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("field", "value"))
  expect_equal(nrow(result), 11)

  expected_fields <- c(
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
  )
  expect_equal(result$field, expected_fields)
})

test_that("summarise_CREED_details correctly summarises reference data", {
  result <- summarise_CREED_details(sessionData)

  source_value <- result$value[result$field == "source"]
  expect_true(grepl("Smith, J.", source_value))
  expect_true(grepl("2023", source_value))
})

test_that("summarise_CREED_details correctly counts sites and samples", {
  result <- summarise_CREED_details(sessionData)

  num_sites <- result$value[result$field == "num_sites"]
  num_samples <- result$value[result$field == "num_samples"]

  expect_equal(num_sites, "2")
  expect_equal(num_samples, "2")
})

test_that("summarise_CREED_details separates sampling and analytical methods", {
  result <- summarise_CREED_details(sessionData)

  sampling_methods <- result$value[result$field == "sampling_methods"]
  analytical_methods <- result$value[result$field == "analytical_methods"]

  expect_true(grepl("Sampling Protocol", sampling_methods))
  expect_true(grepl("Grab", sampling_methods))
  expect_true(grepl("Analytical Protocol", analytical_methods))
  expect_true(grepl("ICP-MS", analytical_methods))
})

test_that("summarise_CREED_details handles missing data", {
  sessionData <- list(
    referenceData = NULL,
    parametersData = NULL,
    compartmentsData = NULL,
    sitesData = NULL,
    samplesData = NULL,
    measurementsData = NULL,
    methodsData = NULL
  )

  result <- summarise_CREED_details(sessionData)

  expect_true(all(result$value == "Relevant data not found"))
})

test_that("summarise_CREED_details calculates LOQ/LOD info correctly", {
  result <- summarise_CREED_details(sessionData)

  loq_info <- result$value[result$field == "loq_info"]

  expect_true(grepl("LOQ:", loq_info))
  expect_true(grepl("LOD:", loq_info))
  expect_true(grepl("0.05 to 1", loq_info)) # Range of LOQ values
})

test_that("summarise_CREED_details handles NA dates correctly", {
  sessionData$samplesData$SAMPLING_DATE <- as.Date(NA)

  result <- summarise_CREED_details(sessionData)
  sampling_period <- result$value[result$field == "sampling_period"]

  expect_equal(sampling_period, "Relevant data not found")
})

test_that("summarise_CREED_details deduplicates values correctly", {
  # Add duplicate compartment
  sessionData$compartmentsData <- add_row(
    sessionData$compartmentsData,
    tibble(
      ENVIRON_COMPARTMENT = "Aquatic"
    )
  )

  result <- summarise_CREED_details(sessionData)
  medium <- result$value[result$field == "medium"]

  # Should not have "Aquatic Sediment" repeated multiple times in output
  aquatic_sediment_count <- length(gregexpr("Aquatic Sediment", medium)[[1]])
  expect_lte(aquatic_sediment_count, 2) # Once in the value, once in potential plural
})

# test-creed-reliability-functions.R ----

test_that("summarise_CREED_reliability correctly summarises compartments and protocols for RB1", {
  result <- summarise_CREED_reliability(sessionData)

  rb1_value <- result$value[result$field == "RB1"]

  # Should contain compartments
  expect_true(grepl("compartment", rb1_value))
  expect_true(grepl("Aquatic Sediment", rb1_value))

  # Should contain protocol information
  expect_true(grepl("protocol", rb1_value))
})

test_that("summarise_CREED_reliability correctly summarises sites for RB4", {
  result <- summarise_CREED_reliability(sessionData)

  rb4_value <- result$value[result$field == "RB4"]

  expect_true(grepl("2 sites", rb4_value))
})

test_that("summarise_CREED_reliability correctly calculates date range for RB5", {
  result <- summarise_CREED_reliability(sessionData)

  rb5_value <- result$value[result$field == "RB5"]

  expect_true(grepl("2023-03-15", rb5_value))
  expect_true(grepl("2023-04-20", rb5_value))
  expect_true(grepl("to", rb5_value))
})

test_that("summarise_CREED_reliability reuses sampling protocol summary for RB2, RB13", {
  result <- summarise_CREED_reliability(sessionData)

  rb2_value <- result$value[result$field == "RB2"]
  rb13_value <- result$value[result$field == "RB13"]

  # All three should be identical
  expect_equal(rb2_value, rb13_value)

  # Should contain sampling protocol info
  expect_true(grepl("Sampling Protocol", rb2_value))
})

test_that("summarise_CREED_reliability reuses analytical protocol summary for RB6, RB10, RB11, RB12", {
  result <- summarise_CREED_reliability(sessionData)

  rb6_value <- result$value[result$field == "RB6"]
  rb10_value <- result$value[result$field == "RB10"]
  rb11_value <- result$value[result$field == "RB11"]
  rb12_value <- result$value[result$field == "RB12"]

  # All four should be identical
  expect_equal(rb6_value, rb10_value)
  expect_equal(rb6_value, rb11_value)
  expect_equal(rb6_value, rb12_value)

  # Should contain analytical protocol info
  expect_true(grepl("Analytical Protocol", rb6_value))
})

test_that("summarise_CREED_reliability calculates LOD/LOQ for RB7", {
  result <- summarise_CREED_reliability(sessionData)

  rb7_value <- result$value[result$field == "RB7"]

  expect_true(grepl("LOQ:", rb7_value))
  expect_true(grepl("LOD:", rb7_value))
  expect_true(grepl("0.05 to 1", rb7_value)) # LOQ range
})

test_that("summarise_CREED_reliability calculates significant figures for RB15", {
  result <- summarise_CREED_reliability(sessionData)

  rb15_value <- result$value[result$field == "RB15"]

  expect_true(grepl("significant figure", rb15_value))
})

test_that("summarise_CREED_reliability leaves blank fields empty for RB8, RB16, RB17, RB19", {
  result <- summarise_CREED_reliability(sessionData)

  expect_equal(
    result$value[result$field == "RB8"],
    "Relevant data not collected by app. Please complete manually."
  )
  expect_equal(
    result$value[result$field == "RB16"],
    "Relevant data not collected by app. Please complete manually."
  )
  expect_equal(
    result$value[result$field == "RB17"],
    "Relevant data not collected by app. Please complete manually."
  )
  expect_equal(
    result$value[result$field == "RB19"],
    "Relevant data not collected by app. Please complete manually."
  )
})

test_that("summarise_CREED_reliability handles missing data gracefully", {
  sessionData <- list(
    compartmentsData = NULL,
    methodsData = NULL,
    sitesData = NULL,
    samplesData = NULL,
    measurementsData = NULL
  )

  result <- summarise_CREED_reliability(sessionData)

  # Should still return 19 rows
  expect_equal(nrow(result), 19)

  # Non-empty fields should say "Relevant data not found"
  non_empty_fields <- c(
    "RB1",
    "RB2",
    "RB3",
    "RB4",
    "RB5",
    "RB6",
    "RB7",
    "RB9",
    "RB10",
    "RB11",
    "RB12",
    "RB13",
    "RB14",
    "RB15",
    "RB18"
  )
  for (field in non_empty_fields) {
    expect_equal(
      result$value[result$field == field],
      "Relevant data not found"
    )
  }
})

test_that("summarise_CREED_reliability handles uncertainty types for RB14 and RB18", {
  # Add uncertainty data
  sessionData$measurementsData$UNCERTAINTY_TYPE <- c("SD", "SD", NA)
  sessionData$measurementsData$MEASUREMENT_COMMENT <- c(
    NA,
    NA,
    "Below LOQ - biota sample"
  )

  result <- summarise_CREED_reliability(sessionData)

  rb14_value <- result$value[result$field == "RB14"]
  rb18_value <- result$value[result$field == "RB18"]

  # RB14 should have both uncertainty types and comments
  expect_true(grepl("Uncertainty types", rb14_value))
  expect_true(grepl("Measurement comments", rb14_value))

  # RB18 should have only uncertainty types
  expect_true(grepl("Uncertainty types", rb18_value))
  expect_true(grepl("SD", rb18_value))
})
