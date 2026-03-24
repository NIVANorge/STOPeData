# Test: fct_dummy_data.R ----

# =========================================================================
# SESSION DATA CREATION TESTS
# =========================================================================

test_that("create_dummy_session_data creates valid userData structure", {
  expect_no_error(result <- create_dummy_session_data())
  expect_no_warning(result <- create_dummy_session_data())
  expect_type(result, "list")

  # Check all expected data tibbles are present and populated
  expect_s3_class(result$campaignData, "tbl_df")
  expect_gt(nrow(result$campaignData), 0)

  expect_s3_class(result$referenceData, "tbl_df")
  expect_gt(nrow(result$referenceData), 0)

  expect_s3_class(result$sitesData, "tbl_df")
  expect_gt(nrow(result$sitesData), 0)

  expect_s3_class(result$parametersData, "tbl_df")
  expect_gt(nrow(result$parametersData), 0)

  expect_s3_class(result$compartmentsData, "tbl_df")
  expect_gt(nrow(result$compartmentsData), 0)

  expect_s3_class(result$methodsData, "tbl_df")
  expect_gt(nrow(result$methodsData), 0)

  expect_s3_class(result$samplesData, "tbl_df")
  expect_gt(nrow(result$samplesData), 0)

  expect_s3_class(result$biotaData, "tbl_df")
  expect_gt(nrow(result$biotaData), 0)

  expect_s3_class(result$measurementsData, "tbl_df")
  expect_gt(nrow(result$measurementsData), 0)
})

test_that("create_dummy_session_data sets validity flags to TRUE", {
  result <- create_dummy_session_data()

  expect_true(result$campaignDataValid)
  expect_true(result$referenceDataValid)
  expect_true(result$sitesDataValid)
  expect_true(result$parametersDataValid)
  expect_true(result$compartmentsDataValid)
  expect_true(result$methodsDataValid)
  expect_true(result$samplesDataValid)
  expect_true(result$biotaDataValid)
  expect_true(result$measurementsDataValid)
})

test_that("create_dummy_session_data column structures match initialise functions", {
  result <- create_dummy_session_data()

  expect_equal(names(result$campaignData), names(initialise_campaign_tibble()))
  expect_equal(
    names(result$referenceData),
    names(initialise_references_tibble())
  )
  expect_equal(names(result$sitesData), names(initialise_sites_tibble()))
  expect_equal(
    names(result$parametersData),
    names(initialise_parameters_tibble())
  )
  expect_equal(
    names(result$compartmentsData),
    names(initialise_compartments_tibble())
  )
  expect_equal(names(result$methodsData), names(initialise_methods_tibble()))
  expect_equal(names(result$samplesData), names(initialise_samples_tibble()))
  expect_equal(names(result$biotaData), names(initialise_biota_tibble()))
  expect_equal(
    names(result$measurementsData),
    names(initialise_measurements_tibble())
  )
})
