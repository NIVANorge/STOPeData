# tests/testthat/test-parameter_creation.r ----

# Setup ----
test_that("initialise_parameters_tibble creates correct structure", {
  result <- initialise_parameters_tibble()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 10)

  expected_cols <- c(
    "PARAMETER_TYPE",
    "PARAMETER_TYPE_SUB",
    "MEASURED_TYPE",
    "PARAMETER_NAME",
    "PARAMETER_NAME_SUB",
    "INCHIKEY_SD",
    "PUBCHEM_CID",
    "CAS_RN",
    "ENTERED_BY",
    "PARAMETER_COMMENT"
  )
  expect_equal(names(result), expected_cols)
})

# create_new_parameter ----
test_that("create_new_parameter matches initialise structure", {
  result <- create_new_parameter("Chemical", "TestUser")
  template <- initialise_parameters_tibble()

  # Same column names and types
  expect_equal(names(result), names(template))
  expect_equal(sapply(result, class), sapply(template, class))

  # Has exactly one row
  expect_equal(nrow(result), 1)
})

test_that("create_new_parameter populates correct fields", {
  result <- create_new_parameter("Physical", "TestUser")

  expect_equal(result$PARAMETER_TYPE, "Physical")
  expect_equal(result$ENTERED_BY, "TestUser")
  expect_equal(result$MEASURED_TYPE, "Concentration")

  # Check blank fields are indeed blank
  expect_equal(result$PARAMETER_TYPE_SUB, "")
  expect_equal(result$PARAMETER_NAME, "")
  expect_equal(result$PARAMETER_COMMENT, "")
})

# create_existing_parameter ----
test_that("create_existing_parameter matches initialise structure", {
  dummy_params <- tibble(
    PARAMETER_NAME = "TestParam",
    PARAMETER_TYPE_SUB = "SubType",
    MEASURED_TYPE = "Mass",
    PARAMETER_NAME_SUB = "SubName",
    INCHIKEY_SD = "TESTKEY",
    PUBCHEM_CID = 12345,
    CAS_RN = "123-45-6",
    ENTERED_BY = "DummyUser"
  )

  result <- create_existing_parameter(
    "Chemical",
    "TestParam",
    dummy_params
  )

  template <- initialise_parameters_tibble()

  # Same column names and types
  expect_equal(names(result), names(template))
  expect_equal(sapply(result, class), sapply(template, class))

  # Has exactly one row
  expect_equal(nrow(result), 1)
})

test_that("create_existing_parameter finds parameter in dummy_parameters", {
  dummy_params <- tibble(
    PARAMETER_NAME = "Lead",
    PARAMETER_TYPE_SUB = "Metal",
    MEASURED_TYPE = "Mass",
    PARAMETER_NAME_SUB = "",
    INCHIKEY_SD = "",
    PUBCHEM_CID = 5352425,
    CAS_RN = "7439-92-1",
    ENTERED_BY = "System"
  )

  result <- create_existing_parameter(
    "Chemical",
    "Lead",
    dummy_params
  )

  expect_equal(result$PARAMETER_TYPE, "Chemical")
  expect_equal(result$PARAMETER_NAME, "Lead")
  expect_equal(result$PARAMETER_TYPE_SUB, "Metal")
  expect_equal(result$CAS_RN, "7439-92-1")
  expect_equal(result$ENTERED_BY, "System")
})

test_that("create_existing_parameter falls back to session_parameters", {
  dummy_params <- tibble(
    PARAMETER_NAME = "OtherParam",
    ENTERED_BY = "Dummy"
  )

  session_params <- tibble(
    PARAMETER_TYPE = "Biological",
    PARAMETER_NAME = "TestParam",
    PARAMETER_TYPE_SUB = "Marker",
    MEASURED_TYPE = "Activity",
    PARAMETER_NAME_SUB = "",
    INCHIKEY_SD = "",
    PUBCHEM_CID = "",
    CAS_RN = "",
    ENTERED_BY = "SessionUser"
  )

  result <- create_existing_parameter(
    "Biological",
    "TestParam",
    dummy_params,
    session_params
  )

  expect_equal(result$PARAMETER_NAME, "TestParam")
  expect_equal(result$MEASURED_TYPE, "Activity")
  expect_equal(result$ENTERED_BY, "SessionUser")
})

test_that("create_existing_parameter returns NULL when not found", {
  dummy_params <- tibble(
    PARAMETER_NAME = "Something",
    ENTERED_BY = "User"
  )

  result <- create_existing_parameter(
    "Chemical",
    "NonExistent",
    dummy_params
  )

  expect_null(result)
})

# Integration tests ----
test_that("all three functions produce compatible structures", {
  template <- initialise_parameters_tibble()
  new_param <- create_new_parameter("Test", "User")

  setwd("../../") # set to root so that we can load source data from the right dir
  dummy_params <- parameters_vocabulary()
  existing_param <- create_existing_parameter(
    "Quality parameter",
    "Chlorophyll a",
    dummy_params
  )
  setwd("tests/testthat") # and back again

  # Should be able to bind these together without issues
  combined <- bind_rows(template, new_param, existing_param)

  expect_equal(nrow(combined), 2)
  expect_equal(ncol(combined), 10)
  expect_equal(names(combined), names(template))
})

test_that("create_new_parameter() works and uses the correct format", {
  expect_no_error(create_new_parameter(
    param_type = "Quality Parameter",
    entered_by = "Tim Testing"
  ))

  # test should fail unless all columns are compatible with initialise_parameters_tibble()
})
