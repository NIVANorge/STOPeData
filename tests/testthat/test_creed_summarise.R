# test-creed-functions.R ----
# tests for all the little CREED summarise_* functions designed to provide a text summary of tables or individual variables

# Setup: Create dummy data matching actual structure ----
dummy <- create_dummy_session_data()

# Tests for calculate_coordinate_precision() ----
test_that("calculate_coordinate_precision finds minimum decimal places", {
  result <- calculate_coordinate_precision(
    dummy$sitesData$LATITUDE,
    dummy$sitesData$LONGITUDE
  )
  # Lat: 59.9139 (4), 60.3913 (4)
  # Lon: 10.7522 (4), 5.3221 (4)
  expect_equal(result, 4)
})

test_that("calculate_coordinate_precision handles integer coordinates", {
  lat <- c(60, 61, 62)
  lon <- c(10, 11, 12)
  result <- calculate_coordinate_precision(lat, lon)
  expect_equal(result, 0)
})

test_that("calculate_coordinate_precision returns N/A for all NA", {
  lat <- c(NA, NA, NA)
  lon <- c(NA, NA, NA)
  result <- calculate_coordinate_precision(lat, lon)
  expect_equal(result, "N/A")
})

# Tests for summarise_sites() ----
test_that("summarise_sites returns full summary with all parameters TRUE", {
  result <- summarise_sites(
    dummy$sitesData,
    COUNTRY_ISO = TRUE,
    OCEAN_IHO = TRUE,
    SITE_GEOGRAPHIC_FEATURE = TRUE,
    SITE_GEOGRAPHIC_FEATURE_SUB = TRUE,
    PRECISION = TRUE
  )
  expect_match(result, "2 sites")
  expect_match(result, "NO") # Country code
  expect_match(result, "Skagerrak, North Sea")
  expect_match(result, "Distinct features")
  expect_match(result, "Lowest coordinate precision: 4")
})

test_that("summarise_sites respects FALSE parameters", {
  result <- summarise_sites(
    dummy$sitesData,
    COUNTRY_ISO = FALSE,
    OCEAN_IHO = FALSE,
    PRECISION = FALSE
  )
  expect_match(result, "2 sites")
  expect_false(grepl("Countries", result))
  expect_false(grepl("Areas", result))
  expect_false(grepl("precision", result))
})

test_that("summarise_sites handles coordinate precision correctly", {
  # Create custom test data with varying decimal places
  test_coords <- tibble(
    SITE_CODE = c("S1", "S2", "S3"),
    SITE_NAME = c("Site 1", "Site 2", "Site 3"),
    COUNTRY_ISO = c("NO", "NO", "SE"),
    OCEAN_IHO = c("Oslo", "Bergen", "Stockholm"),
    SITE_GEOGRAPHIC_FEATURE = c("Coastal, fjord", "Coastal, fjord", "Lake"),
    SITE_GEOGRAPHIC_FEATURE_SUB = c("Water column", "Sediment", "Water column"),
    LATITUDE = c(60.1, 60.12, 60.123),
    LONGITUDE = c(10.1234, 10.12, 10.1),
    SITE_COORDINATE_SYSTEM = rep("WGS84", 3),
    ALTITUDE_VALUE = rep(NA_real_, 3),
    ALTITUDE_UNIT = rep(NA_character_, 3),
    ENTERED_BY = rep("test_user", 3),
    ENTERED_DATE = rep("2023-07-01", 3),
    SITE_COMMENT = rep(NA_character_, 3)
  )

  result <- summarise_sites(test_coords, PRECISION = TRUE)
  expect_match(result, "Lowest coordinate precision: 1")
})

# Test: summarise_date_range ----
test_that("summarise_date_range handles various date inputs correctly", {
  # Single date
  single_date <- as.Date("2024-01-15")
  expect_equal(
    summarise_date_range(single_date),
    "2024-01-15 (n=1)"
  )

  # Date range with duplicates
  date_range <- as.Date(c(
    "2024-01-15",
    "2024-01-15",
    "2024-01-20",
    "2024-03-20"
  ))
  expect_equal(
    summarise_date_range(date_range),
    "2024-01-15 to 2024-03-20 (n=3, 65 days (to nearest day))"
  )

  # All same date (multiple entries)
  same_dates <- rep(as.Date("2024-01-15"), 5)
  expect_equal(
    summarise_date_range(same_dates),
    "2024-01-15 (n=1)"
  )

  # NULL input
  expect_equal(
    summarise_date_range(NULL),
    "Relevant data not found"
  )

  # Empty vector
  expect_equal(
    summarise_date_range(as.Date(character(0))),
    "Relevant data not found"
  )

  # All NA values
  expect_equal(
    summarise_date_range(as.Date(c(NA, NA, NA))),
    "Relevant data not found"
  )

  # Mix of valid and NA dates
  mixed_dates <- as.Date(c("2024-01-15", NA, "2024-02-15"))
  expect_equal(
    summarise_date_range(mixed_dates),
    "2024-01-15 to 2024-02-15 (n=2, 31 days (to nearest day))"
  )
})
