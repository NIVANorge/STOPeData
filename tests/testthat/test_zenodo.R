# tests/testthat/test_zenodo.R ----
# Basic tests for fct_zenodo.R helper functions and static data

# =========================================================================
# STATIC DATA TESTS
# =========================================================================

test_that("zenodo_licenses loads without error and has expected structure", {
  expect_no_error(lic <- zenodo_licenses)
  expect_s3_class(lic, "tbl_df")
  expect_true(nrow(lic) > 0)
  expect_true(all(c("id", "title", "popular") %in% names(lic)))
  expect_type(lic$id, "character")
  expect_type(lic$title, "character")
})

test_that("zenodo_resource_types is a non-empty named character vector", {
  expect_no_error(rt <- zenodo_resource_types)
  expect_type(rt, "character")
  expect_true(length(rt) > 0)
  expect_false(is.null(names(rt)))
  expect_true(all(nzchar(names(rt))))
  expect_true(all(nzchar(rt)))
  # Spot-check the dataset type is present
  expect_true("dataset" %in% rt)
})

# =========================================================================
# generate_zenodo_readme() TESTS
# =========================================================================

# Shared generic inputs used across tests
generic_authors <- list(
  list(first = "Jane", last = "Smith",  affiliation = "Test University", orcid = "0000-0000-0000-0001"),
  list(first = "John", last = "Doe",    affiliation = "",                orcid = "")
)

test_that("generate_zenodo_readme returns a single character string", {
  result <- generate_zenodo_readme(
    title       = "Test Dataset",
    description = "A dataset for testing purposes.",
    authors     = generic_authors,
    license_id  = "cc-by-4.0"
  )
  expect_no_error(result)
  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(nzchar(result))
})

test_that("generate_zenodo_readme contains all expected section headers", {
  result <- generate_zenodo_readme(
    title       = "Test Dataset",
    description = "A dataset for testing purposes.",
    authors     = generic_authors,
    license_id  = "cc-by-4.0"
  )
  expect_true(grepl("^# Test Dataset",    result))
  expect_true(grepl("## What is this\\?", result))
  expect_true(grepl("## Authors",         result))
  expect_true(grepl("## How to cite",     result))
  expect_true(grepl("## License",         result))
  expect_true(grepl("## Contact",         result))
})

test_that("generate_zenodo_readme interpolates title, description, and authors", {
  result <- generate_zenodo_readme(
    title       = "My Special Dataset",
    description = "Contains marine pollution data.",
    authors     = generic_authors,
    license_id  = "cc-by-4.0"
  )
  expect_true(grepl("My Special Dataset",       result))
  expect_true(grepl("Contains marine pollution", result))
  expect_true(grepl("Jane Smith",               result))
  expect_true(grepl("Primary Author",           result))
  expect_true(grepl("John Doe",                 result))
  expect_true(grepl("Author 2",                 result))
})

test_that("generate_zenodo_readme includes affiliation and ORCID when provided", {
  result <- generate_zenodo_readme(
    title       = "Test Dataset",
    description = "Description.",
    authors     = generic_authors,
    license_id  = "cc-by-4.0"
  )
  expect_true(grepl("Test University",           result))
  expect_true(grepl("0000-0000-0000-0001",       result))
})

test_that("generate_zenodo_readme includes contact info when provided", {
  result <- generate_zenodo_readme(
    title         = "Test Dataset",
    description   = "Description.",
    authors       = generic_authors,
    license_id    = "cc-by-4.0",
    contact_name  = "Data Manager",
    contact_email = "data@example.com"
  )
  expect_true(grepl("Data Manager",    result))
  expect_true(grepl("data@example.com", result))
})

test_that("generate_zenodo_readme uses license title from zenodo_licenses when id is known", {
  # cc-by-4.0 should resolve to a human-readable title, not the raw id
  result <- generate_zenodo_readme(
    title       = "Test Dataset",
    description = "Description.",
    authors     = generic_authors,
    license_id  = "cc-by-4.0"
  )
  # The license display should appear under the License section
  expect_true(grepl("## License", result))
  expect_true(grepl("Released under", result))
})

test_that("generate_zenodo_readme falls back to license_id when id is unrecognised", {
  result <- generate_zenodo_readme(
    title       = "Test Dataset",
    description = "Description.",
    authors     = generic_authors,
    license_id  = "unknown-license-xyz"
  )
  expect_true(grepl("unknown-license-xyz", result))
})

test_that("generate_zenodo_readme uses placeholder text for blank title and description", {
  result <- generate_zenodo_readme(
    title       = "",
    description = "",
    authors     = generic_authors,
    license_id  = "cc-by-4.0"
  )
  expect_true(grepl("\\[Dataset Title\\]", result))
  expect_true(grepl("\\[Description\\]",   result))
})

test_that("generate_zenodo_readme handles a single author correctly", {
  single_author <- list(
    list(first = "Alice", last = "Brown", affiliation = "", orcid = "")
  )
  result <- generate_zenodo_readme(
    title       = "Single Author Dataset",
    description = "Description.",
    authors     = single_author,
    license_id  = "cc-by-4.0"
  )
  expect_no_error(result)
  expect_true(grepl("Primary Author", result))
  expect_false(grepl("Author 2",      result))
  # Citation format: "Brown A."
  expect_true(grepl("Brown A\\.",     result))
})


# =========================================================================
# parse_author_string() TESTS
# =========================================================================

test_that("parse_author_string returns empty list for NULL, NA, and blank input", {
  expect_equal(parse_author_string(NULL),  list())
  expect_equal(parse_author_string(NA),    list())
  expect_equal(parse_author_string(""),    list())
  expect_equal(parse_author_string("   "), list())
})

test_that("parse_author_string parses a single 'Last, First' author", {
  result <- parse_author_string("Smith, J.")
  expect_length(result, 1)
  expect_equal(result[[1]]$last,  "Smith")
  expect_equal(result[[1]]$first, "J.")
})

test_that("parse_author_string parses multiple semicolon-separated authors", {
  result <- parse_author_string("Smith, J.; Jones, A.; Williams, B.")
  expect_length(result, 3)
  expect_equal(result[[1]]$last,  "Smith")
  expect_equal(result[[2]]$last,  "Jones")
  expect_equal(result[[3]]$last,  "Williams")
  expect_equal(result[[3]]$first, "B.")
})

test_that("parse_author_string sets first to empty string when no comma in entry", {
  result <- parse_author_string("Smith")
  expect_equal(result[[1]]$last,  "Smith")
  expect_equal(result[[1]]$first, "")
})

test_that("parse_author_string trims whitespace around names", {
  result <- parse_author_string("  Smith  ,  J.  ;  Jones  ,  A.  ")
  expect_equal(result[[1]]$last,  "Smith")
  expect_equal(result[[1]]$first, "J.")
  expect_equal(result[[2]]$last,  "Jones")
})

test_that("parse_author_string is consistent with example_references_tibble() author format", {
  author_str <- eDataDRF::example_references_tibble()$AUTHOR
  result <- parse_author_string(author_str)
  # example data has "Smith, J.; Jones, A.; Williams, B."
  expect_length(result, 3)
  expect_equal(result[[1]]$last,  "Smith")
  expect_equal(result[[1]]$first, "J.")
})


# =========================================================================
# extract_campaign_name() TESTS
# (lives in fct_download.R; directly used by the Zenodo module for pre-filling)
# =========================================================================

test_that("extract_campaign_name returns NULL for NULL input", {
  expect_null(extract_campaign_name(NULL))
})

test_that("extract_campaign_name returns NULL for empty tibble", {
  expect_null(extract_campaign_name(eDataDRF::initialise_campaign_tibble()))
})

test_that("extract_campaign_name returns the campaign name from example data", {
  result <- extract_campaign_name(eDataDRF::example_campaign_tibble())
  expect_type(result, "character")
  expect_equal(result, "Test Campaign 2023: Heavy Metals in Coastal Sediments")
})

test_that("extract_campaign_name returns NULL when CAMPAIGN_NAME is NA", {
  bad_data <- eDataDRF::example_campaign_tibble()
  bad_data$CAMPAIGN_NAME[1] <- NA_character_
  expect_null(extract_campaign_name(bad_data))
})

test_that("extract_campaign_name returns NULL when CAMPAIGN_NAME column is absent", {
  bad_data <- eDataDRF::example_campaign_tibble()[, names(eDataDRF::example_campaign_tibble()) != "CAMPAIGN_NAME"]
  expect_null(extract_campaign_name(bad_data))
})
