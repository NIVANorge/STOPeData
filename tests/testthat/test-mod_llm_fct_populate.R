library(testthat)
library(tibble)
library(dplyr)
library(glue)

# Source internal functions directly (they are @noRd / unexported).
# golem::print_dev() is called inside some functions but not validate_species_against_database,
# so we stub it to avoid a hard dependency on the full package environment.
if (!requireNamespace("golem", quietly = TRUE) || !exists("print_dev")) {
  print_dev <- function(...) invisible(NULL)
}
source(testthat::test_path("../../R/mod_llm_fct_populate.R"))

# Mini species database covering all test cases:
#   - Unique scientific + common name pairs (Gadus morhua, Salmo salar, Anguilla anguilla)
#   - Two species sharing the same common name "mussel" (for the multiple-hits case)
test_species_db <- tibble(
  SPECIES_NAME = c(
    "Gadus morhua",
    "Salmo salar",
    "Anguilla anguilla",
    "Mytilus edulis",
    "Mytilus galloprovincialis"
  ),
  SPECIES_COMMON_NAME = c(
    "Atlantic cod",
    "Atlantic salmon",
    "European eel",
    "mussel",
    "mussel"
  )
)

make_biota <- function(...) {
  tibble(SAMPLE_SPECIES = c(...))
}

# ------------------------------------------------------------
# validate_species_against_database
# ------------------------------------------------------------

test_that("returns clean result when exact scientific name is found", {
  result <- validate_species_against_database(
    make_biota("Gadus morhua"),
    test_species_db
  )
  expect_false(result$has_warnings)
  expect_match(result$validation_text, "\u2713 Found as scientific name")
  expect_match(result$validation_text, "Gadus morhua")
})

test_that("scientific name match is case-insensitive", {
  result <- validate_species_against_database(
    make_biota("gadus morhua"),
    test_species_db
  )
  expect_false(result$has_warnings)
  expect_match(result$validation_text, "\u2713 Found as scientific name")
})

test_that("common name match produces a warning flag", {
  result <- validate_species_against_database(
    make_biota("Atlantic salmon"),
    test_species_db
  )
  expect_true(result$has_warnings)
  expect_match(result$validation_text, "\u26A0 Found as common name")
  expect_match(result$validation_text, "Salmo salar")
})

test_that("common name matching is also case-insensitive", {
  result <- validate_species_against_database(
    make_biota("atlantic salmon"),
    test_species_db
  )
  expect_true(result$has_warnings)
  expect_match(result$validation_text, "\u26A0 Found as common name")
})

test_that("multiple species sharing a common name produces a warning", {
  result <- validate_species_against_database(
    make_biota("mussel"),
    test_species_db
  )
  expect_true(result$has_warnings)
  expect_match(
    result$validation_text,
    "Multiple species match this common name"
  )
  # Both scientific names should be listed
  expect_match(result$validation_text, "Mytilus edulis")
  expect_match(result$validation_text, "Mytilus galloprovincialis")
})

test_that("species not in database produces a warning", {
  result <- validate_species_against_database(
    make_biota("Homo sapiens"),
    test_species_db
  )
  expect_true(result$has_warnings)
  expect_match(result$validation_text, "Species not found in database")
})

test_that("summary counts are correct for mixed inputs", {
  result <- validate_species_against_database(
    make_biota(
      "Gadus morhua", # scientific match
      "Atlantic salmon", # common name match
      "Homo sapiens" # not found
    ),
    test_species_db
  )
  expect_match(result$validation_text, "Species processed: 3")
  expect_match(result$validation_text, "Found as scientific names: 1")
  expect_match(result$validation_text, "Found as common names: 1")
  expect_match(result$validation_text, "Not found in database: 1")
  expect_true(result$has_warnings)
})

test_that("empty biota data returns early with no warnings", {
  result <- validate_species_against_database(
    tibble(SAMPLE_SPECIES = character(0)),
    test_species_db
  )
  expect_false(result$has_warnings)
  expect_equal(result$validation_text, "No biota samples to validate.")
})

test_that("biota data with only blank/NA species names returns warning", {
  result <- validate_species_against_database(
    tibble(SAMPLE_SPECIES = c(NA, "")),
    test_species_db
  )
  expect_true(result$has_warnings)
  expect_match(
    result$validation_text,
    "No species names provided for validation."
  )
})

test_that("duplicate species names in input are only processed once", {
  result <- validate_species_against_database(
    make_biota("Gadus morhua", "Gadus morhua", "Gadus morhua"),
    test_species_db
  )
  expect_match(result$validation_text, "Species processed: 1")
})
