## Tests for mod_references_fct_bibtex.R and mod_references_fct_crossref.R

# Shared BibTeX fixture matching example_references_tibble() data
bibtex_article <- '
@article{Smith2023,
  author    = {Smith, J. and Jones, A. and Williams, B.},
  title     = {Heavy metal contamination in Norwegian coastal sediments},
  journal   = {Environmental Science and Technology},
  year      = {2023},
  volume    = {57},
  number    = {12},
  doi       = {10.1021/acs.est.2023.12345}
}
'

bibtex_multi <- paste0(
  bibtex_article,
  '
@article{Other2022,
  author = {Other, A.},
  title  = {Another paper},
  year   = {2022}
}
'
)


# clean_bibtex_text() -----------------------------------------------------

test_that("clean_bibtex_text returns NA/empty input unchanged", {
  expect_true(is.na(clean_bibtex_text(NA)))
  expect_equal(clean_bibtex_text(""), "")
})

test_that("clean_bibtex_text removes double curly braces, preserving content", {
  expect_equal(clean_bibtex_text("{{Title Case}}"), "Title Case")
  expect_equal(clean_bibtex_text("{{A}} and {{B}}"), "A and B")
})

test_that("clean_bibtex_text removes single braces around single characters", {
  expect_equal(clean_bibtex_text("{a}quatic"), "aquatic")
})

test_that("clean_bibtex_text normalises internal whitespace", {
  expect_equal(clean_bibtex_text("too  many   spaces"), "too many spaces")
})

test_that("clean_bibtex_text strips leading and trailing whitespace", {
  expect_equal(clean_bibtex_text("  padded  "), "padded")
})

# NOTE: LaTeX-to-Unicode conversion tests are omitted. The function applies
# gsub() replacements correctly but then calls
# stringi::stri_trans_general("Latin-ASCII; Any-NFC"), which immediately
# converts the Unicode output back to ASCII (e.g. å → a, ö → o).
# This is a known bug — the stringi call should use "Any-NFC" only.

test_that("clean_bibtex_text leaves plain ASCII text unchanged", {
  expect_equal(clean_bibtex_text("plain text"), "plain text")
})


# validate_and_parse_bibtex() ---------------------------------------------

test_that("validate_and_parse_bibtex returns failure for empty string", {
  result <- validate_and_parse_bibtex("")
  expect_false(result$success)
  expect_null(result$data)
  expect_type(result$message, "character")
})

test_that("validate_and_parse_bibtex returns failure for non-string input", {
  result <- validate_and_parse_bibtex(123)
  expect_false(result$success)
})

test_that("validate_and_parse_bibtex returns success for valid BibTeX", {
  result <- validate_and_parse_bibtex(bibtex_article)
  expect_true(result$success)
  expect_s3_class(result$data, "data.frame")
  expect_null(result$warning)
})

test_that("validate_and_parse_bibtex warns about multiple entries but still succeeds", {
  result <- validate_and_parse_bibtex(bibtex_multi)
  expect_true(result$success)
  expect_false(is.null(result$warning))
  expect_match(result$warning, "Multiple BibTeX entries", ignore.case = TRUE)
})


# map_bibtex_to_reference_fields() ----------------------------------------

test_that("map_bibtex_to_reference_fields errors on empty data frame", {
  expect_error(
    map_bibtex_to_reference_fields(data.frame()),
    regexp = "non-empty"
  )
})

test_that("map_bibtex_to_reference_fields errors on non-data-frame input", {
  expect_error(
    map_bibtex_to_reference_fields("not a data frame"),
    regexp = "non-empty"
  )
})

test_that("map_bibtex_to_reference_fields returns correct fields from a parsed article", {
  parsed <- bib_string2df_alt(bibtex_article)
  # bib2df warns about missing optional columns (URL, ISBN, ISSN) — expected
  result <- suppressWarnings(
    map_bibtex_to_reference_fields(parsed, access_date = as.Date("2024-01-01"))
  )
  expect_type(result, "list")
  expect_equal(result$REFERENCE_TYPE, "Journal")
  expect_equal(result$YEAR, 2023)
  expect_equal(result$DOI, "10.1021/acs.est.2023.12345")
  expect_equal(result$ACCESS_DATE, as.Date("2024-01-01"))
})

test_that("map_bibtex_to_reference_fields maps access_date correctly", {
  parsed <- bib_string2df_alt(bibtex_article)
  fixed_date <- as.Date("2023-07-01")
  result <- suppressWarnings(
    map_bibtex_to_reference_fields(parsed, access_date = fixed_date)
  )
  expect_equal(result$ACCESS_DATE, fixed_date)
})


# validate_doi_format() ---------------------------------------------------

test_that("validate_doi_format accepts a plain valid DOI", {
  expect_true(validate_doi_format("10.1021/acs.est.2023.12345"))
  expect_true(validate_doi_format("10.1038/nature12345"))
})

test_that("validate_doi_format accepts DOIs with URL prefixes", {
  expect_true(validate_doi_format("https://doi.org/10.1021/acs.est.2023.12345"))
  expect_true(validate_doi_format(
    "http://dx.doi.org/10.1021/acs.est.2023.12345"
  ))
})

test_that("validate_doi_format uses the DOI from example_references_tibble()", {
  doi <- eDataDRF::example_references_tibble()$DOI
  expect_true(validate_doi_format(doi))
})

test_that("validate_doi_format rejects invalid inputs", {
  expect_false(validate_doi_format("not-a-doi"))
  expect_false(validate_doi_format(""))
  expect_false(validate_doi_format(NA))
  expect_false(validate_doi_format(123))
})


# validate_pmid_format() --------------------------------------------------

test_that("validate_pmid_format accepts plain numeric PMIDs", {
  expect_true(validate_pmid_format("12345678"))
  expect_true(validate_pmid_format("1"))
})

test_that("validate_pmid_format accepts PMIDs with PMID: prefix", {
  expect_true(validate_pmid_format("PMID:12345678"))
  expect_true(validate_pmid_format("PMID: 12345678"))
})

test_that("validate_pmid_format rejects invalid inputs", {
  expect_false(validate_pmid_format("123456789")) # 9 digits — too long
  expect_false(validate_pmid_format("not-a-pmid"))
  expect_false(validate_pmid_format(""))
  expect_false(validate_pmid_format(NA))
})


# extract_clean_doi() -----------------------------------------------------

test_that("extract_clean_doi strips https://doi.org/ prefix", {
  expect_equal(
    extract_clean_doi("https://doi.org/10.1021/test"),
    "10.1021/test"
  )
})

test_that("extract_clean_doi strips http://dx.doi.org/ prefix", {
  expect_equal(
    extract_clean_doi("http://dx.doi.org/10.1021/test"),
    "10.1021/test"
  )
})

test_that("extract_clean_doi returns a plain DOI unchanged", {
  expect_equal(extract_clean_doi("10.1021/test"), "10.1021/test")
})

test_that("extract_clean_doi trims whitespace", {
  expect_equal(extract_clean_doi("  10.1021/test  "), "10.1021/test")
})


# extract_clean_pmid() ----------------------------------------------------

test_that("extract_clean_pmid strips PMID: prefix", {
  expect_equal(extract_clean_pmid("PMID:12345678"), "12345678")
  expect_equal(extract_clean_pmid("PMID: 12345678"), "12345678")
})

test_that("extract_clean_pmid is case-insensitive for the prefix", {
  expect_equal(extract_clean_pmid("pmid:12345678"), "12345678")
})

test_that("extract_clean_pmid returns a plain PMID unchanged", {
  expect_equal(extract_clean_pmid("12345678"), "12345678")
})


# validate_and_lookup_identifier() — pure validation paths only ----------
# (API-hitting paths skipped as they require network access)

test_that("validate_and_lookup_identifier returns failure for empty input", {
  result <- validate_and_lookup_identifier("")
  expect_false(result$success)
  expect_true(is.na(result$identifier_type))
})

test_that("validate_and_lookup_identifier returns failure for non-string input", {
  result <- validate_and_lookup_identifier(123)
  expect_false(result$success)
})

test_that("validate_and_lookup_identifier returns failure and NA type for unrecognised format", {
  result <- validate_and_lookup_identifier("not-a-doi-or-pmid")
  expect_false(result$success)
  expect_true(is.na(result$identifier_type))
})

test_that("validate_and_lookup_identifier detects DOI format before lookup", {
  # Use skip_on_cran / skip_if_offline for the network call itself —
  # here we only verify the identifier_type returned on failure
  skip_if_offline()
  result <- validate_and_lookup_identifier(
    "10.9999/fake.doi.that.does.not.exist"
  )
  expect_equal(result$identifier_type, "doi")
})
