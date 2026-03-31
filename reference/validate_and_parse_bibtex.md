# Validate and parse BibTeX string with error handling

Wrapper function that validates BibTeX syntax, parses the string using
bib_string2df_alt, and provides detailed error handling with
user-friendly error messages.

## Usage

``` r
validate_and_parse_bibtex(bibtex_string, allow_multiple = FALSE)
```

## Arguments

- bibtex_string:

  Character string containing BibTeX formatted data

- allow_multiple:

  Logical indicating whether to allow multiple entries. If FALSE
  (default), will return an error for multiple entries.

## Value

A list with components:

- success: Logical indicating whether parsing succeeded

- data: Parsed data frame (if success = TRUE) or NULL

- message: Success/error message for user feedback

- warning: Additional warning message (if applicable)

## See also

Other validate:
[`validate_and_lookup_identifier()`](https://nivanorge.github.io/STOPeData/reference/validate_and_lookup_identifier.md),
[`validate_dataset_structure()`](https://nivanorge.github.io/STOPeData/reference/validate_dataset_structure.md),
[`validate_doi_format()`](https://nivanorge.github.io/STOPeData/reference/validate_doi_format.md),
[`validate_pmid_format()`](https://nivanorge.github.io/STOPeData/reference/validate_pmid_format.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  bibtex_str <- "@article{smith2022,
    author  = {Smith, Jane},
    title   = {Marine pollution},
    journal = {Marine Pollution Bulletin},
    year    = {2022}
  }"
  result <- validate_and_parse_bibtex(bibtex_str)
  result$success
  result$data$TITLE
} # }
```
