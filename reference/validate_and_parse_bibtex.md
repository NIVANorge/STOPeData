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

A list with components: - success: Logical indicating whether parsing
succeeded - data: Parsed data frame (if success = TRUE) or NULL -
message: Success/error message for user feedback - warning: Additional
warning message (if applicable)
