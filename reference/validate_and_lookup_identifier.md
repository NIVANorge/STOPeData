# Validate and lookup DOI/PMID with comprehensive error handling

Main function that validates input format, converts PMID to DOI if
needed, looks up publication data from Crossref, and returns formatted
results.

## Usage

``` r
validate_and_lookup_identifier(input_string, access_date = Sys.Date())
```

## Arguments

- input_string:

  Character string containing DOI or PMID

- access_date:

  Date to use for ACCESS_DATE field. Defaults to today's date.

## Value

List with components: - success: Logical indicating whether lookup
succeeded - data: Mapped field data (if success = TRUE) or NULL -
message: Status message for user feedback - identifier_type: Type of
identifier detected ("doi" or "pmid")

## Details

This is the main entry point for DOI/PMID lookup functionality. It
handles the complete workflow: 1. Validate input format 2. Convert PMID
to DOI if necessary 3. Query Crossref for publication data 4. Map
results to reference field format
