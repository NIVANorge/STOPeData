# Convert PMID to DOI using PubMed API

Attempts to convert a PubMed ID to a DOI by querying the PubMed API.
This is necessary because Crossref doesn't directly support PMID lookup.

## Usage

``` r
pmid_to_doi(pmid)
```

## Arguments

- pmid:

  Character string containing PMID

## Value

List with components:

- success: Logical indicating whether conversion succeeded

- doi: DOI string (if success = TRUE) or NULL

- message: Status message for user feedback

## Details

Uses the PubMed E-utilities API to fetch article information and extract
DOI. Requires internet connection. May fail if PubMed is unavailable or
if the article doesn't have a DOI registered.

## Examples

``` r
if (FALSE) { # \dontrun{
  result <- pmid_to_doi("12345678")
  result$doi
} # }
```
