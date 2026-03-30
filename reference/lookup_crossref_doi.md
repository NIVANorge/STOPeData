# Lookup publication data using Crossref

Queries Crossref API using rcrossref::cr_works() to retrieve publication
metadata for a given DOI.

## Usage

``` r
lookup_crossref_doi(doi)
```

## Arguments

- doi:

  Character string containing DOI

## Value

List with components: - success: Logical indicating whether lookup
succeeded - data: Crossref data (if success = TRUE) or NULL - message:
Status message for user feedback

## Details

Uses rcrossref::cr_works() to query the Crossref API. Requires internet
connection. May fail if Crossref is unavailable or if the DOI is not
registered with Crossref.
