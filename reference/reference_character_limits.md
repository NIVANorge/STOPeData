# Reference field character limits

Returns maximum character lengths for text fields in the references
table
([`eDataDRF::initialise_references_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_references_tibble.html)).

## Usage

``` r
reference_character_limits()
```

## Value

A named list of character limits for reference fields

## Details

Provides a named list of character limits for reference metadata fields
to ensure data quality and database compatibility. Limits include:

DOCUMENT_NUMBER: 200 characters

DOI: 200 characters

EDITION: 200 characters

INSTITUTION: 200 characters

ISBN_ISSN: 200 characters

PERIODICAL_JOURNAL: 200 characters

PUBLISHER: 200 characters

REF_COMMENT: 1000 characters

URL: 200 characters

## See also

[`eDataDRF::initialise_references_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_references_tibble.html),
[`eDataDRF::example_references_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_references_tibble.html),
[`eDataDRF::reference_type_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/reference_type_vocabulary.html),
[`eDataDRF::data_source_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/data_source_vocabulary.html)

## Examples

``` r
limits <- reference_character_limits()
limits$DOI
#> [1] 200
limits$REF_COMMENT
#> [1] 1000
```
