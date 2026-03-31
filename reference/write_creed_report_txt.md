# Write CREED report as human-readable text file

Merge CREED details, relevance, and reliability tibbles into a single
human-readable text file with section headers

## Usage

``` r
write_creed_report_txt(
  creed_details,
  creed_relevance,
  creed_reliability,
  file_path
)
```

## Arguments

- creed_details:

  Tibble with field/value columns from summarise_CREED_details()

- creed_relevance:

  Tibble with field/value columns from summarise_CREED_relevance(). Use
  [`eDataDRF::example_CREED_relevance_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_relevance_tibble.html)
  to generate example data.

- creed_reliability:

  Tibble with field/value columns from summarise_CREED_reliability().
  Use
  [`eDataDRF::example_CREED_reliability_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_reliability_tibble.html)
  to generate example data.

- file_path:

  Character. Path where to write the report file

## Value

NULL (invisibly). File is written to disk as a side effect.

## See also

[`eDataDRF::example_CREED_relevance_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_relevance_tibble.html),
[`eDataDRF::example_CREED_reliability_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_reliability_tibble.html),
[`eDataDRF::initialise_CREED_data_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_CREED_data_tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
  # creed_* tibbles are produced by the summarise_CREED_* functions
  write_creed_report_txt(creed_details, creed_relevance, creed_reliability,
                         file_path = tempfile(fileext = ".txt"))
} # }
```
