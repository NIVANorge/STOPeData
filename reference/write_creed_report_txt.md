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

  Tibble with field/value columns from summarise_CREED_relevance()

- creed_reliability:

  Tibble with field/value columns from summarise_CREED_reliability()

- file_path:

  Character. Path where to write the report file

## Value

NULL (invisibly). File is written to disk as a side effect.
