# Detect dataset type from filename

Determine which type of dataset based on filename patterns. Dataset
types correspond to eDataDRF tables — see
[`eDataDRF::initialise_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_sites_tibble.html),
[`eDataDRF::initialise_measurements_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_measurements_tibble.html),
etc. for the full set.

## Usage

``` r
detect_dataset_type(filename)
```

## Arguments

- filename:

  Name of the file

## Value

Character string of dataset type or NULL if not recognized

## See also

[`eDataDRF::initialise_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_sites_tibble.html),
[`eDataDRF::initialise_measurements_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_measurements_tibble.html),
[`eDataDRF::initialise_campaign_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_campaign_tibble.html)

## Examples

``` r
detect_dataset_type("Campaign_Sites_20241024.csv")
#> [1] "Sites"
detect_dataset_type("Measurements_export.csv")
#> [1] "Measurements"
detect_dataset_type("unknown_file.csv")
#> NULL
```
