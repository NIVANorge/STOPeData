# Detect dataset type from filename —-

Determine which type of dataset based on filename patterns

## Usage

``` r
detect_dataset_type(filename)
```

## Arguments

- filename:

  Name of the file

## Value

Character string of dataset type or NULL if not recognized

## Examples

``` r
detect_dataset_type("Campaign_Sites_20241024.csv")
#> [1] "Sites"
detect_dataset_type("Measurements_export.csv")
#> [1] "Measurements"
detect_dataset_type("unknown_file.csv")
#> NULL
```
