# Get dataset display name

Convert internal dataset names to user-friendly display names

## Usage

``` r
get_dataset_display_name(dataset_name)
```

## Arguments

- dataset_name:

  Character. Internal name of the dataset (e.g., "sitesData")

## Value

Character. User-friendly display name (e.g., "Sites")

## Examples

``` r
get_dataset_display_name("sitesData")
#> [1] "Sites"
get_dataset_display_name("measurementsData")
#> [1] "Measurements"
get_dataset_display_name("creedReliability")
#> [1] "CREED_RB"
```
