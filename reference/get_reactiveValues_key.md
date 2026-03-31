# Get reactiveValues key for dataset type —-

Map dataset type to reactiveValues key

## Usage

``` r
get_reactiveValues_key(dataset_type)
```

## Arguments

- dataset_type:

  Character string of dataset type

## Value

Character string of reactiveValues key or NULL

## Examples

``` r
get_reactiveValues_key("Sites")
#> [1] "sitesData"
get_reactiveValues_key("CREED_RB")
#> [1] "creedReliability"
```
