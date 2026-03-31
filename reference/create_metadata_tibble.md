# Create metadata tibble

Convert metadata list to tibble format suitable for Excel sheets

## Usage

``` r
create_metadata_tibble(metadata_list)
```

## Arguments

- metadata_list:

  List containing metadata fields

## Value

Tibble with Property and Value columns

## Examples

``` r
create_metadata_tibble(list(campaign = "North Sea 2022", version = "1.0", user = "Jane"))
#> # A tibble: 3 × 2
#>   Property Value         
#>   <chr>    <chr>         
#> 1 campaign North Sea 2022
#> 2 version  1.0           
#> 3 user     Jane          
```
