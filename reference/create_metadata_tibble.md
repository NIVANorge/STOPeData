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
#> # A tibble: 9 × 2
#>   Property                 Value  
#>   <chr>                    <chr>  
#> 1 Session: App             NA     
#> 2 Session: Version         NA     
#> 3 Session: Format Version  NA     
#> 4 Session: Start           NA     
#> 5 Session: User            Unknown
#> 6 Export: Campaign         NA     
#> 7 Export: Datetime         NA     
#> 8 Export: Session Duration NA     
#> 9 Extractions (YAML)       None   
```
