# Zenodo license options tibble

Returns a tibble of Zenodo license options as of March 2026. Includes
`id`, `title`, `description`, `url`, `schema`, `osi_approved`,
`revision_id`, `created`, `updated`, and `popular`, a custom boolean
highlighting the most commonly used licenses.

## Usage

``` r
zenodo_licenses
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 444
rows and 10 columns.

## See also

Other zenodo:
[`generate_zenodo_readme()`](https://nivanorge.github.io/STOPeData/reference/generate_zenodo_readme.md),
[`zenodo_resource_types`](https://nivanorge.github.io/STOPeData/reference/zenodo_resource_types.md)

## Examples

``` r
# Show only popular licenses
zenodo_licenses[zenodo_licenses$popular, c("id", "title")]
#> # A tibble: 6 × 2
#>   id              title                                                         
#>   <chr>           <chr>                                                         
#> 1 apache-2.0      Apache License 2.0                                            
#> 2 cc-by-4.0       Creative Commons Attribution 4.0 International                
#> 3 cc-by-nc-4.0    Creative Commons Attribution Non Commercial 4.0 International 
#> 4 cc-by-nc-sa-4.0 Creative Commons Attribution Non Commercial Share Alike 4.0 I…
#> 5 cc-by-sa-4.0    Creative Commons Attribution Share Alike 4.0 International    
#> 6 mit             MIT License                                                   
```
