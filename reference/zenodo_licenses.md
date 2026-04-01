# Zenodo license options tibble

A tibble of Zenodo license options as of March 2026. Includes `id`,
`title`, `description`, `url`, `schema`, `osi_approved`, `revision_id`,
`created`, `updated`, and `popular`, a custom boolean highlighting the
most commonly used licenses.

## Usage

``` r
zenodo_licenses
```

## Format

A tibble with 444 rows and 10 columns:

- id:

  Character. Zenodo license identifier, e.g. `"cc-by-4.0"`.

- title:

  Character. Human-readable license name.

- description:

  Character. License description.

- url:

  Character. URL to the full license text.

- schema:

  Character. License schema identifier.

- osi_approved:

  Logical. Whether the license is OSI-approved.

- revision_id:

  Integer. Zenodo internal revision identifier.

- created:

  Character. ISO 8601 creation timestamp.

- updated:

  Character. ISO 8601 last-updated timestamp.

- popular:

  Logical. Custom flag for licenses commonly used in scientific
  datasets.

## Source

Zenodo REST API: `GET https://zenodo.org/api/licenses?size=1000`
Retrieved March 2026. See `data-raw/zenodo_licenses.R` for details.

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
