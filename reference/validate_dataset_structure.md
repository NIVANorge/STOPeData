# Validate dataset structure

Basic validation of dataset structure

## Usage

``` r
validate_dataset_structure(data, dataset_type)
```

## Arguments

- data:

  Tibble/data.frame to validate. Use
  [`eDataDRF::example_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_sites_tibble.html)
  or other `example_*_tibble()` functions to generate valid example
  inputs.

- dataset_type:

  Character string of dataset type, as returned by
  [`detect_dataset_type()`](https://nivanorge.github.io/STOPeData/reference/detect_dataset_type.md)

## Value

List with valid (logical) and message (character)

## See also

[`detect_dataset_type()`](https://nivanorge.github.io/STOPeData/reference/detect_dataset_type.md),
[`eDataDRF::initialise_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_sites_tibble.html)

Other validate:
[`validate_and_lookup_identifier()`](https://nivanorge.github.io/STOPeData/reference/validate_and_lookup_identifier.md),
[`validate_and_parse_bibtex()`](https://nivanorge.github.io/STOPeData/reference/validate_and_parse_bibtex.md),
[`validate_doi_format()`](https://nivanorge.github.io/STOPeData/reference/validate_doi_format.md),
[`validate_pmid_format()`](https://nivanorge.github.io/STOPeData/reference/validate_pmid_format.md)

## Examples

``` r
validate_dataset_structure(eDataDRF::example_sites_tibble(), "Sites")
#> $valid
#> [1] TRUE
#> 
#> $message
#> [1] "Valid structure"
#> 
validate_dataset_structure(eDataDRF::example_measurements_tibble(), "Measurements")
#> $valid
#> [1] TRUE
#> 
#> $message
#> [1] "Valid structure"
#> 
validate_dataset_structure(data.frame(), "Sites")
#> $valid
#> [1] FALSE
#> 
#> $message
#> [1] "Data has no columns"
#> 
```
