# Validate dataset structure —-

Basic validation of dataset structure

## Usage

``` r
validate_dataset_structure(data, dataset_type)
```

## Arguments

- data:

  Tibble/data.frame to validate

- dataset_type:

  Character string of dataset type

## Value

List with valid (logical) and message (character)

## See also

Other validate:
[`validate_and_lookup_identifier()`](https://nivanorge.github.io/STOPeData/reference/validate_and_lookup_identifier.md),
[`validate_and_parse_bibtex()`](https://nivanorge.github.io/STOPeData/reference/validate_and_parse_bibtex.md),
[`validate_doi_format()`](https://nivanorge.github.io/STOPeData/reference/validate_doi_format.md),
[`validate_pmid_format()`](https://nivanorge.github.io/STOPeData/reference/validate_pmid_format.md)

## Examples

``` r
df <- data.frame(SITE_CODE = c("S001", "S002"), LATITUDE = c(59.1, 57.4))
validate_dataset_structure(df, "Sites")
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
