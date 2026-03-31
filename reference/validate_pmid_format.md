# Validate PMID format

Validates whether a string contains a valid PubMed ID (PMID) format.
PMIDs are numeric identifiers, typically 1-8 digits.

## Usage

``` r
validate_pmid_format(input_string)
```

## Arguments

- input_string:

  Character string to validate as PMID

## Value

Logical indicating whether the input is a valid PMID format

## See also

Other validate:
[`validate_and_lookup_identifier()`](https://nivanorge.github.io/STOPeData/reference/validate_and_lookup_identifier.md),
[`validate_and_parse_bibtex()`](https://nivanorge.github.io/STOPeData/reference/validate_and_parse_bibtex.md),
[`validate_dataset_structure()`](https://nivanorge.github.io/STOPeData/reference/validate_dataset_structure.md),
[`validate_doi_format()`](https://nivanorge.github.io/STOPeData/reference/validate_doi_format.md)

## Examples

``` r
validate_pmid_format("12345678")
#> [1] TRUE
validate_pmid_format("PMID: 9876543")
#> [1] TRUE
validate_pmid_format("not-a-pmid")
#> [1] FALSE
```
