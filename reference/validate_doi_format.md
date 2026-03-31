# Validate DOI format

Validates whether a string contains a valid DOI format. Accepts DOIs
with or without URL prefixes (https://doi.org/ or http://dx.doi.org/).

## Usage

``` r
validate_doi_format(input_string)
```

## Arguments

- input_string:

  Character string to validate as DOI

## Value

Logical indicating whether the input is a valid DOI format

## Details

DOI format follows the pattern: 10.registrant/suffix Where registrant is
typically 4+ digits and suffix can contain various characters. This
function validates format only, not whether the DOI actually exists.

## See also

Other validate:
[`validate_and_lookup_identifier()`](https://nivanorge.github.io/STOPeData/reference/validate_and_lookup_identifier.md),
[`validate_and_parse_bibtex()`](https://nivanorge.github.io/STOPeData/reference/validate_and_parse_bibtex.md),
[`validate_dataset_structure()`](https://nivanorge.github.io/STOPeData/reference/validate_dataset_structure.md),
[`validate_pmid_format()`](https://nivanorge.github.io/STOPeData/reference/validate_pmid_format.md)

## Examples

``` r
validate_doi_format("10.1016/j.marpolbul.2022.01.001")
#> [1] TRUE
validate_doi_format("https://doi.org/10.1000/xyz123")
#> [1] TRUE
validate_doi_format("not-a-doi")
#> [1] FALSE
```
