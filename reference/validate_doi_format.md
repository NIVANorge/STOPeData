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
