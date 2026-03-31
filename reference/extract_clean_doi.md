# Extract clean DOI from input string

Extracts a clean DOI string by removing URL prefixes and whitespace.

## Usage

``` r
extract_clean_doi(input_string)
```

## Arguments

- input_string:

  Character string containing DOI

## Value

Clean DOI string or the original input if no cleaning needed

## Examples

``` r
extract_clean_doi("https://doi.org/10.1016/j.marpolbul.2022.01.001")
#> [1] "10.1016/j.marpolbul.2022.01.001"
extract_clean_doi("10.1000/xyz123")
#> [1] "10.1000/xyz123"
```
