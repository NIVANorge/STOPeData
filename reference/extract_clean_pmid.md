# Extract clean PMID from input string

Extracts a clean PMID string by removing PMID: prefixes and whitespace.

## Usage

``` r
extract_clean_pmid(input_string)
```

## Arguments

- input_string:

  Character string containing PMID

## Value

Clean PMID string or the original input if no cleaning needed

## Examples

``` r
extract_clean_pmid("PMID: 12345678")
#> [1] "12345678"
extract_clean_pmid("9876543")
#> [1] "9876543"
```
