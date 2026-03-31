# Clean BibTeX text formatting

Removes common BibTeX formatting artifacts including double curly braces
used for capitalization preservation and LaTeX accent commands for
non-ASCII characters.

## Usage

``` r
clean_bibtex_text(text)
```

## Arguments

- text:

  Character vector to clean

## Value

Character vector with cleaned text

## Details

This function performs the following cleaning operations: - Removes
double curly braces used for capitalization preservation - Converts
common LaTeX accent commands to Unicode characters - Normalises
whitespace

## Examples

``` r
clean_bibtex_text("{{Marine}} pollution")
#> [1] "Marine pollution"
clean_bibtex_text("M\\u00FCller")
#> [1] "M\\u00FCller"
```
