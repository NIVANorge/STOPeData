# Convert BibTeX string to data frame using temporary file

This function converts a BibTeX formatted string into a data frame by
writing the string to a temporary file and then using
[`bib2df::bib2df`](https://docs.ropensci.org/bib2df/reference/bib2df.html)
to parse it. The temporary file is automatically cleaned up after use.

## Usage

``` r
bib_string2df_alt(string, ...)
```

## Arguments

- string:

  A BibTeX formatted string containing one or more entries

- ...:

  Additional arguments passed to
  [`bib2df`](https://docs.ropensci.org/bib2df/reference/bib2df.html)

## Value

A tibble/data frame with parsed BibTeX entries

## Details

This approach uses temporary files to work around the limitation that
[`bib2df::bib2df`](https://docs.ropensci.org/bib2df/reference/bib2df.html)
only accepts file paths, not strings directly. The temporary file is
created in the system temp directory and removed automatically, even if
an error occurs.

## Author

Philipp Ottolinger (original function), Sam Welch (added string wrapper)

## Examples

``` r
if (FALSE) { # \dontrun{
  bibtex_str <- "@article{smith2022,
    author  = {Smith, Jane},
    title   = {Marine pollution},
    journal = {Marine Pollution Bulletin},
    year    = {2022}
  }"
  df <- bib_string2df_alt(bibtex_str)
  df$TITLE
} # }
```
