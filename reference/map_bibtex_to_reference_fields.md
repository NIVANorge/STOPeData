# Map BibTeX fields to reference input fields

Converts parsed BibTeX data from bib2df format into a list of values
that correspond to the reference module input fields. Handles field
mapping between BibTeX conventions and the reference data structure.

## Usage

``` r
map_bibtex_to_reference_fields(bibtex_df, access_date = Sys.Date())
```

## Arguments

- bibtex_df:

  A data frame from bib2df containing parsed BibTeX data. Should contain
  at least one row of BibTeX entry data.

- access_date:

  Date to use for ACCESS_DATE field. Defaults to today's date.

## Value

A named list containing mapped field values for all reference input
fields. Values are NA for fields not present in the BibTeX entry.
