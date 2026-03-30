# Map Crossref data to reference input fields

Converts Crossref API response data into a list of values that
correspond to the reference module input fields.

## Usage

``` r
map_crossref_to_reference_fields(crossref_data, access_date = Sys.Date())
```

## Arguments

- crossref_data:

  Data frame returned from rcrossref::cr_works()

- access_date:

  Date to use for ACCESS_DATE field. Defaults to today's date.

## Value

A named list containing mapped field values for all reference input
fields. Values are NA for fields not present in the Crossref data.

## Details

Maps Crossref fields to reference fields: - type → REFERENCE_TYPE
(journal-article → journal, book → book, etc.) - title → TITLE - author
→ AUTHOR (formatted as Last, First; Last, First) -
published.online/published.print → YEAR - container.title →
PERIODICAL_JOURNAL - volume → VOLUME - issue → ISSUE - publisher →
PUBLISHER - DOI → DOI - URL → URL - page → PAGES - ISBN/ISSN → ISBN_ISSN
