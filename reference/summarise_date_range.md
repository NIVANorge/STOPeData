# Calculate Date Range

Creates a date range string from vector of dates with count and
interval. Format is:
`"YYYY-MM-DD to YYYY-MM-DD (n = ..., ... days (to nearest day))"`

## Usage

``` r
summarise_date_range(dates)
```

## Arguments

- dates:

  Vector of dates

## Value

Character string with date range, sample count, and interval in days

## See also

Other summarise:
[`calculate_coordinate_precision()`](https://nivanorge.github.io/STOPeData/reference/calculate_coordinate_precision.md),
[`manual_completion_message()`](https://nivanorge.github.io/STOPeData/reference/manual_completion_message.md),
[`summarise_CREED_details()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_details.md),
[`summarise_CREED_relevance()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_relevance.md),
[`summarise_CREED_reliability()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_reliability.md),
[`summarise_biota()`](https://nivanorge.github.io/STOPeData/reference/summarise_biota.md),
[`summarise_compartments()`](https://nivanorge.github.io/STOPeData/reference/summarise_compartments.md),
[`summarise_lod_loq()`](https://nivanorge.github.io/STOPeData/reference/summarise_lod_loq.md),
[`summarise_measured_units()`](https://nivanorge.github.io/STOPeData/reference/summarise_measured_units.md),
[`summarise_multiple()`](https://nivanorge.github.io/STOPeData/reference/summarise_multiple.md),
[`summarise_protocols()`](https://nivanorge.github.io/STOPeData/reference/summarise_protocols.md),
[`summarise_reference()`](https://nivanorge.github.io/STOPeData/reference/summarise_reference.md),
[`summarise_sig_figs()`](https://nivanorge.github.io/STOPeData/reference/summarise_sig_figs.md),
[`summarise_sites()`](https://nivanorge.github.io/STOPeData/reference/summarise_sites.md),
[`summarise_uncertainty_comments()`](https://nivanorge.github.io/STOPeData/reference/summarise_uncertainty_comments.md)

## Examples

``` r
summarise_date_range(as.Date(c("2020-01-15", "2021-06-01", "2022-03-10")))
#> 2020-01-15 to 2022-03-10 (n=3, 785 days (to nearest day))
summarise_date_range(as.Date(c("2021-07-01", "2021-07-01")))
#> 2021-07-01 (n=1)
```
