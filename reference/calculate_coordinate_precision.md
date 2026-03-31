# Calculate coordinate precision

Determines the minimum decimal places across latitude and longitude

## Usage

``` r
calculate_coordinate_precision(latitude, longitude)
```

## Arguments

- latitude:

  Numeric vector of latitudes

- longitude:

  Numeric vector of longitudes

## Value

Integer representing minimum decimal places, or "N/A" if no valid coords

## See also

Other summarise:
[`manual_completion_message()`](https://nivanorge.github.io/STOPeData/reference/manual_completion_message.md),
[`summarise_CREED_details()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_details.md),
[`summarise_CREED_relevance()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_relevance.md),
[`summarise_CREED_reliability()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_reliability.md),
[`summarise_biota()`](https://nivanorge.github.io/STOPeData/reference/summarise_biota.md),
[`summarise_compartments()`](https://nivanorge.github.io/STOPeData/reference/summarise_compartments.md),
[`summarise_date_range()`](https://nivanorge.github.io/STOPeData/reference/summarise_date_range.md),
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
calculate_coordinate_precision(c(59.123, 57.4), c(10.12, 11.789))
#> [1] 1
calculate_coordinate_precision(c(59.1234567, 57.456789), c(10.1, 11.12345))
#> [1] 1
```
