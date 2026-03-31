# Summarise compartments data

Creates a summary string of compartments from session data

## Usage

``` r
summarise_compartments(compartmentsData)
```

## Arguments

- compartmentsData:

  The compartments dataset

## Value

Character string summarising compartments, or "Relevant data not found"

## See also

Other summarise:
[`calculate_coordinate_precision()`](https://nivanorge.github.io/STOPeData/reference/calculate_coordinate_precision.md),
[`manual_completion_message()`](https://nivanorge.github.io/STOPeData/reference/manual_completion_message.md),
[`summarise_CREED_details()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_details.md),
[`summarise_CREED_relevance()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_relevance.md),
[`summarise_CREED_reliability()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_reliability.md),
[`summarise_biota()`](https://nivanorge.github.io/STOPeData/reference/summarise_biota.md),
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
comps <- data.frame(
  ENVIRON_COMPARTMENT = c("Water", "Sediment"),
  ENVIRON_COMPARTMENT_SUB = c("Surface water", NA)
)
summarise_compartments(comps)
#> 2 compartments: Water (Surface water); Sediment
```
