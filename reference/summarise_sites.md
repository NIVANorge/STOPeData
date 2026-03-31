# Summarise sites data

Creates a summary string of sites including countries and areas

## Usage

``` r
summarise_sites(
  sitesData,
  COUNTRY_ISO = FALSE,
  OCEAN_IHO = FALSE,
  SITE_GEOGRAPHIC_FEATURE = FALSE,
  SITE_GEOGRAPHIC_FEATURE_SUB = FALSE,
  PRECISION = FALSE
)
```

## Arguments

- sitesData:

  The sites dataset

- COUNTRY_ISO:

  Logical. Include country summary?

- OCEAN_IHO:

  Logical. Include area summary?

- SITE_GEOGRAPHIC_FEATURE:

  Logical. Include geographic feature summary?

- SITE_GEOGRAPHIC_FEATURE_SUB:

  Logical. Include geographic feature sub summary?

- PRECISION:

  Logical. Include coordinate precision?

## Value

Character string summarising sites, or "Relevant data not found"

## See also

Other summarise:
[`calculate_coordinate_precision()`](https://nivanorge.github.io/STOPeData/reference/calculate_coordinate_precision.md),
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
[`summarise_uncertainty_comments()`](https://nivanorge.github.io/STOPeData/reference/summarise_uncertainty_comments.md)

## Examples

``` r
sites <- data.frame(
  COUNTRY_ISO = c("NO", "SE"),
  OCEAN_IHO = c("North Sea", "Baltic Sea"),
  SITE_GEOGRAPHIC_FEATURE = c("Fjord", "Bay"),
  LATITUDE = c(59.123, 57.456),
  LONGITUDE = c(10.12, 11.789)
)
summarise_sites(sites, COUNTRY_ISO = TRUE, PRECISION = TRUE)
#> [1] "2 sites. Countries (2): NO, SE. Lowest coordinate precision: 2"
```
