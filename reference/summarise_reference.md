# Summarise mod_reference data into a single string

Creates a formatted bibliographic reference from reference data. See
[`eDataDRF::initialise_references_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_references_tibble.html)
for the expected column structure.

## Usage

``` r
summarise_reference(ref_data)
```

## Arguments

- ref_data:

  Reference data frame. Use
  [`eDataDRF::example_references_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_references_tibble.html)
  to generate an example input.

## Value

Character string with formatted reference

## See also

[`eDataDRF::example_references_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_references_tibble.html),
[`eDataDRF::initialise_references_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_references_tibble.html),
[`eDataDRF::generate_reference_id()`](https://NIVANorge.github.io/eDataDRF/reference/generate_reference_id.html)

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
[`summarise_sig_figs()`](https://nivanorge.github.io/STOPeData/reference/summarise_sig_figs.md),
[`summarise_sites()`](https://nivanorge.github.io/STOPeData/reference/summarise_sites.md),
[`summarise_uncertainty_comments()`](https://nivanorge.github.io/STOPeData/reference/summarise_uncertainty_comments.md)

## Examples

``` r
summarise_reference(eDataDRF::example_references_tibble())
#> [1] "Smith, J.; Jones, A.; Williams, B.. (2023). Heavy metal contamination in Norwegian coastal sediments. Environmental Science & Technology. (10.1021/acs.est.2023.12345)"
```
