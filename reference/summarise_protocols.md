# Summarise protocols data

Creates a summary string of protocols by category

## Usage

``` r
summarise_protocols(methodsData, categories)
```

## Arguments

- methodsData:

  The methods/protocols dataset. Use
  [`eDataDRF::example_methods_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_methods_tibble.html)
  to generate an example input.

- categories:

  Character vector of protocol categories to include. Must be values
  from
  [`eDataDRF::protocol_categories_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/protocol_categories_vocabulary.html)
  (e.g., `c("Sampling Protocol", "Analytical Protocol")`).

## Value

Character string summarising protocols, or "Relevant data not found"

## See also

[`eDataDRF::example_methods_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_methods_tibble.html),
[`eDataDRF::protocol_categories_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/protocol_categories_vocabulary.html),
[`eDataDRF::protocol_options_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/protocol_options_vocabulary.html)

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
[`summarise_reference()`](https://nivanorge.github.io/STOPeData/reference/summarise_reference.md),
[`summarise_sig_figs()`](https://nivanorge.github.io/STOPeData/reference/summarise_sig_figs.md),
[`summarise_sites()`](https://nivanorge.github.io/STOPeData/reference/summarise_sites.md),
[`summarise_uncertainty_comments()`](https://nivanorge.github.io/STOPeData/reference/summarise_uncertainty_comments.md)

## Examples

``` r
summarise_protocols(
  eDataDRF::example_methods_tibble(),
  categories = c("Sampling Protocol", "Analytical Protocol")
)
#> 2 protocols:
#> Sampling Protocol - Grab sampling: (Surface sediment grab samples collected by Van Veen grab)
#> Analytical Protocol - Inductively coupled plasma mass spectrometry: (Inductively coupled plasma mass spectrometry using the ICP-MASTER 9000.)
```
