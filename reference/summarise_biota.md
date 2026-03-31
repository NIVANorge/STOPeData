# Summarise biota data

Creates a summary string of biota including species, tissues, and life
stages

## Usage

``` r
summarise_biota(
  biotaData,
  SPECIES_GROUP = FALSE,
  SAMPLE_SPECIES = FALSE,
  SAMPLE_TISSUE = FALSE,
  SAMPLE_SPECIES_LIFESTAGE = FALSE,
  SAMPLE_SPECIES_GENDER = FALSE
)
```

## Arguments

- biotaData:

  The biota dataset. Use
  [`eDataDRF::example_biota_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_biota_tibble.html)
  to generate an example input.

- SPECIES_GROUP:

  Logical. Include species group summary? See
  [`eDataDRF::species_groups_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/species_groups_vocabulary.html)
  for valid groups.

- SAMPLE_SPECIES:

  Logical. Include sample species summary? See
  [`eDataDRF::species_names_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/species_names_vocabulary.html)
  for valid species.

- SAMPLE_TISSUE:

  Logical. Include tissue type summary? See
  [`eDataDRF::tissue_types_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/tissue_types_vocabulary.html)
  for valid tissue types.

- SAMPLE_SPECIES_LIFESTAGE:

  Logical. Include life stage summary? See
  [`eDataDRF::lifestage_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/lifestage_vocabulary.html)
  for valid life stages.

- SAMPLE_SPECIES_GENDER:

  Logical. Include gender summary? See
  [`eDataDRF::gender_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/gender_vocabulary.html)
  for valid gender values.

## Value

Character string summarising biota, or "Relevant data not found"

## See also

[`eDataDRF::example_biota_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_biota_tibble.html),
[`eDataDRF::species_groups_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/species_groups_vocabulary.html),
[`eDataDRF::species_names_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/species_names_vocabulary.html),
[`eDataDRF::tissue_types_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/tissue_types_vocabulary.html),
[`eDataDRF::lifestage_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/lifestage_vocabulary.html),
[`eDataDRF::gender_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/gender_vocabulary.html)

Other summarise:
[`calculate_coordinate_precision()`](https://nivanorge.github.io/STOPeData/reference/calculate_coordinate_precision.md),
[`manual_completion_message()`](https://nivanorge.github.io/STOPeData/reference/manual_completion_message.md),
[`summarise_CREED_details()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_details.md),
[`summarise_CREED_relevance()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_relevance.md),
[`summarise_CREED_reliability()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_reliability.md),
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
summarise_biota(eDataDRF::example_biota_tibble(), SAMPLE_SPECIES = TRUE, SAMPLE_TISSUE = TRUE)
#> [1] "1 biota samples. Species: Gadus morhua. Tissue types: Liver"
```
