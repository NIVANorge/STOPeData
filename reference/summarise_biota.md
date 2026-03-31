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

  The biota dataset

- SPECIES_GROUP:

  Logical. Include species group summary?

- SAMPLE_SPECIES:

  Logical. Include sample species summary?

- SAMPLE_TISSUE:

  Logical. Include tissue type summary?

- SAMPLE_SPECIES_LIFESTAGE:

  Logical. Include life stage summary?

- SAMPLE_SPECIES_GENDER:

  Logical. Include gender summary?

## Value

Character string summarising biota, or "Relevant data not found"

## See also

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
biota <- data.frame(
  SPECIES_GROUP = c("Fish", "Fish"),
  SAMPLE_SPECIES = c("Gadus morhua", "Clupea harengus"),
  SAMPLE_TISSUE = c("Liver", "Muscle"),
  SAMPLE_SPECIES_LIFESTAGE = c("Adult", "Juvenile"),
  SAMPLE_SPECIES_GENDER = c("Male", "Female")
)
summarise_biota(biota, SAMPLE_SPECIES = TRUE, SAMPLE_TISSUE = TRUE)
#> [1] "2 biota samples. Species (2): Gadus morhua, Clupea harengus. Tissue types (2): Liver, Muscle"
```
