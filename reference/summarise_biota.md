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
