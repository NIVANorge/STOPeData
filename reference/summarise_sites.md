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
