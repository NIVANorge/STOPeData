# CREED Reliability Criteria Configuration

Returns the configuration list for all 19 CREED reliability criteria
(RB1-RB19). Each criterion includes its title and whether it is Required
or Recommended.

## Usage

``` r
CREED_reliability_criteria_config()
```

## Value

A named list where each element is a criterion configuration with:

- title:

  Character. Human-readable criterion title.

- type:

  Character. Either "Required" or "Recommended".

## Details

Reliability criteria assess the methodological quality of the data: -
RB1-RB3: Media (sample medium, collection method, handling) - RB4:
Spatial (site location) - RB5: Temporal (date and time) - RB6-RB13:
Analytical (analytes, LOD/LOQ, QA/QC, methods) - RB14-RB18: Data
handling (calculations, significant figures, outliers, censored data) -
RB19: Supporting parameters

RB8 (Accreditation/QMS) is a shortcut criterion: if fully met, RB9-RB12
may be skipped.

## See also

\[CREED_relevance_criteria_config()\], \[collect_CREED_data()\]
