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

## Examples

``` r
config <- CREED_reliability_criteria_config()
names(config)
#>  [1] "RB1"  "RB2"  "RB3"  "RB4"  "RB5"  "RB6"  "RB7"  "RB8"  "RB9"  "RB10"
#> [11] "RB11" "RB12" "RB13" "RB14" "RB15" "RB16" "RB17" "RB18" "RB19"
config$RB1
#> $title
#> [1] "Sample Medium/Matrix"
#> 
#> $type
#> [1] "Required"
#> 
```
