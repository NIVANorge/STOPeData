# Collect CREED Scores from Input

Collects CREED criterion scores, relevant data, and limitations from
Shiny inputs into a standardised tibble. Works for both reliability and
relevance criteria.

## Usage

``` r
collect_CREED_data(criteria_config, input)
```

## Arguments

- criteria_config:

  the CREED criteria for either reliability or relevance

- input:

  the shiny input object for the module

## Examples

``` r
if (FALSE) { # \dontrun{
  # Used inside a Shiny module server function
  config <- CREED_reliability_criteria_config()
  scores <- collect_CREED_data(config, input)
  scores
} # }
```
