# Convert CREED Tibble to Mock Input List

Converts a CREED data tibble (from
[`eDataDRF::example_CREED_reliability_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_reliability_tibble.html)
or
[`eDataDRF::example_CREED_relevance_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_relevance_tibble.html))
into a mock shiny input list object suitable for testing
collect_CREED_data().

## Usage

``` r
creed_tibble_to_mock_input(creed_tibble)
```

## Arguments

- creed_tibble:

  A tibble with columns: criterion_id, relevant_data, score,
  limitations. Use
  [`eDataDRF::example_CREED_reliability_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_reliability_tibble.html)
  or
  [`eDataDRF::example_CREED_relevance_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_relevance_tibble.html)
  to generate example inputs.

## Value

A named list mimicking the structure of a Shiny input object, with
entries named `criterionId_score`, `criterionId_relevant_data`, and
`criterionId_limitations` (or `criterionId_justification` for RB8).

## Details

Score values are converted from numeric to text:

- 1 = "Fully Met" (or "Not Relevant")

- 2 = "Partly Met"

- 3 = "Not Reported"

- 4 = "Not Met"

## See also

[`eDataDRF::example_CREED_reliability_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_reliability_tibble.html),
[`eDataDRF::example_CREED_relevance_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_relevance_tibble.html)

## Examples

``` r
mock_input <- creed_tibble_to_mock_input(eDataDRF::example_CREED_reliability_tibble())
mock_input$RB1_score
#>           1 
#> "Fully Met" 
```
