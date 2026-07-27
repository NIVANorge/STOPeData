# Create Dummy Session Data

Creates a complete userData-like list structure populated with dummy
data from all
[`eDataDRF::example_campaign_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_campaign_tibble.html)
and related functions. This mirrors the structure created by
initialise_userData() but with test data instead of empty tibbles.

Useful for testing outside of a reactive context.

## Usage

``` r
create_dummy_session_data()
```

## Value

A list matching the structure of initialise_userData() with dummy data

## See also

[`eDataDRF::example_campaign_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_campaign_tibble.html),
[`eDataDRF::example_references_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_references_tibble.html),
[`eDataDRF::example_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_sites_tibble.html),
[`eDataDRF::example_parameters_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_parameters_tibble.html),
[`eDataDRF::example_compartments_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_compartments_tibble.html),
[`eDataDRF::example_methods_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_methods_tibble.html),
[`eDataDRF::example_samples_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_samples_tibble.html),
[`eDataDRF::example_biota_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_biota_tibble.html),
[`eDataDRF::example_measurements_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_measurements_tibble.html),
[`eDataDRF::example_CREED_reliability_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_reliability_tibble.html),
[`eDataDRF::example_CREED_relevance_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_relevance_tibble.html),
[`eDataDRF::example_CREED_scores_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_CREED_scores_tibble.html)

Other create:
[`create_compartment_combination()`](https://nivanorge.github.io/STOPeData/reference/create_compartment_combination.md),
[`create_existing_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_existing_parameter.md),
[`create_new_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_new_parameter.md),
[`create_new_site()`](https://nivanorge.github.io/STOPeData/reference/create_new_site.md)

## Examples

``` r
session_data <- create_dummy_session_data()
#> Error in guess_where_config(path): Unable to locate a config file from the default location.Please restore this file or use the 'GOLEM_CONFIG_PATH' environment variable to
#>       set a custom path to the config file.
#> The default path is: /home/runner/work/STOPeData/STOPeData/docs/reference/inst/golem-config.yml
names(session_data)
#> Error: object 'session_data' not found
nrow(session_data$sitesData)
#> Error: object 'session_data' not found
```
