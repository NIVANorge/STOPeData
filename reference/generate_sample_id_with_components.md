# Generate Sample ID with Components —-

Generates vectorised sample identifiers from site, parameter,
compartment, date, and subsample components. This is the STOPeData-local
implementation; see
[`eDataDRF::generate_sample_id_with_components()`](https://NIVANorge.github.io/eDataDRF/reference/generate_sample_id_with_components.html)
for the canonical version and
[`eDataDRF::sample_id_regex()`](https://NIVANorge.github.io/eDataDRF/reference/sample_id_regex.html)
to validate generated IDs.

## Usage

``` r
generate_sample_id_with_components(
  site_code,
  parameter_name,
  environ_compartment,
  environ_compartment_sub,
  date,
  subsample = 1
)
```

## Arguments

- site_code:

  Site code (vectorized)

- parameter_name:

  Parameter name (vectorized). See
  [`eDataDRF::parameters_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameters_vocabulary.html)
  for valid parameter names.

- environ_compartment:

  Environmental compartment (vectorized). Must be a value from
  [`eDataDRF::environ_compartments_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/environ_compartments_vocabulary.html).

- environ_compartment_sub:

  Environmental sub-compartment (vectorized). Must be a value from
  [`eDataDRF::environ_compartments_sub_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/environ_compartments_sub_vocabulary.html).

- date:

  Sampling date (vectorized)

- subsample:

  subsample

## Value

Character vector of sample IDs.

## See also

[`eDataDRF::generate_sample_id_with_components()`](https://NIVANorge.github.io/eDataDRF/reference/generate_sample_id_with_components.html),
[`eDataDRF::sample_id_regex()`](https://NIVANorge.github.io/eDataDRF/reference/sample_id_regex.html),
[`eDataDRF::environ_compartments_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/environ_compartments_vocabulary.html),
[`eDataDRF::environ_compartments_sub_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/environ_compartments_sub_vocabulary.html)

## Examples

``` r
generate_sample_id_with_components(
  site_code = "SITE001",
  parameter_name = "Cadmium",
  environ_compartment = "Water",
  environ_compartment_sub = "Surface water",
  date = as.Date("2022-06-15"),
  subsample = 1
)
#> SITE001-Cadmium-Surfacewater-2022-06-15-R-1

# Validate the generated ID against the regex
id <- generate_sample_id_with_components(
  site_code = eDataDRF::example_sites_tibble()$SITE_CODE[1],
  parameter_name = eDataDRF::example_parameters_tibble()$PARAMETER_NAME[1],
  environ_compartment = eDataDRF::example_compartments_tibble()$ENVIRON_COMPARTMENT[1],
  environ_compartment_sub = eDataDRF::example_compartments_tibble()$ENVIRON_COMPARTMENT_SUB[1],
  date = as.Date("2022-06-15"),
  subsample = 1
)
grepl(eDataDRF::sample_id_regex(), id)
#> [1] TRUE
```
