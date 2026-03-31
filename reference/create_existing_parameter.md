# Create parameter row from existing parameter data

Looks up an existing parameter by name and type, returning a populated
tibble row or NULL if not found.

## Usage

``` r
create_existing_parameter(
  param_type,
  param_name,
  dummy_parameters,
  session_parameters = NULL
)
```

## Arguments

- param_type:

  Character string specifying the parameter type

- param_name:

  Character string specifying the parameter name

- dummy_parameters:

  Dataframe containing base parameters

- session_parameters:

  Optional dataframe containing session-specific parameters

## Value

tibble with parameter information or NULL if not found

## See also

[`eDataDRF::initialise_parameters_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_parameters_tibble.html)

Other create:
[`create_compartment_combination()`](https://nivanorge.github.io/STOPeData/reference/create_compartment_combination.md),
[`create_dummy_session_data()`](https://nivanorge.github.io/STOPeData/reference/create_dummy_session_data.md),
[`create_new_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_new_parameter.md),
[`create_new_site()`](https://nivanorge.github.io/STOPeData/reference/create_new_site.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # dummy_parameters is a dataframe from the eDataDRF package
  row <- create_existing_parameter("Chemical", "Cadmium", dummy_parameters)
  row
} # }
```
