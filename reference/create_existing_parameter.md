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

  Character string specifying the parameter type. Must be a value from
  [`eDataDRF::parameter_types_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_vocabulary.html).

- param_name:

  Character string specifying the parameter name. Must be a value from
  [`eDataDRF::parameters_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameters_vocabulary.html).

- dummy_parameters:

  Dataframe containing base parameters. Use
  [`eDataDRF::parameters_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameters_vocabulary.html)
  to obtain the full parameter list.

- session_parameters:

  Optional dataframe containing session-specific parameters

## Value

tibble with parameter information or NULL if not found

## See also

[`eDataDRF::initialise_parameters_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_parameters_tibble.html),
[`eDataDRF::parameters_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameters_vocabulary.html),
[`eDataDRF::parameter_types_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_vocabulary.html)

Other create:
[`create_compartment_combination()`](https://nivanorge.github.io/STOPeData/reference/create_compartment_combination.md),
[`create_dummy_session_data()`](https://nivanorge.github.io/STOPeData/reference/create_dummy_session_data.md),
[`create_new_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_new_parameter.md),
[`create_new_site()`](https://nivanorge.github.io/STOPeData/reference/create_new_site.md)

## Examples

``` r
create_existing_parameter("Chemical", "Cadmium", eDataDRF::parameters_vocabulary())
#> # A tibble: 1 × 10
#>   PARAMETER_TYPE PARAMETER_TYPE_SUB          MEASURED_TYPE PARAMETER_NAME
#>   <chr>          <chr>                       <chr>         <chr>         
#> 1 Chemical       Homogeneous metal compounds Concentration Cadmium       
#> # ℹ 6 more variables: PARAMETER_NAME_SUB <chr>, INCHIKEY_SD <chr>,
#> #   PUBCHEM_CID <int>, CAS_RN <chr>, ENTERED_BY <chr>, PARAMETER_COMMENT <chr>
```
