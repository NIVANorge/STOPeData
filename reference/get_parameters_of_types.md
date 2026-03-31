# Get parameter names for a specific parameter type

Returns a character vector of parameter names for the given type,
combining base parameters with any session-specific parameters.

## Usage

``` r
get_parameters_of_types(
  param_type,
  dummy_parameters,
  session_parameters = NULL
)
```

## Arguments

- param_type:

  Character string specifying the parameter type. Must be a value from
  [`eDataDRF::parameter_types_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_vocabulary.html).

- dummy_parameters:

  Dataframe containing base parameters. Use
  [`eDataDRF::parameters_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameters_vocabulary.html)
  to obtain the full parameter list.

- session_parameters:

  Optional list containing session-specific parameters

## Value

Character vector of parameter names

## See also

[`eDataDRF::parameters_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameters_vocabulary.html),
[`eDataDRF::parameter_types_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_vocabulary.html),
[`eDataDRF::parameter_types_sub_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_sub_vocabulary.html)

## Examples

``` r
params <- get_parameters_of_types("Chemical", eDataDRF::parameters_vocabulary())
head(params)
#> [1] "-- New Parameter --"
```
