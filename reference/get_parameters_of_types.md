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

  Character string specifying the parameter type

- dummy_parameters:

  Dataframe containing base parameters

- session_parameters:

  Optional list containing session-specific parameters

## Value

Character vector of parameter names
