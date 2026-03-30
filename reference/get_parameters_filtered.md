# Get parameter names filtered by type and optionally subtype

Returns a character vector of parameter names filtered by type and
optionally by subtype, combining base parameters with session-specific
parameters.

## Usage

``` r
get_parameters_filtered(
  param_type,
  param_subtype = "Show all",
  dummy_parameters,
  session_parameters = NULL
)
```

## Arguments

- param_type:

  Character string specifying the parameter type

- param_subtype:

  Character string specifying the parameter subtype (optional)

- dummy_parameters:

  Dataframe containing base parameters

- session_parameters:

  Optional list containing session-specific parameters

## Value

Character vector of parameter names
