# Create parameter row from existing parameter data

Create parameter row from existing parameter data

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

`initialise_parameters_tibble`
