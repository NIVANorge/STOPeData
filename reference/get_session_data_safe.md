# Safely retrieve data from session reactive values

Safely retrieve data from session reactive values

## Usage

``` r
get_session_data_safe(
  session,
  table_name,
  variable_name,
  fallback_value = "Unknown"
)
```

## Arguments

- session:

  Shiny session object containing userData\$reactiveValues

- table_name:

  Character string: name of the table in reactiveValues

- variable_name:

  Character string: name of the variable/column to retrieve

- fallback_value:

  Character string: value to return if conditions not met (default:
  "Unknown")

## Value

Value from the specified table/variable if available, fallback_value
otherwise
