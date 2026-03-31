# Safely retrieve data from session reactive values

Safely retrieves a column or variable from a named table stored in
session reactive values, returning a fallback value if the table or
variable is not available.

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

## Examples

``` r
if (FALSE) { # \dontrun{
  # session is the Shiny session object from the module server function
  site_codes <- get_session_data_safe(session, "sitesData", "SITE_CODE")
  entered_by <- get_session_data_safe(session, "campaignData", "ENTERED_BY",
                                      fallback_value = "Unknown user")
} # }
```
