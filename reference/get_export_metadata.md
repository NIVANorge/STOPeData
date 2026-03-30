# Get export metadata

Gather metadata about the current export session

## Usage

``` r
get_export_metadata(session = NULL)
```

## Arguments

- session:

  Shiny session object. Required to access user data and client
  information.

## Value

List containing export metadata fields (campaign_name, export_datetime,
app_name, etc.)
