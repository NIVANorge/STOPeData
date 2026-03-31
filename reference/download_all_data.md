# Download all data as CSV and TXT files in a ZIP archive

Creates a Shiny downloadHandler that exports all available datasets as
CSV files (for tabular data) or TXT files (for text/object data) in a
single ZIP archive. Also includes a metadata file with export
information.

## Usage

``` r
download_all_data(session, moduleState = NULL)
```

## Arguments

- session:

  Shiny session object. Required to access reactive values.

- moduleState:

  ReactiveValues object containing export_ready flag,
  available_datasets, and campaign_name fields.

## Value

A Shiny downloadHandler function

## Examples

``` r
if (FALSE) { # \dontrun{
  # Used inside a Shiny module server function
  output$download_btn <- download_all_data(session, moduleState)
} # }
```
