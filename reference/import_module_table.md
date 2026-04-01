# Import module dataset from CSV

Import a single dataset and add to session reactiveValues

## Usage

``` r
import_module_table(csv_path, session)
```

## Arguments

- csv_path:

  Path to CSV file

- session:

  Shiny session object

## Value

List with success status and details

## Examples

``` r
if (FALSE) { # \dontrun{
  # session is the Shiny session object from the module server function
  result <- import_module_table("path/to/Sites_export.csv", session)
  result$rows_imported
} # }
```
