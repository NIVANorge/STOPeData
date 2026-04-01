# Import data from exported ZIP file

Import datasets from a ZIP file created by mod_export

## Usage

``` r
import_session_from_zip(zip_path, session)
```

## Arguments

- zip_path:

  Path to the ZIP file

- session:

  Shiny session object

## Value

List with success status and user-facing message

## Examples

``` r
if (FALSE) { # \dontrun{
  # session is the Shiny session object from the module server function
  result <- import_session_from_zip("path/to/session_export.zip", session)
  result$success
} # }
```
