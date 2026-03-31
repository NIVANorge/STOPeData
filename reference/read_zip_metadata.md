# Read metadata from ZIP file —-

Extract and read metadata from a ZIP file without importing data

## Usage

``` r
read_zip_metadata(zip_path)
```

## Arguments

- zip_path:

  Path to the ZIP file

## Value

List with metadata information or NULL if not found/readable

## Examples

``` r
if (FALSE) { # \dontrun{
  metadata <- read_zip_metadata("path/to/session_export.zip")
  metadata$campaign_name
} # }
```
