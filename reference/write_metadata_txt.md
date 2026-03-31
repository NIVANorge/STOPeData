# Create readable metadata text file

Create a human-readable text file with export metadata

## Usage

``` r
write_metadata_txt(metadata_list, file_path)
```

## Arguments

- metadata_list:

  List containing metadata fields (campaign_name, export_datetime, etc.)

- file_path:

  Character. Path where to write the metadata file

## Value

NULL (invisibly). File is written to disk as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
  meta <- list(campaign_name = "North Sea 2022", export_datetime = Sys.time(),
               user = "Jane", app_name = "STOPeData", app_version = "1.0",
               clientData = "localhost")
  write_metadata_txt(meta, tempfile(fileext = ".txt"))
} # }
```
