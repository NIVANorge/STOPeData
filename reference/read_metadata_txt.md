# Read metadata from text file —-

Read metadata from a human-readable text file

## Usage

``` r
read_metadata_txt(file_path)
```

## Arguments

- file_path:

  Path to the metadata text file

## Value

List with metadata or NULL if not found/readable

## Examples

``` r
if (FALSE) { # \dontrun{
  metadata <- read_metadata_txt("path/to/export_metadata.txt")
  metadata$campaign_name
} # }
```
