# Convert object to human-readable text

Convert various R objects (character vectors, lists, S3/S4 objects) to
readable text format suitable for writing to .txt files

## Usage

``` r
object_to_text(obj, dataset_name = "unknown")
```

## Arguments

- obj:

  The object to convert (character, list, ellmer_schema, or other object
  types)

- dataset_name:

  Character. Name of the dataset for header context

## Value

Character vector suitable for use with writeLines()

## Examples

``` r
object_to_text(list(a = 1, b = "hello"), dataset_name = "my_list")
#> [1] "# my_list"                       "# Exported: 2026-03-31 11:40:52"
#> [3] "# Type: List"                    ""                               
#> [5] "# Structure:"                    "list(a = 1, b = \"hello\")"     
object_to_text("already a string")
#> [1] "already a string"
```
