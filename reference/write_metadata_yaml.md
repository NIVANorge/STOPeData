# Write metadata as a YAML file

Serialises a metadata list to a YAML file using the yaml package.

## Usage

``` r
write_metadata_yaml(metadata_list, file_path)
```

## Arguments

- metadata_list:

  List. The metadata to write.

- file_path:

  Character. Path where to write the YAML file.

## Value

NULL (invisibly). File is written to disk as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
  write_metadata_yaml(list(session = list(user = "Ole")), tempfile(fileext = ".yaml"))
} # }
```
