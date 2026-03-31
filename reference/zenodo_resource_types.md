# Zenodo resource type options vector

Named character vector of Zenodo resource types. Names are display
labels; values are Zenodo resource type identifiers. See
<https://help.zenodo.org/docs/deposit/describe-records/resource-type/>
for more details. Filtered to values likely to be used in this
application.

## Usage

``` r
zenodo_resource_types
```

## Format

An object of class `character` of length 5.

## See also

Other zenodo:
[`generate_zenodo_readme()`](https://nivanorge.github.io/STOPeData/reference/generate_zenodo_readme.md),
[`zenodo_licenses`](https://nivanorge.github.io/STOPeData/reference/zenodo_licenses.md)

## Examples

``` r
zenodo_resource_types
#>                  Book               Dataset                Report 
#>    "publication-book"             "dataset"  "publication-report" 
#>       Journal Article                 Other 
#> "publication-article"               "other" 
zenodo_resource_types["Dataset"]
#>   Dataset 
#> "dataset" 
```
