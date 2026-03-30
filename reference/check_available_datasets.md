# Check which datasets contain data and get their dimensions

Check which datasets contain data and get their dimensions

## Usage

``` r
check_available_datasets(rv)
```

## Arguments

- rv:

  standard reactiveValues object from the app

## Value

List with three elements: - available_datasets: character vector of
dataset names with data - dataset_dimensions: named list of lists with
rows/cols for each available dataset - export_ready: logical indicating
if any datasets are available
