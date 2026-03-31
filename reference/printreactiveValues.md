# Print content of reactiveValues object

Print a reactiveValues object, with each named variable and its value on
a new line

## Usage

``` r
printreactiveValues(data)
```

## Arguments

- data:

  A reactiveValues object with named variables

## Value

A string of variable names and values

## Examples

``` r
if (FALSE) { # \dontrun{
  rv <- shiny::reactiveValues(campaign_name = "North Sea 2022", year = 2022)
  printreactiveValues(rv)
} # }
```
