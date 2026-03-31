# Create new compartment combination row

Creates a single-row tibble with specified compartment information,
using the standardised structure from initialise_compartments_tibble().

## Usage

``` r
create_compartment_combination(compartment, sub_compartment, category)
```

## Arguments

- compartment:

  Character string specifying the environmental compartment

- sub_compartment:

  Character string specifying the sub-compartment

- category:

  Character string specifying the measured category

## Value

tibble with one row containing the specified compartment information

## See also

`initialise_compartments_tibble`

Other create:
[`create_dummy_session_data()`](https://nivanorge.github.io/STOPeData/reference/create_dummy_session_data.md),
[`create_existing_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_existing_parameter.md),
[`create_new_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_new_parameter.md),
[`create_new_site()`](https://nivanorge.github.io/STOPeData/reference/create_new_site.md)

## Examples

``` r
create_compartment_combination("Water", "Surface water", "Abiotic")
#> # A tibble: 1 × 3
#>   ENVIRON_COMPARTMENT ENVIRON_COMPARTMENT_SUB MEASURED_CATEGORY
#>   <chr>               <chr>                   <chr>            
#> 1 Water               Surface water           Abiotic          
create_compartment_combination("Biota", "Fish", "Biotic")
#> # A tibble: 1 × 3
#>   ENVIRON_COMPARTMENT ENVIRON_COMPARTMENT_SUB MEASURED_CATEGORY
#>   <chr>               <chr>                   <chr>            
#> 1 Biota               Fish                    Biotic           
```
