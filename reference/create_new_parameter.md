# Create new blank parameter row

Creates a single-row template tibble with default values for a new
parameter.

## Usage

``` r
create_new_parameter(param_type, entered_by)
```

## Arguments

- param_type:

  Character string specifying the parameter type

- entered_by:

  Character string specifying who entered the parameter

## Value

tibble with blank parameter template

## See also

`initialise_parameters_tibble`

Other create:
[`create_compartment_combination()`](https://nivanorge.github.io/STOPeData/reference/create_compartment_combination.md),
[`create_dummy_session_data()`](https://nivanorge.github.io/STOPeData/reference/create_dummy_session_data.md),
[`create_existing_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_existing_parameter.md),
[`create_new_site()`](https://nivanorge.github.io/STOPeData/reference/create_new_site.md)

## Examples

``` r
create_new_parameter("Chemical", "Jane Smith")
#> # A tibble: 1 × 10
#>   PARAMETER_TYPE PARAMETER_TYPE_SUB MEASURED_TYPE PARAMETER_NAME
#>   <chr>          <chr>              <chr>         <chr>         
#> 1 Chemical       ""                 Concentration ""            
#> # ℹ 6 more variables: PARAMETER_NAME_SUB <chr>, INCHIKEY_SD <chr>,
#> #   PUBCHEM_CID <int>, CAS_RN <chr>, ENTERED_BY <chr>, PARAMETER_COMMENT <chr>
```
