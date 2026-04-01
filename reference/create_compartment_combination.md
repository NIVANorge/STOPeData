# Create new compartment combination row

Creates a single-row tibble with specified compartment information,
using the standardised structure from
[`eDataDRF::initialise_campaign_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_campaign_tibble.html).
Function will return an error if compartment, sub_compartment, or
category are not of the correct variable type, but it doesn't check for
vocabulary

## Usage

``` r
create_compartment_combination(compartment, sub_compartment, category)
```

## Arguments

- compartment:

  Character string specifying the environmental compartment
  ([`eDataDRF::environ_compartments_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/environ_compartments_vocabulary.html))

- sub_compartment:

  Character string specifying the sub-compartment
  ([`eDataDRF::environ_compartments_sub_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/environ_compartments_sub_vocabulary.html))

- category:

  Character string specifying the measured category
  ([`eDataDRF::measured_categories_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/measured_categories_vocabulary.html))

## Value

tibble with one row containing the specified compartment information

## See also

[`eDataDRF::initialise_campaign_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_campaign_tibble.html)

Other create:
[`create_dummy_session_data()`](https://nivanorge.github.io/STOPeData/reference/create_dummy_session_data.md),
[`create_existing_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_existing_parameter.md),
[`create_new_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_new_parameter.md),
[`create_new_site()`](https://nivanorge.github.io/STOPeData/reference/create_new_site.md)

## Examples

``` r
create_compartment_combination("Aquatic", "Surface water", "External")
#> # A tibble: 1 × 3
#>   ENVIRON_COMPARTMENT ENVIRON_COMPARTMENT_SUB MEASURED_CATEGORY
#>   <chr>               <chr>                   <chr>            
#> 1 Aquatic             Surface water           External         
create_compartment_combination("Biota", "Biota, Aquatic", "Internal")
#> # A tibble: 1 × 3
#>   ENVIRON_COMPARTMENT ENVIRON_COMPARTMENT_SUB MEASURED_CATEGORY
#>   <chr>               <chr>                   <chr>            
#> 1 Biota               Biota, Aquatic          Internal         
```
