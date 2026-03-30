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
