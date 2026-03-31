# Create a new site record

Creates a new site record with auto-generated site code and default
values. The site code is generated using either a base code or a default
"SITE\_" prefix, followed by a zero-padded three-digit number.

The record structure follows
[`eDataDRF::initialise_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_sites_tibble.html).
Default values for geographic feature, coordinate system, and altitude
unit are drawn from the relevant controlled vocabularies.

## Usage

``` r
create_new_site(site_number = 1, base_code = "", session)
```

## Arguments

- site_number:

  Integer. The site number to use in the site code. Default is 1.

- base_code:

  Character. Optional prefix for the site code. If empty or NULL,
  defaults to "SITE\_". Default is "".

- session:

  Shiny session object. Used to extract the ENTERED_BY value from
  session\$userData\$reactiveValues\$campaignData\$ENTERED_BY.

## Value

A tibble with one row containing the new site record, initialized with
default values according to the eDataDRF sites schema
([`eDataDRF::initialise_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_sites_tibble.html)).

## See also

[`eDataDRF::initialise_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_sites_tibble.html),
[`eDataDRF::example_sites_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_sites_tibble.html),
[`eDataDRF::geographic_features_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/geographic_features_vocabulary.html),
[`eDataDRF::coordinate_systems_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/coordinate_systems_vocabulary.html),
[`eDataDRF::countries_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/countries_vocabulary.html),
[`eDataDRF::ocean_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/ocean_vocabulary.html),
[`eDataDRF::altitude_units_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/altitude_units_vocabulary.html)

Other create:
[`create_compartment_combination()`](https://nivanorge.github.io/STOPeData/reference/create_compartment_combination.md),
[`create_dummy_session_data()`](https://nivanorge.github.io/STOPeData/reference/create_dummy_session_data.md),
[`create_existing_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_existing_parameter.md),
[`create_new_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_new_parameter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # session is the Shiny session object from the module server function
  new_site <- create_new_site(site_number = 1, base_code = "FJORD", session = session)
  new_site$SITE_CODE
} # }
```
