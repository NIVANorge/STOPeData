# Create a new site record

Creates a new site record with auto-generated site code and default
values. The site code is generated using either a base code or a default
"SITE\_" prefix, followed by a zero-padded three-digit number.

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
default values according to the eDataDRF sites schema.
