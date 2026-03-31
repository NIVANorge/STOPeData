# Get export metadata

Gather metadata about the current export session. The `campaign_name`
field is drawn from the `CAMPAIGN_NAME` column of the campaign table
(see
[`eDataDRF::initialise_campaign_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_campaign_tibble.html)).

## Usage

``` r
get_export_metadata(session = NULL)
```

## Arguments

- session:

  Shiny session object. Required to access user data and client
  information.

## Value

List containing export metadata fields (campaign_name, export_datetime,
app_name, etc.)

## See also

[`eDataDRF::initialise_campaign_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/initialise_campaign_tibble.html),
[`eDataDRF::example_campaign_tibble()`](https://NIVANorge.github.io/eDataDRF/reference/example_campaign_tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
  # session is the Shiny session object from the module server function
  meta <- get_export_metadata(session)
  meta$campaign_name
} # }
```
