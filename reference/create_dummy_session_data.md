# Create Dummy Session Data

Creates a complete userData-like list structure populated with dummy
data from all dummy\_\*\_tibble() functions. This mirrors the structure
created by initialise_userData() but with test data instead of empty
tibbles.

Useful for testing outside of a reactive context.

## Usage

``` r
create_dummy_session_data()
```

## Value

A list matching the structure of initialise_userData() with dummy data
