# Format a duration in seconds as a human-readable string

Converts a numeric number of seconds into a readable string such as "2
hours 13 minutes 51 seconds".

## Usage

``` r
format_duration(seconds)
```

## Arguments

- seconds:

  Numeric. Number of seconds to format.

## Value

Character string.

## Examples

``` r
format_duration(7451)   # "2 hours 4 minutes 11 seconds"
#> [1] "2 hours 4 minutes 11 seconds"
format_duration(30)     # "30 seconds"
#> [1] "30 seconds"
```
