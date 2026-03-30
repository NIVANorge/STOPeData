# isTruthy switch operator

isTruthy switch operator

## Usage

``` r
first %|truthy|% second
```

## Arguments

- first:

  The first argument, a value we want to use only if it isTruthy

- second:

  The second argument, a safe alternative if first isn't Truthy

## Value

object types are unchanged

## Details

utilities

A simple switch operator based on purrr's argument if it isTruthy
(exists, not NULL/NA/"", etc.), and otherwise the second argument.
