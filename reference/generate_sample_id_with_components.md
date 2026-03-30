# Generate Sample ID with Components —-

Generates vectorised sample identifiers from site, parameter,
compartment, date, and subsample components.

## Usage

``` r
generate_sample_id_with_components(
  site_code,
  parameter_name,
  environ_compartment,
  environ_compartment_sub,
  date,
  subsample = 1
)
```

## Arguments

- site_code:

  Site code (vectorized)

- parameter_name:

  Parameter name (vectorized)

- environ_compartment:

  Environmental compartment (vectorized)

- environ_compartment_sub:

  Environmental sub-compartment (vectorized)

- date:

  Sampling date (vectorized)

- subsample:

  subsample

## Value

Character vector of sample IDs.
