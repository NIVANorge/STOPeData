# Zenodo license options tibble

Returns a tibble of Zenodo license options as of March 2026. Includes
\`id\`, \`title\`, \`description\`, \`url\`, \`schema\`,
\`osi_approved\`, \`revision_id\`, \`created\`, \`updated\`, and
\`popular\`, a custom boolean highlighting the most commonly used
licenses.

## Usage

``` r
zenodo_licenses
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 444
rows and 10 columns.
