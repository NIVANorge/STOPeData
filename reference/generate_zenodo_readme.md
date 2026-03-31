# Generate a Zenodo submission README

Produces a markdown-formatted README string suitable for attaching to a
Zenodo record. Used both as the companion file uploaded with the dataset
and as the preview shown in the production confirmation modal.

## Usage

``` r
generate_zenodo_readme(
  title,
  description,
  authors,
  license_id,
  contact_name = "",
  contact_email = ""
)
```

## Arguments

- title:

  Character. Dataset title.

- description:

  Character. Dataset description.

- authors:

  List of lists, each with elements `first`, `last`, `affiliation`, and
  `orcid` (all character, may be empty strings).

- license_id:

  Character. Zenodo license identifier, e.g. `"cc-by-4.0"`. The
  human-readable title is looked up from `zenodo_licenses`.

- contact_name:

  Character. Display name or role for the contact.

- contact_email:

  Character. Contact email address.

## Value

Character string containing the full README in markdown format.

## See also

Other zenodo:
[`zenodo_licenses`](https://nivanorge.github.io/STOPeData/reference/zenodo_licenses.md),
[`zenodo_resource_types`](https://nivanorge.github.io/STOPeData/reference/zenodo_resource_types.md)

## Examples

``` r
readme <- generate_zenodo_readme(
  title = "Marine contaminant monitoring 2020-2022",
  description = "Trace metal concentrations in blue mussels from Norwegian fjords.",
  authors = list(
    list(first = "Jane", last = "Smith", affiliation = "NIVA", orcid = "")
  ),
  license_id = "cc-by-4.0",
  contact_name = "Jane Smith",
  contact_email = "jane.smith@niva.no"
)
cat(readme)
#> # Marine contaminant monitoring 2020-2022
#> - **Zenodo DOI:** (Will be generated upon upload)
#> 
#> ---
#> 
#> ## What is this?
#> 
#> Trace metal concentrations in blue mussels from Norwegian fjords.
#> 
#> ---
#> 
#> ## Authors
#> 
#> **Primary Author:** Jane Smith
#>   **Institution:** NIVA
#> 
#> ---
#> 
#> ## How to cite
#> 
#> Smith J. (2026). *Marine contaminant monitoring 2020-2022*. Zenodo. https://doi.org/[DOI-will-be-generated]
#> 
#> ---
#> 
#> ## License
#> 
#> Released under **Creative Commons Attribution 4.0 International**.
#> 
#> ---
#> 
#> ## Contact
#> 
#> - **Name:** Jane Smith
#> - **Email:** jane.smith@niva.no
#> - **Institution:** NIVA
```
