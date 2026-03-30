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

  List of lists, each with elements \`first\`, \`last\`,
  \`affiliation\`, and \`orcid\` (all character, may be empty strings).

- license_id:

  Character. Zenodo license identifier, e.g. \`"cc-by-4.0"\`. The
  human-readable title is looked up from \`zenodo_licenses\`.

- contact_name:

  Character. Display name or role for the contact.

- contact_email:

  Character. Contact email address.

## Value

Character string containing the full README in markdown format.
