# Create a collapsible single-panel accordion containing a markdown file

Creates a collapsible single-panel bslib accordion displaying the
contents of a markdown file, with an info icon.

`content_file` is a package-relative path (everything after `inst/`),
resolved with `app_sys()` (defined in `app_config.R`, not exported by
golem). It is deliberately *not* a `www/` URL like image `src`s are: the
markdown is read from disk by R and rendered into the HTML server side,
so the browser never requests it and it needs no resource path. Hence
the files live in `inst/app/md/`, outside the publicly-served
`inst/app/www/`.

## Usage

``` r
info_accordion(title = "Instructions", content_file, ...)
```

## Arguments

- title:

  the desired title of the accordion panel

- content_file:

  package-relative path to a markdown file, e.g.
  `"app/md/intro_sites.md"`. `NULL` renders an empty panel.

- ...:

  other arguments to accordion()

## Value

a bslib::accordion html element

## Examples

``` r
if (FALSE) { # \dontrun{
  # Used inside a Shiny UI function
  info_accordion("Instructions", "app/md/intro_sites.md")
} # }
```
