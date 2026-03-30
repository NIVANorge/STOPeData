# Create a collapsible single-panel accordion containing a markdown file

Creates a collapsible single-panel bslib accordion displaying the
contents of a markdown file, with an info icon.

## Usage

``` r
info_accordion(title = "Instructions", content_file, ...)
```

## Arguments

- title:

  the desired title of the accordion panel

- content_file:

  the path to a markdown file

- ...:

  other arguments to accordion()

## Value

a bslib::accordion html element
