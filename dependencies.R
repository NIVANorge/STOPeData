# Install pak
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))

# Install all packages in one call for better dependency resolution
pak::pkg_install(c(
  "devtools",
  "ISOcodes",
  "dplyr",
  "shiny",
  "DT",
  "arrow",
  "bsicons",
  "bslib",
  "ellmer",
  "ggplot2",
  "glue",
  "googledrive",
  "htmltools",
  "httr",
  "jsonlite",
  "leaflet",
  "openxlsx2",
  "plotly",
  "purrr",
  "rcrossref",
  "readr",
  "rhandsontable",
  "shinyWidgets",
  "shinyjs",
  "shinyvalidate",
  "stringi",
  "stringr",
  "tibble",
  "tidyr",
  "xml2",
  "yaml",
  "zip",
  "bib2df",
  "lubridate",
  "markdown",
  "shinytest2"
))

# Avoid a later version of Golem, which breaks the app


pak::pkg_install("https://github.com/ThinkR-open/golem/releases/tag/v0.5.1")
