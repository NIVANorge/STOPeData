# Zenodo license list
#
# Source: Zenodo REST API
#   GET https://zenodo.org/api/licenses?size=1000&q=&page=1
#
# Retrieved: March 2026
#
# The API returns a paginated list of all licenses registered in Zenodo.
# The `popular` column is a custom boolean added by the package maintainer
# to flag licenses most commonly used for scientific datasets (CC-BY-4.0,
# CC0-1.0, etc.).
#
# To refresh this dataset:
#   1. Call the Zenodo API and parse the JSON response into a tibble.
#   2. Add the `popular` column for the most relevant licenses.
#   3. Re-run usethis::use_data() below.

library(tibble)

# zenodo_licenses <- <fetch from API and process> |> as_tibble()

usethis::use_data(zenodo_licenses, overwrite = TRUE)
