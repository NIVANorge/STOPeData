## code to prepare `ecotox_species` dataset goes here

# usethis::use_data(ecotox_species, overwrite = TRUE)

## code to prepare `inst/ecotox_2025_06_12_chemicals.txt` dataset goes here

# downloaded from EPA ECOTOX

# usethis::use_data(
#   "inst/extdata/raw/ecotox_2025_06_12_chemicals.txt",
#   overwrite = TRUE
# )

species <- readr::read_delim(
  file = "inst/extdata/raw/ecotox_2025_06_12_species.txt",
  delim = "|",
  col_names = TRUE,
  col_types = "ccccccccccccccccc",
  trim_ws = TRUE,
) |>
  dplyr::select(common_name, latin_name, kingdom, ecotox_group) |>
  # at least one row has an invalid character
  dplyr::mutate(dplyr::across(
    dplyr::where(is.character),
    ~ stringr::str_replace_all(.x, "[^\\x20-\\x7E\\u00A0-\\uFFFF]", "")
  )) |>
  dplyr::mutate(
    # first element of species_group is most useful for filtering?
    species_group = stringr::str_split_i(ecotox_group, ";", i = 1)
  )

species |>
  arrow::write_parquet(
    sink = "inst/extdata/clean/ecotox_2025_06_12_species.parquet"
  )
