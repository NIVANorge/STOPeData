## code to prepare `IHO_Oceans` dataset goes here
# From https://doi.pangaea.de/10.1594/PANGAEA.777975

# usethis::use_data(IHO_Oceans, overwrite = TRUE)

oceans <- readr::read_tsv(
  "inst/extdata/raw/Limits_of_oceans_and_seas.tsv",
  skip = 15
) |>
  dplyr::mutate(
    NAME = stringr::str_remove_all(
      `Name (of ocean or sea)`,
      pattern = ", eastern part|, western part"
    ),
    .keep = "none"
  ) |>
  dplyr::distinct() |>
  arrange(NAME)

readr::write_rds(x = oceans, file = "inst/extdata/clean/IHO_oceans.rds")
