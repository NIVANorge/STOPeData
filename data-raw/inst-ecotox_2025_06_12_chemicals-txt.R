## code to prepare `inst/ecotox_2025_06_12_chemicals.txt` dataset goes here

# downloaded from EPA ECOTOX

# usethis::use_data(
#   "inst/extdata/raw/ecotox_2025_06_12_chemicals.txt",
#   overwrite = TRUE
# )

# Function to format CAS numbers ----
format_cas_number <- function(cas_string) {
  # Remove any existing hyphens or spaces first
  clean_cas <- stringr::str_remove_all(cas_string, "[-\\s]")
  cas_length <- stringr::str_length(clean_cas)

  # Format: take all but last 3 digits, then last 2, then last 1
  formatted <- stringr::str_replace(
    clean_cas,
    "^(\\d+)(\\d{2})(\\d{1})$",
    "\\1-\\2-\\3"
  )

  # Return NA for invalid lengths, otherwise return formatted
  dplyr::if_else(
    cas_length < 3 | cas_length > 10, # Note: | not ||
    NA_character_,
    formatted
  )
}

chemicals <- readr::read_delim(
  file = "inst/extdata/raw/ecotox_2025_06_12_chemicals.txt",
  delim = "|",
  col_names = TRUE,
  col_types = "cccc",
  trim_ws = TRUE,
) |>
  dplyr::mutate(
    # Use iconv with substitution for problematic characters
    PARAMETER_NAME = iconv(chemical_name, to = "UTF-8", sub = "") |>
      # ASCII apostrophes cause problems?
      gsub(pattern = "'", replacement = "'"),
    CAS_RN = format_cas_number(cas_number),
    .keep = "none"
  )

chemicals |>
  arrow::write_parquet(
    sink = "inst/extdata/clean/ecotox_2025_06_12_chemicals.parquet"
  )
