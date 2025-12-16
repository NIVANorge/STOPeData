## code to prepare `claude-2025-08-05-methods.csv` dataset goes here

# usethis::use_data(claude-2025-08-05-methods.csv, overwrite = TRUE)

methods_table <- readr::read_csv(
  file = "inst/extdata/raw/inst-claude-2025-08-05-methods.csv"
)

arrow::write_parquet(
  methods_table,
  sink = "inst/extdata/clean/inst-claude-2025-08-05-methods.parquet"
)
