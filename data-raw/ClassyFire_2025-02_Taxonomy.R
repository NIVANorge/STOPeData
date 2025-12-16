## code to prepare `ClassyFire_Taxonomy` dataset goes here
## Get data from ECOTOX first
source("data-raw/inst-ecotox_2025_06_12_chemicals-txt.R") # needed to filter chemicals
chemicals_vector <- chemicals$CAS_RN

# usethis::use_data(ClassyFire_Taxonomy, overwrite = TRUE)

# From https://ice.ntp.niehs.nih.gov/DATASETDESCRIPTION?section=Chemical%20Taxonomies

chemical_taxonomy <- readr::read_tsv(
  file = "inst/extdata/raw/ClassyFire_2025-02_ChemicalTaxonomies.txt",
  col_names = TRUE,
  trim_ws = TRUE,
  col_select = c(Chemical_Name, CASRN, DTXSID, InChiKey, Superclass) # save some time, we don't need the whole taxonomy system
)


filtered <- chemical_taxonomy |>
  # filter to substances in ECOTOX dataset
  dplyr::filter(CASRN %in% chemicals_vector) |>
  dplyr::mutate(
    PARAMETER_NAME = dplyr::case_when(
      is.na(Chemical_Name) ~ "Not relevant",
      TRUE ~ Chemical_Name
    ),
    CAS_RN = CASRN,
    INCHIKEY_SD = InChiKey,
    PARAMETER_TYPE = "Stressor",
    PARAMETER_TYPE_SUB = Superclass,
    .keep = "none"
  )

filtered |>
  arrow::write_parquet(
    sink = "inst/extdata/clean/ClassyFire_Taxonomy_2025_02.parquet"
  )
