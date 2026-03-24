#' Zenodo license options tibble
#'
#' @description Returns a 444x10 tibble of Zenodo license options as of March 2026.
#' Includes `id`, `title`, `description`, `url`, `schema`, `osi_approved`, `revision_id`,
#' `created`, `updated`, and `popular`, a custom boolean highlighting the most commonly
#' used licenses.
#'
#' @importFrom tibble tibble
#' @export
zenodo_licenses <- readRDS("inst/extdata/clean/Zenodo_Licenses_2026_03.rds") |>
  as_tibble()

#' Zenodo resource type options vector
#'
#' @description Named character vector of Zenodo resource types.
#'   Names are display labels; values are Zenodo resource type identifiers.
#'   See \cite{https://help.zenodo.org/docs/deposit/describe-records/resource-type/}
#'   for more details. Filtered to values likely to be used in this application.
#'
#' @export
zenodo_resource_types <- c(
  "Book" = "publication-book",
  "Dataset" = "dataset",
  "Report" = "publication-report",
  "Journal Article" = "publication-article",
  "Other" = "other"
)
