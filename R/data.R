# Package datasets ----

#' Zenodo license options tibble
#'
#' @description A tibble of Zenodo license options as of March 2026.
#' Includes `id`, `title`, `description`, `url`, `schema`, `osi_approved`,
#' `revision_id`, `created`, `updated`, and `popular`, a custom boolean
#' highlighting the most commonly used licenses.
#'
#' @format A tibble with 444 rows and 10 columns:
#' \describe{
#'   \item{id}{Character. Zenodo license identifier, e.g. `"cc-by-4.0"`.}
#'   \item{title}{Character. Human-readable license name.}
#'   \item{description}{Character. License description.}
#'   \item{url}{Character. URL to the full license text.}
#'   \item{schema}{Character. License schema identifier.}
#'   \item{osi_approved}{Logical. Whether the license is OSI-approved.}
#'   \item{revision_id}{Integer. Zenodo internal revision identifier.}
#'   \item{created}{Character. ISO 8601 creation timestamp.}
#'   \item{updated}{Character. ISO 8601 last-updated timestamp.}
#'   \item{popular}{Logical. Custom flag for licenses commonly used in scientific datasets.}
#' }
#'
#' @source Zenodo REST API: `GET https://zenodo.org/api/licenses?size=1000`
#'   Retrieved March 2026. See `data-raw/zenodo_licenses.R` for details.
#'
#' @family zenodo
#' @examples
#' # Show only popular licenses
#' zenodo_licenses[zenodo_licenses$popular, c("id", "title")]
#' @export
"zenodo_licenses"
