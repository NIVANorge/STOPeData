#' Create new compartment combination row
#'
#' Creates a single-row tibble with specified compartment information,
#' using the standardised structure from initialise_compartments_tibble().
#'
#' @param compartment Character string specifying the environmental compartment
#' @param sub_compartment Character string specifying the sub-compartment
#' @param category Character string specifying the measured category
#'
#' @return tibble with one row containing the specified compartment information
#' @family create
#' @importFrom dplyr add_row
#' @import eDataDRF
#' @examples
#' create_compartment_combination("Water", "Surface water", "Abiotic")
#' create_compartment_combination("Biota", "Fish", "Biotic")
#' @export
#' @seealso \code{\link{initialise_compartments_tibble}}
create_compartment_combination <- function(
  compartment,
  sub_compartment,
  category
) {
  initialise_compartments_tibble() |>
    add_row(
      ENVIRON_COMPARTMENT = compartment,
      ENVIRON_COMPARTMENT_SUB = sub_compartment,
      MEASURED_CATEGORY = category
    )
}
