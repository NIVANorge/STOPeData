#' Create new compartment combination row
#'
#' Creates a single-row tibble with specified compartment information,
#' using the standardised structure from [eDataDRF::initialise_campaign_tibble()].
#' Function will return an error if compartment, sub_compartment, or category are
#' not of the correct variable type, but it doesn't check for vocabulary
#'
#' @param compartment Character string specifying the environmental compartment ([eDataDRF::environ_compartments_vocabulary()])
#' @param sub_compartment Character string specifying the sub-compartment ([eDataDRF::environ_compartments_sub_vocabulary()])
#' @param category Character string specifying the measured category ([eDataDRF::measured_categories_vocabulary()])
#'
#' @return tibble with one row containing the specified compartment information
#' @family create
#' @importFrom dplyr add_row
#' @import eDataDRF
#' @examples
#' create_compartment_combination("Aquatic", "Surface water", "External")
#' create_compartment_combination("Biota", "Biota, Aquatic", "Internal")
#' @export
#' @seealso [eDataDRF::initialise_campaign_tibble()]
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
