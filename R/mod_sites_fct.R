#' Create a new site record
#'
#' @description
#' Creates a new site record with auto-generated site code and default values.
#' The site code is generated using either a base code or a default "SITE_" prefix,
#' followed by a zero-padded three-digit number.
#'
#' @param site_number Integer. The site number to use in the site code. Default is 1.
#' @param base_code Character. Optional prefix for the site code. If empty or NULL,
#'   defaults to "SITE_". Default is "".
#' @param session Shiny session object. Used to extract the ENTERED_BY value from
#'   session$userData$reactiveValues$campaignData$ENTERED_BY.
#'
#' @return A tibble with one row containing the new site record, initialized with
#'   default values according to the eDataDRF sites schema.
#' @importFrom eDataDRF initialise_sites_tibble
#' @export
create_new_site <- function(site_number = 1, base_code = "", session) {
  # Generate site code ----
  if (base_code == "" || is.null(base_code)) {
    site_code <- paste0("SITE_", sprintf("%03d", site_number))
  } else {
    site_code <- paste0(base_code, sprintf("%03d", site_number))
  }

  # Initialize and populate site record ----
  eDataDRF::initialise_sites_tibble() |>
    add_row(
      SITE_CODE = site_code,
      SITE_NAME = "",
      SITE_GEOGRAPHIC_FEATURE = "Not reported",
      SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
      SITE_COORDINATE_SYSTEM = "WGS 84",
      LATITUDE = NA,
      LONGITUDE = NA,
      COUNTRY_ISO = "",
      OCEAN_IHO = "",
      ALTITUDE_VALUE = NA,
      ALTITUDE_UNIT = "m",
      ENTERED_BY = session$userData$reactiveValues$campaignData$ENTERED_BY %|truthy|%
        "",
      ENTERED_DATE = as.character(Sys.Date()),
      SITE_COMMENT = ""
    )
}
