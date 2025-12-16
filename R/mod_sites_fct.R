#' @import eDataDRF
create_new_site <- function(site_number = 1, base_code = "", session) {
  # Generate site code
  if (base_code == "" || is.null(base_code)) {
    site_code <- paste0("SITE_", sprintf("%03d", site_number))
  } else {
    site_code <- paste0(base_code, sprintf("%03d", site_number))
  }

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
