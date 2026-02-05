#' Generate Protocol ID ----
#'
#' @description
#' Generates a standardised protocol identifier based on protocol type, name,
#' sequence number, and campaign. The function is fully vectorised and can
#' handle multiple protocols simultaneously.
#'
#' @param protocol_type Character vector. The category of protocol
#'   (e.g., "Sampling Protocol", "Analytical Protocol"). Must match one of
#'   the predefined categories or will default to "X".
#' @param protocol_name Character vector. The specific name/method within
#'   the protocol category. Will be abbreviated and cleaned for ID generation.
#' @param sequence_number Numeric vector. Sequential number for protocols
#'   within the same category. Defaults to 1. Will be zero-padded to 2 digits.
#' @param campaign_name Character vector. Name of the campaign/study.
#'   Will be abbreviated to first 10 alphanumeric characters. Defaults to "".
#'
#'
#'   Where:
#'   - TypeCode: S (Sampling), F (Fractionation), E (Extraction),
#'     A (Analytical), X (Unknown)
#'   - SequenceNumber: Zero-padded 2-digit number
#'   - AbbreviatedName: Up to 15 alphanumeric characters (optional)
#'   - CampaignAbbrev: Up to 10 alphanumeric characters (optional)
#'
#' @details
#' The function handles edge cases gracefully:
#' - NULL or empty values result in appropriate defaults
#' - Invalid protocol types default to "X"
#' - Names are cleaned of special characters and spaces
#' - Empty components are omitted from the final ID
#'
#' @family protocol_functions
#' @importFrom stringr str_trim str_split str_to_upper str_to_lower str_sub
#'   str_remove_all str_c
#' @export
generate_protocol_id <- function(
  protocol_type,
  protocol_name,
  sequence_number = 1,
  campaign_name = ""
) {
  # Map protocol categories to single letters ----
  type_mapping <- c(
    "Sampling Protocol" = "S",
    "Fractionation Protocol" = "F",
    "Extraction Protocol" = "E",
    "Analytical Protocol" = "A"
  )

  # Get the letter code (vectorised) ----
  type_code <- type_mapping[protocol_type]
  type_code <- ifelse(is.na(type_code), "X", type_code)

  # Create abbreviated name (vectorised) ----
  # Handle NULL, NA, and empty values
  is_empty <- is.na(protocol_name) | str_trim(protocol_name) == ""

  abbreviated_name <- ifelse(
    is_empty,
    "",
    {
      # Split by whitespace, capitalize first letter of each word, rejoin
      words <- str_split(protocol_name, "\\s+")

      # Process each set of words
      processed <- vapply(
        words,
        FUN = function(word_vec) {
          # Capitalize and combine words
          capitalized <- str_c(
            str_to_upper(str_sub(word_vec, 1, 1)),
            str_to_lower(str_sub(word_vec, 2, -1))
          )
          # Join words, remove non-alphanumeric, truncate
          str_c(capitalized, collapse = "") |>
            str_remove_all("[^A-Za-z0-9]") |>
            str_sub(1, 15)
        },
        FUN.VALUE = character(1)
      )

      processed
    }
  )

  # Format sequence number (vectorised) ----
  formatted_sequence <- sprintf("%02d", sequence_number)

  # Create campaign abbreviation (vectorised) ----
  is_empty_campaign <- is.na(campaign_name) | str_trim(campaign_name) == ""

  campaign_abbrev <- ifelse(
    is_empty_campaign,
    "",
    campaign_name |>
      str_remove_all("[^A-Za-z0-9]") |>
      str_sub(1, 10)
  )

  # Combine all parts (vectorised) ----
  protocol_id <- str_c(
    type_code,
    formatted_sequence,
    ifelse(abbreviated_name == "", "", str_c("_", abbreviated_name)),
    ifelse(campaign_abbrev == "", "", str_c("_", campaign_abbrev))
  )

  return(protocol_id)
}
