# Crossref Functions ----
# Functions for validating and looking up DOI/PMID identifiers using Crossref

#' Validate DOI format
#'
#' @description
#' Validates whether a string contains a valid DOI format. Accepts DOIs with or
#' without URL prefixes (https://doi.org/ or http://dx.doi.org/).
#'
#' @param input_string Character string to validate as DOI
#'
#' @return Logical indicating whether the input is a valid DOI format
#'
#' @details
#' DOI format follows the pattern: 10.registrant/suffix
#' Where registrant is typically 4+ digits and suffix can contain various characters.
#' This function validates format only, not whether the DOI actually exists.
#'

#' @export
validate_doi_format <- function(input_string) {
  if (
    !is.character(input_string) ||
      length(input_string) != 1 ||
      is.na(input_string)
  ) {
    return(FALSE)
  }

  # Remove common URL prefixes
  clean_string <- gsub(
    "^https?://(dx\\.)?doi\\.org/",
    "",
    input_string,
    ignore.case = TRUE
  )
  clean_string <- trimws(clean_string)

  # DOI pattern from https://www.crossref.org/blog/dois-and-matching-regular-expressions/
  # Catches ~99% of CrossRef DOIs as of 2013...
  doi_pattern <- "^10.\\d{4,9}/[-._;()/:A-Z0-9]+$"

  return(grepl(doi_pattern, clean_string, ignore.case = TRUE))
}

#' Validate PMID format
#'
#' @description
#' Validates whether a string contains a valid PubMed ID (PMID) format.
#' PMIDs are numeric identifiers, typically 1-8 digits.
#'
#' @param input_string Character string to validate as PMID
#'
#' @return Logical indicating whether the input is a valid PMID format
#'
#' @export
validate_pmid_format <- function(input_string) {
  if (
    !is.character(input_string) ||
      length(input_string) != 1 ||
      is.na(input_string)
  ) {
    return(FALSE)
  }

  # Remove PMID: prefix if present
  clean_string <- gsub("^PMID:?\\s*", "", input_string, ignore.case = TRUE)
  clean_string <- trimws(clean_string)

  # PMID pattern: 1-8 digits
  pmid_pattern <- "^[0-9]{1,8}$"

  return(grepl(pmid_pattern, clean_string))
}

#' Extract clean DOI from input string
#'
#' @description
#' Extracts a clean DOI string by removing URL prefixes and whitespace.
#'
#' @param input_string Character string containing DOI
#'
#' @return Clean DOI string or the original input if no cleaning needed
#'
#' @export
extract_clean_doi <- function(input_string) {
  clean_string <- gsub(
    "^https?://(dx\\.)?doi\\.org/",
    "",
    input_string,
    ignore.case = TRUE
  )
  return(trimws(clean_string))
}

#' Extract clean PMID from input string
#'
#' @description
#' Extracts a clean PMID string by removing PMID: prefixes and whitespace.
#'
#' @param input_string Character string containing PMID
#'
#' @return Clean PMID string or the original input if no cleaning needed
#'
#' @export
extract_clean_pmid <- function(input_string) {
  clean_string <- gsub("^PMID:?\\s*", "", input_string, ignore.case = TRUE)
  return(trimws(clean_string))
}

#' Convert PMID to DOI using PubMed API
#'
#' @description
#' Attempts to convert a PubMed ID to a DOI by querying the PubMed API.
#' This is necessary because Crossref doesn't directly support PMID lookup.
#'
#' @param pmid Character string containing PMID
#'
#' @return List with components:
#'   - success: Logical indicating whether conversion succeeded
#'   - doi: DOI string (if success = TRUE) or NULL
#'   - message: Status message for user feedback
#'
#' @details
#' Uses the PubMed E-utilities API to fetch article information and extract DOI.
#' Requires internet connection. May fail if PubMed is unavailable or if the
#' article doesn't have a DOI registered.
#'
#' @importFrom httr GET content http_error
#' @importFrom xml2 read_xml xml_find_first xml_text
#'
#' @export
pmid_to_doi <- function(pmid) {
  tryCatch(
    {
      # PubMed E-utilities API URL
      url <- paste0(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?",
        "db=pubmed&id=",
        pmid,
        "&retmode=xml"
      )

      # Make API request
      response <- httr::GET(url)

      if (httr::http_error(response)) {
        return(list(
          success = FALSE,
          doi = NULL,
          message = "PubMed API request failed"
        ))
      }

      # Parse XML response
      xml_content <- httr::content(response, "text", encoding = "UTF-8")
      xml_doc <- xml2::read_xml(xml_content)

      # Look for DOI in ArticleIdList
      doi_node <- xml2::xml_find_first(xml_doc, "//ArticleId[@IdType='doi']")

      if (length(doi_node) == 0 || is.na(doi_node)) {
        return(list(
          success = FALSE,
          doi = NULL,
          message = "No DOI found for this PMID in PubMed"
        ))
      }

      doi <- xml2::xml_text(doi_node)

      if (is.na(doi) || doi == "") {
        return(list(
          success = FALSE,
          doi = NULL,
          message = "Empty DOI found for this PMID"
        ))
      }

      return(list(
        success = TRUE,
        doi = doi,
        message = "PMID successfully converted to DOI"
      ))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        doi = NULL,
        message = paste("Error converting PMID to DOI:", e$message)
      ))
    }
  )
}

#' Lookup publication data using Crossref
#'
#' @description
#' Queries Crossref API using rcrossref::cr_works() to retrieve publication
#' metadata for a given DOI.
#'
#' @param doi Character string containing DOI
#'
#' @return List with components:
#'   - success: Logical indicating whether lookup succeeded
#'   - data: Crossref data (if success = TRUE) or NULL
#'   - message: Status message for user feedback
#'
#' @details
#' Uses rcrossref::cr_works() to query the Crossref API. Requires internet
#' connection. May fail if Crossref is unavailable or if the DOI is not
#' registered with Crossref.
#'
#' @importFrom rcrossref cr_works
#'
#' @export
lookup_crossref_doi <- function(doi) {
  tryCatch(
    {
      # Query Crossref
      result <- rcrossref::cr_works(doi)

      if (is.null(result) || is.null(result$data)) {
        return(list(
          success = FALSE,
          data = NULL,
          message = "Valid DOI format, but no record found in Crossref"
        ))
      }

      return(list(
        success = TRUE,
        data = result$data,
        message = "Publication data retrieved successfully from Crossref"
      ))
    },
    error = function(e) {
      # Check if it's a "not found" error vs other errors
      if (grepl("404|not found", e$message, ignore.case = TRUE)) {
        return(list(
          success = FALSE,
          data = NULL,
          message = "Valid DOI format, but no record found in Crossref"
        ))
      } else {
        return(list(
          success = FALSE,
          data = NULL,
          message = paste("Crossref lookup failed:", e$message)
        ))
      }
    },
    warning = function(e) {
      # Check if it's a "not found" error vs other errors
      if (grepl("404|not found", e$message, ignore.case = TRUE)) {
        return(list(
          success = FALSE,
          data = NULL,
          message = "Valid DOI format, but no record found in Crossref"
        ))
      } else {
        return(list(
          success = FALSE,
          data = NULL,
          message = paste("Crossref lookup failed:", e$message)
        ))
      }
    }
  )
}

#' Map Crossref data to reference input fields
#'
#' @description
#' Converts Crossref API response data into a list of values that correspond
#' to the reference module input fields.
#'
#' @param crossref_data Data frame returned from rcrossref::cr_works()
#' @param access_date Date to use for ACCESS_DATE field. Defaults to today's date.
#'
#' @return A named list containing mapped field values for all reference input fields.
#'   Values are NA for fields not present in the Crossref data.
#'
#' @importFrom stringr str_extract
#' @importFrom stats na.omit
#' @details
#' Maps Crossref fields to reference fields:
#' - type → REFERENCE_TYPE (journal-article → journal, book → book, etc.)
#' - title → TITLE
#' - author → AUTHOR (formatted as Last, First; Last, First)
#' - published.online/published.print → YEAR
#' - container.title → PERIODICAL_JOURNAL
#' - volume → VOLUME
#' - issue → ISSUE
#' - publisher → PUBLISHER
#' - DOI → DOI
#' - URL → URL
#' - page → PAGES
#' - ISBN/ISSN → ISBN_ISSN
#'
#' @export
map_crossref_to_reference_fields <- function(
  crossref_data,
  access_date = Sys.Date()
) {
  # Input validation
  if (!is.data.frame(crossref_data) || nrow(crossref_data) == 0) {
    stop("crossref_data must be a non-empty data frame")
  }

  # Take first row if multiple entries
  entry <- crossref_data[1, ]

  # Map Crossref reference types to our reference types ----
  ref_type_mapping <- list(
    "journal-article" = "journal",
    "book" = "book",
    "book-chapter" = "book",
    "report" = "report",
    "dataset" = "dataset",
    "proceedings-article" = "journal", # Conference papers → journal
    "monograph" = "book",
    "reference-book" = "book",
    "edited-book" = "book"
  )

  # Get reference type, default to journal if not found
  ref_type <- ref_type_mapping[[entry$type %||% ""]] %||% "journal"

  # Helper function to safely extract and convert values (or return NA if they don't exist)
  safe_extract <- function(col_name, convert_func = identity) {
    if (col_name %in% colnames(entry)) {
      value <- entry[[col_name]]
      if (is.null(value) || is.na(value) || value == "") {
        return(NA)
      }
      tryCatch(
        convert_func(value),
        error = function(e) NA,
        warning = function(e) NA
      )
    } else {
      return(NA)
    }
  }

  # Format authors (Crossref provides structured author data)
  format_authors <- function(author_df) {
    tryCatch(author_df <- author_df[[1]], error = function(e) {
      "Could not unnest list"
    })
    if (is.null(author_df) || nrow(author_df) == 0) {
      return(NA)
    }
    # Handle case where author_df might be a string already
    if (is.character(author_df)) {
      return(author_df)
    }
    # If it's a data frame of author objects, format them
    if (is.data.frame(author_df)) {
      formatted_authors <- character(0)
      has_name_col <- "name" %in% colnames(author_df)
      has_given_col <- "given" %in% colnames(author_df)
      has_family_col <- "family" %in% colnames(author_df)

      for (i in seq_len(nrow(author_df))) {
        author <- author_df[i, ]

        # Check for organizational name first
        if (has_name_col && !is.na(author$name) && author$name != "") {
          formatted_authors <- c(formatted_authors, author$name)
        } else if (has_family_col) {
          # Handle individual authors with given/family names
          given <- if (has_given_col) author$given %||% "" else ""
          family <- author$family %||% ""
          if (!is.na(family) && family != "") {
            if (!is.na(given) && given != "") {
              formatted_authors <- c(
                formatted_authors,
                paste0(family, ", ", given)
              )
            } else {
              formatted_authors <- c(formatted_authors, family)
            }
          }
        }
      }
      if (length(formatted_authors) > 0) {
        return(paste(formatted_authors, collapse = "; "))
      }
    }
    return(NA)
  }

  # Get year from published date
  pub_year <- na.omit(c(
    safe_extract("published.online"),
    safe_extract("published.print"),
    safe_extract("year"),
    NA
  ))[1] |>
    str_extract(pattern = "[0-9]{4}")
  if (
    !is.na(pub_year) &&
      # sanity check years
      (pub_year < 1800 ||
        pub_year > as.numeric(format(Sys.Date(), "%Y")) + 5)
  ) {
    pub_year <- NA
  }

  # Create field mapping ----
  mapped_fields <- list(
    # Always required fields
    REFERENCE_TYPE = ref_type,
    AUTHOR = if ("author" %in% colnames(entry)) {
      format_authors(entry$author)
    } else {
      NA
    },
    TITLE = safe_extract("title"),
    YEAR = pub_year,
    ACCESS_DATE = as.Date(access_date),

    # Journal-specific fields
    PERIODICAL_JOURNAL = safe_extract("container.title"),
    VOLUME = safe_extract("volume", as.numeric),
    ISSUE = safe_extract("issue", as.numeric),

    # Book-specific fields
    PUBLISHER = safe_extract("publisher"),

    # Report-specific fields (may not be in Crossref)
    INSTITUTION = NA,

    # Dataset-specific fields (may not be in Crossref)
    DB_NAME = NA,
    DB_PROVIDER = NA,

    # Optional fields for all types
    DOI = safe_extract("doi"),
    URL = safe_extract("url"),
    PAGES = safe_extract("page"),
    ISBN_ISSN = safe_extract("issn") %||% safe_extract("isbn"), # Try both
    EDITION = NA, # Not typically in Crossref
    PUBLISHED_PLACE = NA, # Not typically in Crossref
    DOCUMENT_NUMBER = NA,
    ACCESSION_NUMBER = NA,
    PMCID = NA,
    SERIES_TITLE = NA,
    SERIES_EDITOR = NA,
    SERIES_VOLUME = NA,
    NUMBER_OF_PAGES = NA,
    NUMBER_OF_VOLUMES = NA,
    REF_COMMENT = NA,
    ENTERED_BY = NA # Will be filled from session data
  )

  return(mapped_fields)
}

#' Validate and lookup DOI/PMID with comprehensive error handling
#'
#' @description
#' Main function that validates input format, converts PMID to DOI if needed,
#' looks up publication data from Crossref, and returns formatted results.
#'
#' @param input_string Character string containing DOI or PMID
#' @param access_date Date to use for ACCESS_DATE field. Defaults to today's date.
#'
#' @return List with components:
#'   - success: Logical indicating whether lookup succeeded
#'   - data: Mapped field data (if success = TRUE) or NULL
#'   - message: Status message for user feedback
#'   - identifier_type: Type of identifier detected ("doi" or "pmid")
#'
#' @details
#' This is the main entry point for DOI/PMID lookup functionality.
#' It handles the complete workflow:
#' 1. Validate input format
#' 2. Convert PMID to DOI if necessary
#' 3. Query Crossref for publication data
#' 4. Map results to reference field format
#'
#' @export
validate_and_lookup_identifier <- function(
  input_string,
  access_date = Sys.Date()
) {
  # Input validation
  if (!is.character(input_string) || length(input_string) != 1) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "Input must be a single character string",
      identifier_type = NA
    ))
  }

  input_string <- trimws(input_string)

  if (input_string == "") {
    return(list(
      success = FALSE,
      data = NULL,
      message = "Please enter a DOI or PMID before looking up",
      identifier_type = NA
    ))
  }

  # Validate format and determine identifier type
  if (validate_doi_format(input_string)) {
    # It's a DOI
    clean_doi <- extract_clean_doi(input_string)

    # Lookup directly with Crossref
    lookup_result <- lookup_crossref_doi(clean_doi)

    if (!lookup_result$success) {
      return(list(
        success = FALSE,
        data = NULL,
        message = lookup_result$message,
        identifier_type = "doi"
      ))
    }

    # Map Crossref data to reference fields
    mapped_fields <- map_crossref_to_reference_fields(
      lookup_result$data,
      access_date
    )

    return(list(
      success = TRUE,
      data = mapped_fields,
      message = "Publication data retrieved successfully from Crossref",
      identifier_type = "doi"
    ))
  } else if (validate_pmid_format(input_string)) {
    # It's a PMID - convert to DOI first
    clean_pmid <- extract_clean_pmid(input_string)

    # Convert PMID to DOI
    pmid_result <- pmid_to_doi(clean_pmid)

    if (!pmid_result$success) {
      return(list(
        success = FALSE,
        data = NULL,
        message = pmid_result$message,
        identifier_type = "pmid"
      ))
    }

    # Now lookup the DOI with Crossref
    lookup_result <- lookup_crossref_doi(pmid_result$doi)

    if (!lookup_result$success) {
      return(list(
        success = FALSE,
        data = NULL,
        message = "DOI found for PMID, but Crossref lookup failed",
        identifier_type = "pmid"
      ))
    }

    # Map Crossref data to reference fields
    mapped_fields <- map_crossref_to_reference_fields(
      lookup_result$data,
      access_date
    )

    return(list(
      success = TRUE,
      data = mapped_fields,
      message = "PMID converted to DOI and publication data retrieved from Crossref",
      identifier_type = "pmid"
    ))
  } else {
    # Invalid format
    return(list(
      success = FALSE,
      data = NULL,
      message = "DOI or PMID format not recognised. Please check input or try another option.",
      identifier_type = NA
    ))
  }
}
