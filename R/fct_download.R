# Metadata Format Helper Functions
# Functions to create and read metadata files

#' Format a duration in seconds as a human-readable string
#'
#' @description Converts a numeric number of seconds into a readable string
#'   such as "2 hours 13 minutes 51 seconds".
#' @param seconds Numeric. Number of seconds to format.
#' @return Character string.
#' @examples
#' format_duration(7451)   # "2 hours 4 minutes 11 seconds"
#' format_duration(30)     # "30 seconds"
#' @export
format_duration <- function(seconds) {
  seconds <- round(as.numeric(seconds))
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  parts <- c(
    if (h > 0) paste(h, if (h == 1) "hour" else "hours"),
    if (m > 0) paste(m, if (m == 1) "minute" else "minutes"),
    paste(s, if (s == 1) "second" else "seconds")
  )
  paste(parts, collapse = " ")
}

#' Write metadata as a YAML file
#'
#' @description Serialises a metadata list to a YAML file using the yaml package.
#' @param metadata_list List. The metadata to write.
#' @param file_path Character. Path where to write the YAML file.
#' @return NULL (invisibly). File is written to disk as a side effect.
#' @importFrom yaml write_yaml
#' @examples
#' \dontrun{
#'   write_metadata_yaml(list(session = list(user = "Ole")), tempfile(fileext = ".yaml"))
#' }
#' @export
write_metadata_yaml <- function(metadata_list, file_path) {
  write_yaml(metadata_list, file_path)
  invisible(NULL)
}

#' Create readable metadata text file
#'
#' @description Create a human-readable text file with export metadata.
#'   Retained for backward compatibility; new exports use [write_metadata_yaml()].
#' @param metadata_list List containing metadata fields (campaign_name, export_datetime, etc.)
#' @param file_path Character. Path where to write the metadata file
#' @return NULL (invisibly). File is written to disk as a side effect.
#' @importFrom glue glue
#' @examples
#' \dontrun{
#'   meta <- list(campaign_name = "North Sea 2022", export_datetime = Sys.time(),
#'                user = "Ole", app_name = "STOPeData", app_version = "1.0",
#'                clientData = "localhost")
#'   write_metadata_txt(meta, tempfile(fileext = ".txt"))
#' }
#' @export
write_metadata_txt <- function(metadata_list, file_path) {
  # Helper operator for string repetition
  `%r%` <- function(string, times) {
    paste(rep(string, times), collapse = "")
  }

  # Create human-readable content
  content <- c(
    "STOPeData Export Metadata",
    "=" %r% 50,
    "",
    glue("Campaign Name: {metadata_list$campaign_name}"),
    glue("Export Date/Time: {metadata_list$export_datetime}"),
    glue("Exported By: {metadata_list$user}"),
    "",
    "Application Information:",
    glue("  App Name: {metadata_list$app_name}"),
    glue("  App Version: {metadata_list$app_version}"),
    glue("  Format Version: {metadata_list$format_version}"),
    glue("  Client Data:\n {metadata_list$clientData}"),
    "",
    "",
    "=" %r% 50,
    "",
    "This ZIP file contains CSV data files exported from STOPeData.",
    "Each CSV file contains one type of data (sites, parameters, etc.).",
    "To re-import this data, use the 'Upload session data' option",
    "in STOPeData."
  )

  # Write to file
  writeLines(content, file_path)
}

#' Get git commit hash
#'
#' @description Retrieve the short git commit hash of the current repository state
#' @return Character. Short git commit hash, or "Git hash not available" if retrieval fails
#' @examples
#' get_git_commit()
#' @export
get_git_commit <- function() {
  tryCatch(
    {
      system("git rev-parse --short HEAD", intern = TRUE)
    },
    error = function(e) "Git hash not available"
  )
}

#' Get export metadata
#'
#' @description Gather metadata about the current export session. The
#'   `campaign_name` field is drawn from the `CAMPAIGN_NAME` column of the
#'   campaign table (see [eDataDRF::initialise_campaign_tibble()]).
#' @param session Shiny session object. Required to access user data and client information.
#' @return List containing export metadata fields (campaign_name, export_datetime, app_name, etc.)
#' @importFrom golem get_golem_version
#' @importFrom rlang `%||%`
#' @seealso [eDataDRF::initialise_campaign_tibble()],
#'   [eDataDRF::example_campaign_tibble()]
#' @examples
#' \dontrun{
#'   # session is the Shiny session object from the module server function
#'   meta <- get_export_metadata(session)
#'   meta$campaign_name
#' }
#' @export
get_export_metadata <- function(session = NULL) {
  if (is.null(session)) {
    stop("session reactive object must be supplied to create CSVs")
  }
  rv <- session$userData$reactiveValues
  meta <- rv$metaData

  # Compute elapsed time from session start
  session_start_time <- tryCatch(
    as.POSIXct(meta$session$session_start, format = "%Y-%m-%d %H:%M:%S"),
    error = function(e) Sys.time()
  )
  elapsed_secs <- as.numeric(difftime(
    Sys.time(),
    session_start_time,
    units = "secs"
  ))

  # Add export-level fields (assembled fresh at download time, not stored reactively)
  meta$export <- list(
    campaign_name = tryCatch(
      rv$campaignData$CAMPAIGN_NAME[[1]],
      error = function(e) NA_character_
    ),
    export_datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    time_from_session_start = format_duration(elapsed_secs)
  )

  meta
}

#' Create metadata tibble
#'
#' @description Convert metadata list to tibble format suitable for Excel sheets
#' @param metadata_list List containing metadata fields
#' @return Tibble with Property and Value columns
#' @importFrom tibble tibble
#' @examples
#' create_metadata_tibble(list(campaign = "North Sea 2022", version = "1.0", user = "Jane"))
#' @export
create_metadata_tibble <- function(metadata_list) {
  s <- metadata_list$session %||% list()
  e <- metadata_list$export %||% list()

  flat <- list(
    "Session: App" = s$app_name %||% NA_character_,
    "Session: Version" = s$app_version %||% NA_character_,
    "Session: Format Version" = s$format_version %||% NA_character_,
    "Session: Start" = s$session_start %||% NA_character_,
    "Session: User" = s$user %||% "Unknown",
    "Export: Campaign" = e$campaign_name %||% NA_character_,
    "Export: Datetime" = e$export_datetime %||% NA_character_,
    "Export: Session Duration" = e$time_from_session_start %||% NA_character_,
    "Extractions (YAML)" = if (length(metadata_list$extractions) > 0) {
      as.yaml(metadata_list$extractions)
    } else {
      "None"
    }
  )

  tibble(
    Property = names(flat),
    Value = as.character(unlist(flat, use.names = FALSE))
  )
}

#' Get dataset display name
#'
#' @description Convert internal dataset names to user-friendly display names
#' @param dataset_name Character. Internal name of the dataset (e.g., "sitesData")
#' @return Character. User-friendly display name (e.g., "Sites")
#' @importFrom rlang `%||%`
#' @examples
#' get_dataset_display_name("sitesData")
#' get_dataset_display_name("measurementsData")
#' get_dataset_display_name("creedReliability")
#' @export
get_dataset_display_name <- function(dataset_name) {
  display_names <- c(
    sitesData = "Sites",
    parametersData = "Parameters",
    compartmentsData = "Compartments",
    referenceData = "Reference",
    campaignData = "Campaign",
    methodsData = "Methods",
    samplesData = "Samples",
    biotaData = "Biota",
    measurementsData = "Measurements",
    schemaLLM = "LLM_Schema",
    promptLLM = "LLM_Prompt",
    rawLLM = "LLM_Raw_Response",
    metaData = "Metadata",
    creedRelevance = "CREED_RV",
    creedReliability = "CREED_RB",
    creedScores = "CREED_Score"
  )

  display_names[[dataset_name]] %||% dataset_name
}

# Function: Check Available Datasets
#' Check which datasets contain data and get their dimensions
#'
#' @description Checks each downloadable dataset in session reactive values for
#'   content, returning availability status and dimension information.
#' @param rv Standard reactiveValues object from the app.
#' @return List with three elements: `available_datasets` (character vector of
#'   dataset names with data), `dataset_dimensions` (named list of rows/cols per
#'   dataset), and `export_ready` (logical).
#' @importFrom utils capture.output str
#' @noRd
check_available_datasets <- function(rv) {
  available <- character(0)
  dimensions <- list()

  for (dataset in downloadable_tabular_datasets()) {
    data <- rv[[dataset]]

    if (dataset %in% downloadable_text_datasets()) {
      # Handle text/object data - check if it exists and has content
      has_content <- FALSE
      char_count <- 0

      if (!is.null(data)) {
        if (is.character(data) && length(data) > 0 && nchar(data[1]) > 0) {
          has_content <- TRUE
          char_count <- nchar(data[1])
        } else if (is.list(data) && length(data) > 0) {
          # For lists (like rawLLM), check if it has any content
          has_content <- TRUE
          char_count <- nchar(paste(
            capture.output(str(data)),
            collapse = "\n"
          ))
        } else if (inherits(data, "ellmer_schema") || is.object(data)) {
          # For schema objects or other objects
          has_content <- TRUE
          char_count <- nchar(paste(
            capture.output(print(data)),
            collapse = "\n"
          ))
        }
      }

      if (has_content) {
        available <- c(available, dataset)
        dimensions[[dataset]] <- list(
          type = "text",
          chars = char_count
        )
      }
    } else {
      # Handle tabular data
      if (!is.null(data) && nrow(data) > 0) {
        available <- c(available, dataset)
        dimensions[[dataset]] <- list(
          type = "tabular",
          rows = nrow(data),
          cols = ncol(data)
        )
      }
    }
  }

  list(
    available_datasets = available,
    dataset_dimensions = dimensions,
    export_ready = length(available) > 0
  )
}

# Function: Extract Campaign Name
#' Extract campaign name from campaign data for use in filenames
#'
#' @description Extracts the campaign name from a campaign data frame for
#'   use in constructing export filenames.
#' @param campaign_data Data frame containing campaign information.
#' @return Character string of campaign name, or `NULL` if not found/invalid.
#' @noRd
extract_campaign_name <- function(campaign_data) {
  if (is.null(campaign_data) || nrow(campaign_data) == 0) {
    return(NULL)
  }

  if (!"CAMPAIGN_NAME" %in% names(campaign_data)) {
    return(NULL)
  }

  campaign_name <- campaign_data$CAMPAIGN_NAME[1]

  if (is.na(campaign_name) || campaign_name == "") {
    return(NULL)
  }

  campaign_name
}

#' Convert object to human-readable text
#'
#' @description Convert various R objects (character vectors, lists, S3/S4 objects) to
#'   readable text format suitable for writing to .txt files
#' @param obj The object to convert (character, list, ellmer_schema, or other object types)
#' @param dataset_name Character. Name of the dataset for header context
#' @return Character vector suitable for use with writeLines()
#' @importFrom glue glue
#' @importFrom utils str capture.output
#' @examples
#' object_to_text(list(a = 1, b = "hello"), dataset_name = "my_list")
#' object_to_text("already a string")
#' @export
object_to_text <- function(obj, dataset_name = "unknown") {
  if (is.character(obj)) {
    # Already a character vector - return as-is
    return(obj)
  } else if (is.list(obj)) {
    # For lists, use str() to get a structured view
    header <- c(
      glue("# {dataset_name}"),
      glue("# Exported: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
      "# Type: List",
      "",
      "# Structure:"
    )
    structure_text <- capture.output(dput(obj))

    return(c(header, structure_text))
  } else if (inherits(obj, "ellmer_schema") || is.object(obj)) {
    # For schema objects or other S3/S4 objects, use print()
    header <- c(
      glue("# {dataset_name}"),
      glue("# Exported: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
      glue("# Type: {class(obj)[1]}"),
      ""
    )
    object_text <- capture.output(print(obj))

    return(c(header, object_text))
  } else {
    # Fallback for other types
    header <- c(
      glue("# {dataset_name}"),
      glue("# Exported: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
      glue("# Type: {class(obj)[1]}"),
      ""
    )
    text <- capture.output(print(obj))

    return(c(header, text))
  }
}

#' List downloadable tabular dataset names
#'
#' @description Returns the names of all reactive value datasets that should be
#'   exported as CSV files in the session ZIP archive.
#' @return Character vector of dataset names.
#' @noRd
downloadable_tabular_datasets <- function() {
  c(
    "sitesData",
    "parametersData",
    "compartmentsData",
    "referenceData",
    "campaignData",
    "methodsData",
    "samplesData",
    "biotaData",
    "measurementsData",
    "creedRelevance",
    "creedReliability",
    # "creedDetails", # Fixme: Renable when needed
    "creedScores"
  )
}

#' List downloadable text/object dataset names
#'
#' @description Returns the names of reactive value datasets that should be
#'   exported as TXT files (LLM schema, prompt, and raw response).
#' @return Character vector of dataset names.
#' @noRd
downloadable_text_datasets <- function() {
  # Note: metaData is no longer exported as a text object — it is written as a
  # dedicated YAML file by build_session_zip() / write_metadata_yaml().
  c("schemaLLM", "promptLLM", "rawLLM")
}


#' Build a session ZIP archive
#'
#' @description Writes all available session datasets to a ZIP file at `dest_file`.
#'   Includes tabular data as CSV, text/object data as TXT, the source PDF (if
#'   present), and a metadata file. Called by both `download_all_data()` and the
#'   Zenodo module's session-upload mode.
#'
#'   The ZIP filename is derived from a reference ID generated by
#'   [eDataDRF::generate_reference_id()].
#' @param session Shiny session object. Required to access reactive values.
#' @param moduleState ReactiveValues object containing `available_datasets` and
#'   `campaign_name` fields.
#' @param dest_file Character. Path where the ZIP file should be written.
#' @return `dest_file` invisibly.
#' @importFrom glue glue
#' @importFrom zip zip
#' @importFrom readr write_excel_csv
#' @importFrom stringr str_detect
#' @importFrom golem print_dev
#' @importFrom rlang `%||%`
#' @importFrom shiny showNotification
#' @importFrom eDataDRF generate_reference_id
#' @seealso [eDataDRF::generate_reference_id()], [download_all_data()]
#' @examples
#' \dontrun{
#'   # session and moduleState are Shiny reactive objects
#'   build_session_zip(session, moduleState, dest_file = tempfile(fileext = ".zip"))
#' }
#' @export
build_session_zip <- function(session, moduleState, dest_file) {
  rv <- session$userData$reactiveValues
  metadata <- get_export_metadata(session = session)

  temp_dir <- tempdir()
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  campaign <- if (nrow(rv$referenceData) > 0) {
    generate_reference_id(
      rv$referenceData$YEAR,
      rv$referenceData$AUTHOR,
      rv$referenceData$TITLE
    )
  } else {
    "ReferenceNotFound"
  }

  # metaData is exported as a dedicated YAML file below, not via object_to_text()
  text_datasets <- c("schemaLLM", "promptLLM", "rawLLM")
  all_files <- character(0)

  # Export each dataset ----
  for (dataset_name in moduleState$available_datasets) {
    tryCatch(
      {
        if (stringr::str_detect(dataset_name, "CREED")) {}
        print_dev(glue("prepping {dataset_name} for export"))

        data <- rv[[dataset_name]]

        display_name <- gsub(" ", "_", get_dataset_display_name(dataset_name))
        base_name <- glue("{campaign}_{display_name}_{timestamp}")

        if (dataset_name %in% text_datasets) {
          if (!is.null(data)) {
            txt_file <- file.path(temp_dir, glue("{base_name}.txt"))
            writeLines(
              object_to_text(data, dataset_name = display_name),
              txt_file
            )
            all_files <- c(all_files, txt_file)
          }
        } else {
          if (!is.null(data) && nrow(data) > 0) {
            csv_file <- file.path(temp_dir, glue("{base_name}.csv"))
            write_excel_csv(data, file = csv_file, na = "")
            all_files <- c(all_files, csv_file)
          }
        }
      },
      error = function(e) {
        showNotification(
          paste0(
            "Error creating download data: ",
            e$message,
            " (Code: build_session_zip())"
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  }

  # Export PDF if available ----
  if (!is.null(rv$pdfPath) && file.exists(rv$pdfPath)) {
    pdf_dest <- file.path(
      temp_dir,
      rv$uploaded_pdf_filename %||% glue("{campaign}_MS.pdf")
    )
    file.copy(rv$pdfPath, pdf_dest)
    all_files <- c(all_files, pdf_dest)
  }

  # Write metadata YAML ----
  combined_metadata_file <- file.path(
    temp_dir,
    glue("{campaign}_metadata_{timestamp}.yaml")
  )
  write_metadata_yaml(metadata, combined_metadata_file)
  all_files <- c(all_files, combined_metadata_file)

  # Write screening comments as a separate YAML if present ----
  if (!is.null(rv$metaData$screening) && length(rv$metaData$screening) > 0) {
    screening_file <- file.path(
      temp_dir,
      glue("{campaign}_screening_comments_{timestamp}.yaml")
    )
    write_metadata_yaml(list(screening = rv$metaData$screening), screening_file)
    all_files <- c(all_files, screening_file)
  }

  # Create and cleanup ZIP archive ----
  zip(zipfile = dest_file, files = all_files, mode = "cherry-pick")
  unlink(all_files)

  invisible(dest_file)
}

#' Download all data as CSV and TXT files in a ZIP archive
#'
#' @description Creates a Shiny downloadHandler that exports all available datasets
#'   as CSV files (for tabular data) or TXT files (for text/object data) in a single ZIP archive.
#'   Also includes a metadata file with export information.
#'
#'   The ZIP filename is derived from a reference ID generated by
#'   [eDataDRF::generate_reference_id()].
#' @param session Shiny session object. Required to access reactive values.
#' @param moduleState ReactiveValues object containing export_ready flag, available_datasets,
#'   and campaign_name fields.
#' @return A Shiny downloadHandler function
#' @importFrom glue glue
#' @importFrom shiny downloadHandler
#' @seealso [eDataDRF::generate_reference_id()], [build_session_zip()]
#' @examples
#' \dontrun{
#'   # Used inside a Shiny module server function
#'   output$download_btn <- download_all_data(session, moduleState)
#' }
#' @export
download_all_data <- function(session, moduleState = NULL) {
  if (is.null(moduleState) || is.null(session)) {
    stop(
      "moduleState & session reactive objects must be supplied to create CSVs"
    )
  }

  campaign_short <- function() {
    if (nrow(session$userData$reactiveValues$referenceData) > 0) {
      eDataDRF::generate_reference_id(
        session$userData$reactiveValues$referenceData$YEAR,
        session$userData$reactiveValues$referenceData$AUTHOR,
        session$userData$reactiveValues$referenceData$TITLE
      )
    } else {
      "ReferenceNotFound"
    }
  }

  downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      campaign <- campaign_short()
      glue("{campaign}_AllData_{timestamp}.zip")
    },
    content = function(file) build_session_zip(session, moduleState, file),
    contentType = "application/zip"
  )
}

#' Write CREED report as human-readable text file
#'
#' @description Merge CREED details, relevance, and reliability tibbles into
#'   a single human-readable text file with section headers
#' @param creed_details Tibble with field/value columns from summarise_CREED_details()
#' @param creed_relevance Tibble with field/value columns from summarise_CREED_relevance().
#'   Use [eDataDRF::example_CREED_relevance_tibble()] to generate example data.
#' @param creed_reliability Tibble with field/value columns from summarise_CREED_reliability().
#'   Use [eDataDRF::example_CREED_reliability_tibble()] to generate example data.
#' @param file_path Character. Path where to write the report file
#' @return NULL (invisibly). File is written to disk as a side effect.
#' @importFrom glue glue
#' @seealso [eDataDRF::example_CREED_relevance_tibble()],
#'   [eDataDRF::example_CREED_reliability_tibble()],
#'   [eDataDRF::initialise_CREED_data_tibble()]
#' @examples
#' \dontrun{
#'   # creed_* tibbles are produced by the summarise_CREED_* functions
#'   write_creed_report_txt(creed_details, creed_relevance, creed_reliability,
#'                          file_path = tempfile(fileext = ".txt"))
#' }
#' @export
write_creed_report_txt <- function(
  creed_details,
  creed_relevance,
  creed_reliability,
  file_path
) {
  # Helper for string repetition

  `%r%` <- function(string, times) {
    paste(rep(string, times), collapse = "")
  }

  # Helper to format a tibble section
  format_section <- function(tbl, section_title) {
    if (
      is.null(tbl) ||
        !is.data.frame(tbl) ||
        nrow(tbl) == 0 ||
        !all(c("field", "value") %in% names(tbl))
    ) {
      return(c(
        section_title,
        "-" %r% nchar(section_title),
        "No data available",
        ""
      ))
    }

    lines <- c(
      section_title,
      "-" %r% nchar(section_title),
      ""
    )

    for (i in seq_len(nrow(tbl))) {
      field <- tbl$field[i]
      value <- tbl$value[i]
      # Handle multi-line values by indenting continuation lines
      value_lines <- strsplit(as.character(value), "\n")[[1]]
      if (length(value_lines) == 1) {
        lines <- c(lines, paste0(field, ": ", value_lines))
      } else {
        lines <- c(lines, paste0(field, ": ", value_lines[1]))
        for (v in value_lines[-1]) {
          lines <- c(
            lines,
            paste0(
              "
 ",
              v
            )
          )
        }
      }
    }

    c(lines, "")
  }

  # Build content
  content <- c(
    "CREED Assessment Report",
    "=" %r% 50,
    glue("Generated: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
    "",
    "",
    format_section(creed_details, "Dataset Details"),
    "",
    format_section(creed_relevance, "Relevance Criteria"),
    "",
    format_section(creed_reliability, "Reliability Criteria")
  )

  writeLines(content, file_path)
  invisible(NULL)
}
