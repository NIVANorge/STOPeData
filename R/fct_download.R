# Metadata Format Helper Functions ----
# Functions to create and read human-readable metadata files

#' Create readable metadata text file
#'
#' @description Create a human-readable text file with export metadata
#' @param metadata_list List containing metadata fields (campaign_name, export_datetime, etc.)
#' @param file_path Character. Path where to write the metadata file
#' @return NULL (invisibly). File is written to disk as a side effect.
#' @importFrom glue glue
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
    glue("  Client Data: {metadata_list$clientData}"),
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
#' @description Gather metadata about the current export session
#' @param session Shiny session object. Required to access user data and client information.
#' @return List containing export metadata fields (campaign_name, export_datetime, app_name, etc.)
#' @export
get_export_metadata <- function(session = NULL) {
  if (is.null(session)) {
    stop("session reactive object must be supplied to create CSVs")
  }
  rv <- session$userData$reactiveValues

  list(
    campaign_name = rv$campaignData$CAMPAIGN_NAME,
    export_datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    app_name = "STOPeData",
    llm_version = "claude-sonnet-4-20250514", # set manually in mod_llm_fct_extract
    clientData = paste(
      sep = "",
      "protocol: ",
      session$clientData$url_protocol,
      "\n",
      "hostname: ",
      session$clientData$url_hostname,
      "\n",
      "pathname: ",
      session$clientData$url_pathname,
      "\n",
      "port: ",
      session$clientData$url_port,
      "\n",
      "search: ",
      session$clientData$url_search,
      "\n"
    ),
    app_version = get_golem_version() %||% "Version not available",
    user = rv$ENTERED_BY %||% "Unknown user"
  )
}

#' Create metadata tibble
#'
#' @description Convert metadata list to tibble format suitable for Excel sheets
#' @param metadata_list List containing metadata fields
#' @return Tibble with Property and Value columns
#' @importFrom tibble tibble
#' @export
create_metadata_tibble <- function(metadata_list) {
  tibble(
    Property = names(metadata_list),
    Value = as.character(unlist(metadata_list))
  )
}

#' Get dataset display name
#'
#' @description Convert internal dataset names to user-friendly display names
#' @param dataset_name Character. Internal name of the dataset (e.g., "sitesData")
#' @return Character. User-friendly display name (e.g., "Sites")
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
    creedRelevance = "CREED_RV",
    creedReliability = "CREED_RB",
    creedScores = "CREED_Score"
  )

  display_names[[dataset_name]] %||% dataset_name
}

# Function: Check Available Datasets ----
#' Check which datasets contain data and get their dimensions
#'
#' @param rv standard reactiveValues object from the app
#'
#' @return List with three elements:
#'   - available_datasets: character vector of dataset names with data
#'   - dataset_dimensions: named list of lists with rows/cols for each available dataset
#'   - export_ready: logical indicating if any datasets are available
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

# Function: Extract Campaign Name ----
#' Extract campaign name from campaign data for use in filenames
#'
#' @param campaign_data Data frame containing campaign information
#'
#' @return Character string of campaign name, or NULL if not found/invalid
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

# just in one place for easier reuse
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

# just in one place for easier reuse
downloadable_text_datasets <- function() {
  c("schemaLLM", "promptLLM", "rawLLM")
}


#' Download all data as CSV and TXT files in a ZIP archive
#'
#' @description Creates a Shiny downloadHandler that exports all available datasets
#'   as CSV files (for tabular data) or TXT files (for text/object data) in a single ZIP archive.
#'   Also includes a metadata file with export information.
#' @param session Shiny session object. Required to access reactive values.
#' @param moduleState ReactiveValues object containing export_ready flag, available_datasets,
#'   and campaign_name fields.
#' @return A Shiny downloadHandler function
#' @importFrom glue glue
#' @importFrom zip zip
#' @importFrom readr write_excel_csv
#' @export
download_all_data <- function(session, moduleState = NULL) {
  if (is.null(moduleState) || is.null(session)) {
    stop(
      "moduleState & session reactive objects must be supplied to create CSVs"
    )
  }

  campaign_short <- function() {
    if (nrow(session$userData$reactiveValues$referenceData) > 0) {
      generate_reference_id(
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

    content = function(file) {
      rv <- session$userData$reactiveValues
      metadata <- get_export_metadata(session = session)

      # Setup temporary directory and naming
      temp_dir <- tempdir()
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      campaign <- campaign_short()

      # Define which datasets should be exported as text files
      text_datasets <- c("schemaLLM", "promptLLM", "rawLLM")

      all_files <- character(0)

      # Export each dataset ----
      for (dataset_name in moduleState$available_datasets) {
        tryCatch(
          {
            if (stringr::str_detect(dataset_name, "CREED")) {}
            print_dev(glue("prepping {dataset_name} for export"))

            data <- rv[[dataset_name]]

            display_name <- gsub(
              " ",
              "_",
              get_dataset_display_name(dataset_name)
            )
            base_name <- glue("{campaign}_{display_name}_{timestamp}")

            if (dataset_name %in% text_datasets) {
              # Handle text/object data
              if (!is.null(data)) {
                txt_file <- file.path(temp_dir, glue("{base_name}.txt"))

                # Convert object to text using helper function
                text_content <- object_to_text(
                  data,
                  dataset_name = display_name
                )
                writeLines(text_content, txt_file)

                all_files <- c(all_files, txt_file)
              }
            } else {
              # Handle tabular data
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
                " (Code: download_all_data())"
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
          rv$uploaded_pdf_filename %||%
            glue("{campaign}_MS.pdf")
        )
        file.copy(rv$pdfPath, pdf_dest)
        all_files <- c(all_files, pdf_dest)
      }

      # Write metadata file ----
      combined_metadata_file <- file.path(
        temp_dir,
        glue("{campaign}_export_metadata_{timestamp}.txt")
      )
      write_metadata_txt(metadata, combined_metadata_file)
      all_files <- c(all_files, combined_metadata_file)

      # Create and cleanup ZIP archive ----
      zip(
        zipfile = file,
        files = all_files,
        mode = "cherry-pick"
      )

      unlink(all_files)
    },

    contentType = "application/zip"
  )
}

#' Write CREED report as human-readable text file
#'
#' @description Merge CREED details, relevance, and reliability tibbles into
#'   a single human-readable text file with section headers
#' @param creed_details Tibble with field/value columns from summarise_CREED_details()
#' @param creed_relevance Tibble with field/value columns from summarise_CREED_relevance()
#' @param creed_reliability Tibble with field/value columns from summarise_CREED_reliability()
#' @param file_path Character. Path where to write the report file
#' @return NULL (invisibly). File is written to disk as a side effect.
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
