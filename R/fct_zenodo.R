# Zenodo Helper Functions and Static Data ----

#' Zenodo license options tibble
#'
#' @description Returns a tibble of Zenodo license options as of March 2026.
#' Includes `id`, `title`, `description`, `url`, `schema`, `osi_approved`, `revision_id`,
#' `created`, `updated`, and `popular`, a custom boolean highlighting the most commonly
#' used licenses.
#'
#' @importFrom tibble as_tibble
#' @export
zenodo_licenses <- readRDS("inst/extdata/clean/Zenodo_Licenses_2026_03.rds") |>
  tibble::as_tibble()

#' Zenodo resource type options vector
#'
#' @description Named character vector of Zenodo resource types.
#'   Names are display labels; values are Zenodo resource type identifiers.
#'   See \url{https://help.zenodo.org/docs/deposit/describe-records/resource-type/}
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

#' Generate a Zenodo submission README
#'
#' @description Produces a markdown-formatted README string suitable for
#'   attaching to a Zenodo record. Used both as the companion file uploaded
#'   with the dataset and as the preview shown in the production confirmation
#'   modal.
#'
#' @param title Character. Dataset title.
#' @param description Character. Dataset description.
#' @param authors List of lists, each with elements `first`, `last`,
#'   `affiliation`, and `orcid` (all character, may be empty strings).
#' @param license_id Character. Zenodo license identifier, e.g. `"cc-by-4.0"`.
#'   The human-readable title is looked up from `zenodo_licenses`.
#' @param contact_name Character. Display name or role for the contact.
#' @param contact_email Character. Contact email address.
#'
#' @return Character string containing the full README in markdown format.
#'
#' @importFrom glue glue
#' @export
generate_zenodo_readme <- function(
  title,
  description,
  authors,
  license_id,
  contact_name = "",
  contact_email = ""
) {
  current_year <- format(Sys.Date(), "%Y")

  # Human-readable license title ----
  license_display <- tryCatch(
    {
      match <- zenodo_licenses[
        zenodo_licenses$id == license_id,
        "title",
        drop = TRUE
      ]
      if (length(match) > 0 && nzchar(match[1])) match[1] else license_id
    },
    error = function(e) license_id
  )

  # Per-author markdown block ----
  author_lines <- vapply(
    seq_along(authors),
    function(i) {
      a <- authors[[i]]
      name <- paste(
        if (nzchar(a$first)) a$first else "[First]",
        if (nzchar(a$last)) a$last else "[Last]"
      )
      label <- if (i == 1) "Primary Author" else glue("Author {i}")
      parts <- c(
        glue("**{label}:** {name}"),
        if (nzchar(a$affiliation)) glue("  **Institution:** {a$affiliation}"),
        if (nzchar(a$orcid)) glue("  **ORCID:** {a$orcid}")
      )
      paste(parts, collapse = "\n")
    },
    character(1)
  )

  # "Smith J., Jones A." citation format ----
  citation_authors <- paste(
    vapply(
      authors,
      function(a) {
        ln <- if (nzchar(a$last)) a$last else "[Last]"
        fn <- if (nzchar(a$first)) substr(a$first, 1, 1) else ""
        if (nzchar(fn)) paste0(ln, " ", fn, ".") else ln
      },
      character(1)
    ),
    collapse = ", "
  )

  # Contact block ----
  contact_parts <- c()
  if (nzchar(contact_name)) {
    contact_parts <- c(contact_parts, glue("**Name:** {contact_name}"))
  }
  if (nzchar(contact_email)) {
    contact_parts <- c(contact_parts, glue("**Email:** {contact_email}"))
  }
  if (nzchar(authors[[1]]$orcid)) {
    contact_parts <- c(contact_parts, glue("**ORCID:** {authors[[1]]$orcid}"))
  }
  if (nzchar(authors[[1]]$affiliation)) {
    contact_parts <- c(
      contact_parts,
      glue("**Institution:** {authors[[1]]$affiliation}")
    )
  }
  contact_section <- if (length(contact_parts)) {
    paste(contact_parts, collapse = "\n- ")
  } else {
    "[Contact information]"
  }

  # Assemble named locals for glue interpolation ----
  title <- if (nzchar(title)) title else "[Dataset Title]"
  description <- if (nzchar(description)) description else "[Description]"
  author_block <- paste(author_lines, collapse = "\n\n")

  glue(
    "# {title}",
    "- **Zenodo DOI:** (Will be generated upon upload)",
    "",
    "---",
    "",
    "## What is this?",
    "",
    "{description}",
    "",
    "---",
    "",
    "## Authors",
    "",
    "{author_block}",
    "",
    "---",
    "",
    "## How to cite",
    "",
    "{citation_authors} ({current_year}). *{title}*. Zenodo. https://doi.org/[DOI-will-be-generated]",
    "",
    "---",
    "",
    "## License",
    "",
    "Released under **{license_display}**.",
    "",
    "---",
    "",
    "## Contact",
    "",
    "- {contact_section}",
    .sep = "\n"
  )
}
