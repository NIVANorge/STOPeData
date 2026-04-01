# Package index

## Summarise

Summarise session data into text strings for CREED quality assessment
reporting.

- [`summarise_CREED_details()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_details.md)
  : summarise user-entered data for the CREED dataset details reporting
- [`summarise_CREED_relevance()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_relevance.md)
  : summarise user-entered data for the CREED relevance criteria
  reporting
- [`summarise_CREED_reliability()`](https://nivanorge.github.io/STOPeData/reference/summarise_CREED_reliability.md)
  : summarise user-entered data for the CREED reliability criteria
  reporting
- [`summarise_biota()`](https://nivanorge.github.io/STOPeData/reference/summarise_biota.md)
  : Summarise biota data
- [`summarise_compartments()`](https://nivanorge.github.io/STOPeData/reference/summarise_compartments.md)
  : Summarise compartments data
- [`summarise_date_range()`](https://nivanorge.github.io/STOPeData/reference/summarise_date_range.md)
  : Calculate Date Range
- [`summarise_lod_loq()`](https://nivanorge.github.io/STOPeData/reference/summarise_lod_loq.md)
  : Summarise LOD/LOQ data
- [`summarise_measured_units()`](https://nivanorge.github.io/STOPeData/reference/summarise_measured_units.md)
  : Generate Units Summary by Parameter
- [`summarise_multiple()`](https://nivanorge.github.io/STOPeData/reference/summarise_multiple.md)
  : Summarise Multiple Values
- [`summarise_protocols()`](https://nivanorge.github.io/STOPeData/reference/summarise_protocols.md)
  : Summarise protocols data
- [`summarise_reference()`](https://nivanorge.github.io/STOPeData/reference/summarise_reference.md)
  : Summarise mod_reference data into a single string
- [`summarise_sig_figs()`](https://nivanorge.github.io/STOPeData/reference/summarise_sig_figs.md)
  : Summarise significant figures in numeric column
- [`summarise_sites()`](https://nivanorge.github.io/STOPeData/reference/summarise_sites.md)
  : Summarise sites data
- [`summarise_uncertainty_comments()`](https://nivanorge.github.io/STOPeData/reference/summarise_uncertainty_comments.md)
  : Summarise uncertainty and measurement comments
- [`calculate_coordinate_precision()`](https://nivanorge.github.io/STOPeData/reference/calculate_coordinate_precision.md)
  : Calculate coordinate precision
- [`manual_completion_message()`](https://nivanorge.github.io/STOPeData/reference/manual_completion_message.md)
  : Return manual completion message

## Validate

Validate data formats and structures.

- [`validate_and_lookup_identifier()`](https://nivanorge.github.io/STOPeData/reference/validate_and_lookup_identifier.md)
  : Validate and lookup DOI/PMID with comprehensive error handling
- [`validate_and_parse_bibtex()`](https://nivanorge.github.io/STOPeData/reference/validate_and_parse_bibtex.md)
  : Validate and parse BibTeX string with error handling
- [`validate_dataset_structure()`](https://nivanorge.github.io/STOPeData/reference/validate_dataset_structure.md)
  : Validate dataset structure
- [`validate_doi_format()`](https://nivanorge.github.io/STOPeData/reference/validate_doi_format.md)
  : Validate DOI format
- [`validate_pmid_format()`](https://nivanorge.github.io/STOPeData/reference/validate_pmid_format.md)
  : Validate PMID format

## Zenodo

Helpers for preparing and submitting datasets to Zenodo.

- [`zenodo_licenses`](https://nivanorge.github.io/STOPeData/reference/zenodo_licenses.md)
  : Zenodo license options tibble
- [`zenodo_resource_types`](https://nivanorge.github.io/STOPeData/reference/zenodo_resource_types.md)
  : Zenodo resource type options vector
- [`generate_zenodo_readme()`](https://nivanorge.github.io/STOPeData/reference/generate_zenodo_readme.md)
  : Generate a Zenodo submission README

## Create

Create new data rows and structures.

- [`create_compartment_combination()`](https://nivanorge.github.io/STOPeData/reference/create_compartment_combination.md)
  : Create new compartment combination row
- [`create_dummy_session_data()`](https://nivanorge.github.io/STOPeData/reference/create_dummy_session_data.md)
  : Create Dummy Session Data
- [`create_existing_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_existing_parameter.md)
  : Create parameter row from existing parameter data
- [`create_metadata_tibble()`](https://nivanorge.github.io/STOPeData/reference/create_metadata_tibble.md)
  : Create metadata tibble
- [`create_new_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_new_parameter.md)
  : Create new blank parameter row
- [`create_new_site()`](https://nivanorge.github.io/STOPeData/reference/create_new_site.md)
  : Create a new site record

## Data Export

Build and download session data as ZIP archives and metadata files.

- [`write_metadata_txt()`](https://nivanorge.github.io/STOPeData/reference/write_metadata_txt.md)
  : Create readable metadata text file
- [`get_git_commit()`](https://nivanorge.github.io/STOPeData/reference/get_git_commit.md)
  : Get git commit hash
- [`get_export_metadata()`](https://nivanorge.github.io/STOPeData/reference/get_export_metadata.md)
  : Get export metadata
- [`get_dataset_display_name()`](https://nivanorge.github.io/STOPeData/reference/get_dataset_display_name.md)
  : Get dataset display name
- [`object_to_text()`](https://nivanorge.github.io/STOPeData/reference/object_to_text.md)
  : Convert object to human-readable text
- [`build_session_zip()`](https://nivanorge.github.io/STOPeData/reference/build_session_zip.md)
  : Build a session ZIP archive
- [`download_all_data()`](https://nivanorge.github.io/STOPeData/reference/download_all_data.md)
  : Download all data as CSV and TXT files in a ZIP archive
- [`write_creed_report_txt()`](https://nivanorge.github.io/STOPeData/reference/write_creed_report_txt.md)
  : Write CREED report as human-readable text file

## Data Import

Import session data from exported ZIP archives.

- [`read_zip_metadata()`](https://nivanorge.github.io/STOPeData/reference/read_zip_metadata.md)
  : Read metadata from ZIP file
- [`import_session_from_zip()`](https://nivanorge.github.io/STOPeData/reference/import_session_from_zip.md)
  : Import data from exported ZIP file
- [`import_module_table()`](https://nivanorge.github.io/STOPeData/reference/import_module_table.md)
  : Import module dataset from CSV
- [`detect_dataset_type()`](https://nivanorge.github.io/STOPeData/reference/detect_dataset_type.md)
  : Detect dataset type from filename
- [`get_reactiveValues_key()`](https://nivanorge.github.io/STOPeData/reference/get_reactiveValues_key.md)
  : Get reactiveValues key for dataset type
- [`read_metadata_txt()`](https://nivanorge.github.io/STOPeData/reference/read_metadata_txt.md)
  : Read metadata from text file

## Reference Management

Parse BibTeX, look up DOIs and PMIDs, and manage bibliographic
references.

- [`bib_string2df_alt()`](https://nivanorge.github.io/STOPeData/reference/bib_string2df_alt.md)
  : Convert BibTeX string to data frame using temporary file
- [`map_bibtex_to_reference_fields()`](https://nivanorge.github.io/STOPeData/reference/map_bibtex_to_reference_fields.md)
  : Map BibTeX fields to reference input fields
- [`map_crossref_to_reference_fields()`](https://nivanorge.github.io/STOPeData/reference/map_crossref_to_reference_fields.md)
  : Map Crossref data to reference input fields
- [`clean_bibtex_text()`](https://nivanorge.github.io/STOPeData/reference/clean_bibtex_text.md)
  : Clean BibTeX text formatting
- [`extract_clean_doi()`](https://nivanorge.github.io/STOPeData/reference/extract_clean_doi.md)
  : Extract clean DOI from input string
- [`extract_clean_pmid()`](https://nivanorge.github.io/STOPeData/reference/extract_clean_pmid.md)
  : Extract clean PMID from input string
- [`pmid_to_doi()`](https://nivanorge.github.io/STOPeData/reference/pmid_to_doi.md)
  : Convert PMID to DOI using PubMed API
- [`lookup_crossref_doi()`](https://nivanorge.github.io/STOPeData/reference/lookup_crossref_doi.md)
  : Lookup publication data using Crossref
- [`validate_and_lookup_identifier()`](https://nivanorge.github.io/STOPeData/reference/validate_and_lookup_identifier.md)
  : Validate and lookup DOI/PMID with comprehensive error handling
- [`reference_character_limits()`](https://nivanorge.github.io/STOPeData/reference/reference_character_limits.md)
  : Reference field character limits

## Data Setup

Look up and generate IDs for parameters, samples, and protocols.

- [`get_parameters_of_types()`](https://nivanorge.github.io/STOPeData/reference/get_parameters_of_types.md)
  : Get parameter names for a specific parameter type
- [`get_parameters_filtered()`](https://nivanorge.github.io/STOPeData/reference/get_parameters_filtered.md)
  : Get parameter names filtered by type and optionally subtype

## CREED Utilities

Configuration and helper functions for CREED quality assessment
criteria.

- [`collect_CREED_data()`](https://nivanorge.github.io/STOPeData/reference/collect_CREED_data.md)
  : Collect CREED Scores from Input
- [`CREED_reliability_criteria_config()`](https://nivanorge.github.io/STOPeData/reference/CREED_reliability_criteria_config.md)
  : CREED Reliability Criteria Configuration
- [`CREED_relevance_criteria_config()`](https://nivanorge.github.io/STOPeData/reference/CREED_relevance_criteria_config.md)
  : CREED Relevance Criteria Configuration
- [`copper_CREED_purpose_statement()`](https://nivanorge.github.io/STOPeData/reference/copper_CREED_purpose_statement.md)
  : Load copper CREED purpose statement

## Testing

Generate dummy data for development and testing.

- [`populate_session_with_dummy_data()`](https://nivanorge.github.io/STOPeData/reference/populate_session_with_dummy_data.md)
  : Populate session data directly with dummy data
- [`creed_tibble_to_mock_input()`](https://nivanorge.github.io/STOPeData/reference/creed_tibble_to_mock_input.md)
  : Convert CREED Tibble to Mock Input List

## Utilities

General-purpose helpers for the Shiny app.

- [`printreactiveValues()`](https://nivanorge.github.io/STOPeData/reference/printreactiveValues.md)
  : Print content of reactiveValues object
- [`info_accordion()`](https://nivanorge.github.io/STOPeData/reference/info_accordion.md)
  : Create a collapsible single-panel accordion containing a markdown
  file
- [`get_session_data_safe()`](https://nivanorge.github.io/STOPeData/reference/get_session_data_safe.md)
  : Safely retrieve data from session reactive values

## App

Run the STOPeData Shiny application.

- [`run_app()`](https://nivanorge.github.io/STOPeData/reference/run_app.md)
  : Run the Shiny Application
