# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

STOPeData ("eData") is an R/Shiny application, built as a [golem](https://thinkr-open.github.io/golem/) package, that guides users through extracting, formatting, and quality-scoring environmental exposure data (e.g. chemical concentrations in environmental matrices) from papers/reports. It's one half of NIVA's Source to Outcome Pathway/Risk Assessment Database (RADB) family; its data schema and vocabularies live in a separate companion package, **eDataDRF** (`NIVANorge/eDataDRF`, installed via `Remotes:` in DESCRIPTION — not part of this repo).

## Common commands

Run these from an R session with the working directory set to the project root (there is no CLI test runner; everything goes through R/devtools).

```r
# Install/refresh dependencies
pak::pak("NIVANorge/eDataDRF")   # companion schema package must be installed
pak::local_install_deps()        # or source dependencies.R for the full list

# Run the app locally (hot-reload dev mode)
golem::run_dev()

# Run the full test suite
devtools::test()

# Run a single test file
devtools::test(filter = "sites")          # matches tests/testthat/test_sites.R
testthat::test_file("tests/testthat/test_llm.R")

# Regenerate NAMESPACE / Rd docs from roxygen comments (required after changing @import/@export)
devtools::document()

# Full R CMD check (mirrors CI)
devtools::check()

# Update DESCRIPTION Imports from code (golem convention, see dev/02_dev.R)
attachment::att_amend_desc()

# Test coverage (mirrors .github/workflows/test-coverage.yaml)
covr::package_coverage()
```

Docker: `docker-compose.yml` / `Dockerfile` build a `rocker/shiny` image that installs deps via `pak` and runs `shiny::runApp('app.R', host='0.0.0.0', port=3838)`.

CI (`.github/workflows/`): `R-CMD-check.yaml` runs `R CMD check` on macOS/Windows/Ubuntu (release/devel/oldrel); `test-coverage.yaml` runs `covr` and uploads to Codecov; `pkgdown.yaml` builds the docs site; `docker-build-push.yml` builds/pushes the container image.

## Architecture

### golem structure

Standard golem layout: `R/app_ui.R` / `R/app_server.R` define the top-level UI/server, `R/run_app.R` calls `shiny::shinyApp()` via `golem::with_golem_options()`, and `dev/` holds the (non-shipped, `.Rbuildignore`d) golem workflow scripts (`01_start.R`, `02_dev.R`, `03_deploy.R`). `app.R` at the repo root is the deployment entrypoint. Modules follow golem's `mod_*` naming: `mod_<name>_ui()` / `mod_<name>_server()`, with helpers split into `mod_<name>_fct_*.R` files. `fct_*.R` / `utils_*.R` hold cross-module helper functions.

### One navbar, one linear workflow

`app_ui()` (`R/app_ui.R`) builds a single `bslib::page_navbar()` where each `nav_panel` is a pipeline stage backed by its own module: Landing → LLM Extraction → Campaign → References → Sites → Parameters → Compartments → Methods → Samples → Biota → Data → Review → CREED (quality scoring) → Zenodo (upload). `app_server()` (`R/app_server.R`) defines `module_order`, a character vector of nav panel `value`s, and uses it to drive the Previous/Next footer buttons via `updateNavbarPage()`.

### Centralized reactive state: `session$userData$reactiveValues`

All cross-module state lives in one `reactiveValues` object initialised once per session by `initialise_userData()` in `app_server()`, not in individual module return values. Every data domain (sites, parameters, compartments, references, campaign, methods, samples, biota, measurements) is stored as a tibble plus a matching `*DataValid` boolean flag, always initialised via a matching `initialise_*_tibble()` function so the "does data exist" check is always `nrow(x) > 0` regardless of data source. Modules read/write this object directly (e.g. `session$userData$reactiveValues$sitesData <- ...`) rather than passing data through module return values — expect to grep this object's fields across many files when tracing a data flow. `metaData` on the same object accumulates session/extraction provenance (model used, token costs, timestamps) for later export.

Data reaches these tibbles from three paths that all populate a separate `*DataLLM` staging tibble first: (1) manual entry in each module's `rhandsontable`, (2) the LLM extraction module populating `*DataLLM` fields which downstream modules merge in, or (3) importing a previously-exported session ZIP (`import_session_from_zip()`).

### LLM extraction pipeline (`R/mod_llm.R`, `R/mod_llm_fct_*.R`)

The Extraction tab uses [`ellmer`](https://ellmer.tidyverse.org/) to run structured extraction against a user-uploaded PDF, supporting Anthropic/OpenAI/Google as interchangeable providers via `provider_options` (a lookup table of env var name, `ellmer` chat function, and curated model list per provider — update this list manually as providers release new models). Two modes: "Screen PDF" (cheap relevance/reliability triage) and "Extract Data" (full structured extraction), both driven by `extraction_schema_components` so the user can extract a subset of the schema to save cost.

Extraction runs off the main process via `mirai`: `app_server.R` starts a single `mirai::daemons(1)` background process at startup and uses `mirai::everywhere()` to source `mod_llm_fct_extract.R` and load its required packages into that worker, so the actual `chat$chat_structured()` call in `extract_pdf_with_llm()` doesn't block the Shiny UI. The task is wired through a `shiny::ExtendedTask`, with a captured `current_mirai` reference kept solely so `input$cancel_extraction` can call `stop_mirai()` on it.

The extraction schema is built with `ellmer`'s `type_object()`/`type_array()`/`type_string()` etc. (see `mod_llm_fct_extraction_schema.R`, one `create_*_schema()` function per data domain) and combined with a prompt template read from `inst/app/www/md/extraction_prompt.md`. Results land in `moduleState$structured_data`, then an observer keyed on `moduleState$llm_status == "successful"` fans them out into the `*DataLLM` reactive fields, one `tryCatch`-wrapped block per data domain so one bad section doesn't block the rest.

### CREED quality scoring (`R/mod_CREED*.R`)

Implements the CREED framework (Relevance / Reliability criteria, Required vs Recommended, Silver/Gold levels) for rating the quality of an assembled dataset. `mod_CREED_gateway.R`/`mod_CREED_relevance.R`/`mod_CREED_reliability.R`/`mod_CREED_details.R` collect scoring inputs; `mod_CREED_fct_summarise.R` and `mod_CREED_fct_helpers.R` compute the aggregate score/report from `session$userData$reactiveValues`, pulled in via the "Get Data from Modules" button (`creedGetData` counter used purely to trigger reactivity across nested modules).

### eDataDRF is the schema authority

Vocabulary lookups (e.g. `eDataDRF::parameters_vocabulary()`), tibble initialisers (`initialise_*_tibble()`), and format-conversion helpers used throughout `fct_download.R`, `fct_upload.R`, `fct_dummy_data.R`, and most `mod_*_fct_*.R` files come from the external `eDataDRF` package. When a column, vocabulary term, or format function is missing or needs changing, it likely needs to change in `eDataDRF`, not here — check there before assuming the bug is local.

### Export/import

`fct_download.R` / `fct_upload.R` handle the session-level "download all data as ZIP" / "import a previously exported ZIP" round-trip (file naming convention: `Campaign_Date_Module`, referenced in the import modal copy in `app_server.R`). `mod_export.R` exists but is currently disabled in `app_server()` (commented out) in favor of the download-all-modal flow built directly into `app_server.R`. `mod_zenodo.R` / `fct_zenodo.R` handle uploading the finished dataset to Zenodo (or the Zenodo Sandbox) via `zen4R`, using the `zenodo_licenses` package dataset (`R/data.R`, `data-raw/`) for license selection.

### Auth

User identity comes from an upstream auth proxy header (`session$request$HTTP_X_AUTH_REQUEST_EMAIL`), not from any login UI in this app — see the observer at the bottom of `app_server()`. Locally this header is absent, so `ENTERED_BY` falls back to manual entry (defaulting to `Sys.getenv("EDATA_USERNAME")`).

## Testing conventions

Tests use `testthat` edition 3 (`Config/testthat/edition: 3`). Server-module logic is generally tested by calling module helper functions directly against a fake session built with `create_dummy_session_data()` (`R/fct_dummy_data.R`) rather than `shiny::testServer()` — e.g. `test_sites.R` sets `testSession$userData$reactiveValues <- create_dummy_session_data()` and calls functions like `create_new_site()` directly, then asserts against `initialise_*_tibble()` templates for column/type parity.

## Secrets

API keys/tokens (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `GOOGLE_API_KEY`, `ZENODO_TOKEN`, `ZENODO_SANDBOX_TOKEN`) are read from environment variables (`.Renviron`, gitignored) and/or entered by the user directly into the Extraction tab's password field — never hardcode these or commit `.Renviron`.
