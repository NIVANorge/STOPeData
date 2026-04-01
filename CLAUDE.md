# STOPeData — Developer Notes for Claude Code

## What This Is

STOPeData is a **golem-based R Shiny app** for entering, formatting, and quality-assessing environmental exposure data (chemical concentrations in environmental matrices). It is part of the NIVA STOP/RADB family of apps. Data is formatted against the **eDataDRF** schema and can be uploaded to Zenodo.

Key workflows:
1. Manual data entry across ~10 data-entry modules
2. LLM-assisted extraction from PDFs (Claude API via `ellmer`)
3. CREED data quality assessment (structured scoring across 4 dimensions)
4. Export and Zenodo upload

Run locally with `golem::run_dev()`. Needs `ANTHROPIC_API_KEY`, `ZENODO_TOKEN`, and `ZENODO_SANDBOX_TOKEN` in `.Renviron` for full functionality (app runs without them, LLM/Zenodo features fail gracefully).

---

## Architecture

### State Management ("stratégie du petit r")

All app data lives in a single `reactiveValues` object stored in `session$userData$reactiveValues`. It is initialised by `initialise_userData()` in `R/app_server.R`.

```r
# Access pattern throughout the app:
session$userData$reactiveValues$sitesData
session$userData$reactiveValues$sitesDataValid
```

**Key conventions:**
- Every data entry module has a `*Data` tibble and a `*DataValid` boolean flag
- All data is stored as tibbles (even single-row items like campaign/reference) for consistency
- Presence check: `!is.null(data) && nrow(data) != 0`
- LLM-extracted versions have `*DataLLM` equivalents (e.g. `sitesDataLLM`)

**Known limitation:** List assignment creates new keys silently — typos in flag names create new flags rather than erroring. An R6-based version exists on a separate branch but is not current.

### Module Organisation

Modules follow the golem naming convention. Each feature typically has:

| File pattern | Purpose |
|---|---|
| `mod_<feature>.R` | UI + server for the module |
| `mod_<feature>_fct_*.R` | Pure business logic functions (testable) |
| `fct_*.R` | Package-level utility functions |
| `utils_*.R` | Internal utility helpers |

Module servers are instantiated in `R/app_server.R`. Data flows in/out via `session$userData$reactiveValues`, not return values.

**Current data-entry modules:** campaign, references, sites, parameters, compartments, methods, samples (+ biota), measurements

**Analysis/output modules:** CREED (purpose, relevance, reliability, details, gateway, scores), LLM extraction, export, Zenodo, review, data, landing

### Async Processing

`mirai` is used for background LLM extraction (`mod_llm_fct_extract.R`). Daemons are initialised at the top of `app_server.R` (outside the server function — intentional) and torn down on `onStop`.

---

### UI/Shiny

Prefer bslib functions for UI and modern shiny.
- Use `input_task_button()` rather than `actionButton()`
- Use `observe() |> bindEvent()` rather than `observeEvent()`
- All `bindEvent()` calls should be labelled with a short, descriptive name
- Observers should use `tryCatch()` to catch warnings/errors and return them at the UI level, with observer label, via `showNotifications()` + `glue()` 

## Key Dependencies

| Package | Role |
|---|---|
| `eDataDRF` | Data schema and tibble initialisation functions (`initialise_*_tibble()`) — NIVA internal package |
| `ellmer` | Claude API client for LLM extraction |
| `golem` | App framework (structure, config, dev tools) |
| `zen4R` | Zenodo API for dataset upload/download |
| `rcrossref` | Reference metadata lookup by DOI |
| `mirai` | Async background processing |
| `shinyvalidate` | Input validation in modules |
| `rhandsontable` | Editable data tables in UI |
| `bslib` | Bootstrap 5 theming |

`eDataDRF` must be installed from GitHub (`NIVANorge/eDataDRF`). It provides the canonical tibble structures and the DRF (Data Resource Format) schema.

---

## Testing

### What Is Tested

| Area | Coverage | Files |
|---|---|---|
| CREED logic | Good | `test_creed.R`, `test_creed_helpers.R`, `test_creed_summarise.R` |
| Parameters | Good | `test_parameters.R` |
| Zenodo | Moderate | `test_zenodo.R` |
| Sites | Sparse | `test_sites.R` |
| LLM extraction | Sparse | `test_llm.R` |
| Samples, References, Protocols, Utils | None/empty | respective test files |

### Testing Approach

- **Unit tests on `fct_` functions** — the primary testing strategy. Business logic extracted into pure functions is testable outside a Shiny session.
- **`testServer()` for module logic** — works for reactive/observer logic but **does not execute UI-side code** (inputs, outputs that depend on render functions). Several tests were abandoned due to this. Don't invest heavily here.
- **`initialise_userData()`** is the key testing helper — call it to get a valid mock of the app state without needing a real session.
- use **`example_*_data()`** functions from `eDataDRF` to provide consistent example data for functions that take user data as inputs

```r
# Pattern for testing with app state:
test_that("something works", {
  ud <- initialise_userData()
  ud$sitesData <- <some test data>
  result <- my_fct_function(ud$sitesData)
  expect_equal(result, expected)
})
```

Run tests with `devtools::test()`. CI runs on GitHub Actions.

---

## Common Gotchas

- `mirai::everywhere(source("R/mod_llm_fct_extract.R"))` at the top of `app_server.R` must source the extract file into daemon processes — this means the file must be self-contained (all imports explicit).
- `session$userData$reactiveValues` is only available inside a Shiny session. Outside tests, use `initialise_userData()` to get the plain list equivalent.
- `samplesDataWithBiota` is a derived tibble (samples joined with biota data) — it is not entered directly, it's computed.
- The `creedGetData` and `creedCalculateScores` integers in `userData` are manual reactivity triggers (increment to fire observers). Acknowledged as a code smell.
- `mod_data_server` and `mod_export_server` both receive `parent_session = session` because `updateNavbarPage()` requires the parent session, not the module session.

---

## File Map (R/)

```
app_server.R          — Main server, userData init, module wiring
app_ui.R              — Top-level UI, navbar structure
run_app.R             — Entry point: run_app() and golem::run_dev()
app_config.R          — golem config helpers
data.R                — Roxygen docs for package data objects

mod_<feature>.R       — Module UI + server (25+ modules)
mod_<feature>_fct_*.R — Testable business logic for that module
fct_*.R               — Shared business logic (upload, download, zenodo, dummy data)
utils_*.R             — Internal utilities (get_data_safe, general utilities)
```

---

## Roadmap (Known TODOs)

- Better test architecture — primary gap is untested modules (samples, references, protocols, utils)
- More user-friendly session saving/loading
- Data extraction from structured formats (Excel, API calls)
- Connection to RADB for long-term storage and lookups
- R6-based state management (branch exists, not current)
