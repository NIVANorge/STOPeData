# Populate session data directly with dummy data

Stores dummy data directly into session reactiveValues. This bypasses
the LLM extraction process and populates all module data objects
immediately.

## Usage

``` r
populate_session_with_dummy_data(
  session,
  navigate_to = NULL,
  parent_session = NULL
)
```

## Arguments

- session:

  Shiny session object

- navigate_to:

  Optional tab to navigate to after loading data

- parent_session:

  Parent session for navigation (if different from session)
