# Build a session ZIP archive

Writes all available session datasets to a ZIP file at \`dest_file\`.
Includes tabular data as CSV, text/object data as TXT, the source PDF
(if present), and a metadata file. Called by both
\`download_all_data()\` and the Zenodo module's session-upload mode.

## Usage

``` r
build_session_zip(session, moduleState, dest_file)
```

## Arguments

- session:

  Shiny session object. Required to access reactive values.

- moduleState:

  ReactiveValues object containing \`available_datasets\` and
  \`campaign_name\` fields.

- dest_file:

  Character. Path where the ZIP file should be written.

## Value

\`dest_file\` invisibly.
