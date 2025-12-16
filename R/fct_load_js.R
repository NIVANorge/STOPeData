# Rhandsontable requires us to write renderers in JS. I've put them here to avoid
# big blocks of unstyled text strings in my nice clean modules

#' Mandatory Column Renderer - Full Highlighting - Text
#'
#' @description Returns a JavaScript renderer function that highlights mandatory
#'   columns with full background colors. Missing data gets yellow background,
#'   valid data gets green background. TEXT fields only.
#'
#' @return A character string containing JavaScript function code for handsontable renderer
#'
#' @details This renderer applies aggressive highlighting to clearly distinguish
#'   between missing and valid data in mandatory columns. Use for high-priority
#'   data validation scenarios.
#'
#' @examples
#' \dontrun{
#' rhandsontable(data) |>
#'   hot_col("mandatory_column", renderer = mandatory_highlight_text())
#' }
#'
#' @noRd
mandatory_highlight_text <- function() {
  "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    const isEmpty = value === null ||
      value === undefined ||
        value === '' ||
          (typeof value === 'string' && value.trim() === '');

    td.style.boxSizing = 'border-box';

    if (isEmpty) {
      td.className = 'htAutocomplete';
      td.style.borderBottom = '2px solid #f9b928';
    } else {
      td.style.borderBottom = '2px solid #007416';
      td.className = 'htAutocomplete';
    }
    return td;
  }"
}
#' Mandatory Column Renderer - Full Highlighting - Dropdowns
#'
#' @description Returns a JavaScript renderer function that highlights mandatory
#'   columns with full background colors. Missing data gets yellow background,
#'   valid data gets green background. DROPDOWN fields only.
#'
#' @return A character string containing JavaScript function code for handsontable renderer
#'
#' @details This renderer applies aggressive highlighting to clearly distinguish
#'   between missing and valid data in mandatory columns. Use for high-priority
#'   data validation scenarios.
#'
#' @examples
#' \dontrun{
#' rhandsontable(data) |>
#'   hot_col("mandatory_column", type = "dropdown", renderer = mandatory_highlight_dropdown())
#' }
#'
#' @noRd
mandatory_highlight_dropdown <- function() {
  "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.AutocompleteRenderer.apply(this, arguments);
    const isEmpty = value === null ||
      value === undefined ||
        value === '' ||
          (typeof value === 'string' && value.trim() === '');

              td.style.boxSizing = 'border-box';

        if (isEmpty) {
          td.className = 'htAutocomplete';
          td.style.borderBottom = '2px solid #f9b928';
        } else {
          td.style.borderBottom = '2px solid #007416';
          td.className = 'htAutocomplete';
        }
        return td;
  }"
}

#' Mandatory Column Renderer - Subtle Highlighting
#'
#' @description Returns a JavaScript renderer function that provides subtle
#'   highlighting for mandatory columns using colored left borders instead of
#'   full background colors.
#'
#' @return A character string containing JavaScript function code for handsontable renderer
#'
#' @details This renderer uses left border indicators (yellow for missing, green
#'   for valid) to provide visual feedback without overwhelming the interface.
#'   Suitable for dense data tables where full background highlighting might be
#'   too distracting.
#'
#' @examples
#' \dontrun{
#' rhandsontable(data) |>
#'   hot_col("mandatory_column", renderer = mandatory_highlight_subtle())
#' }
#'
#' @noRd
mandatory_highlight_subtle <- function() {
  "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    const isEmpty = value === null ||
      value === undefined ||
        value === '' ||
          (typeof value === 'string' && value.trim() === '');
        if (isEmpty) {
          td.style.background = '#fefefe';
          td.style.borderLeft = '4px solid #ffc107';
          td.style.color = '#6c757d';
        } else {
          td.style.background = '#ffffff';
          td.style.borderLeft = '4px solid #28a745';
          td.style.color = '#495057';
        }
        return td;
  }"
}
