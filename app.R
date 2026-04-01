# Launch the shinyApp. Called by Dockerfile.

logger::log_appender(logger::appender_stdout)
logger::log_layout(logger::layout_json())
logger::log_threshold(logger::INFO)

logger::log_messages()
logger::log_warnings()

pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE,
  quiet = TRUE
)
options(shiny.maxRequestSize = 20 * 1024^2) # increase permitted file upload size
options(golem.app.prod = FALSE)
options(bslib.color_contrast_warnings = FALSE)
STOPeData::run_app() # add parameters here (if any)
