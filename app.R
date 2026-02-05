# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# If we're not on Rstudio, install the app package from Github
# (otherwise it searches CRAN and crashes...)

# THIS IS HOW WE RUN THE APP IN THE CONTAINER
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
options(golem.app.prod = FALSE) # TODO: make a comment on what this actually does?
options(bslib.color_contrast_warnings = FALSE)
STOPeData::run_app() # add parameters here (if any)
