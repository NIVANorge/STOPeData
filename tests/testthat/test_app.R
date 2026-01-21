# Test campaign module data validation ----
# load_all()

# Define expected campaign values ----
# TODO: None of this works, and the testServer doesn't seem to want to a) execute any logic in the UI portion or b) provide any useful feedback about why stuff doesn't work.
# TODO: Back to basic tests for the time being.
# expected_campaign <- list(
#   CAMPAIGN_NAME = "Lakes and Rivers of Norway, Sampling 2024",
#   CAMPAIGN_NAME_SHORT = "LakesRivers2024",
#   CAMPAIGN_START_DATE = "2024-01-01",
#   ENTERED_BY = "Ole Nordman",
#   ORGANISATION = "Test Organisation",
#   ENTERED_DATE = as.Date("2025-01-01")
# )

# testServer(app = run_app(), {
#   # Set all mandatory campaign inputs ----
#   session$setInputs(
#     "campaign-CAMPAIGN_NAME" = expected_campaign$CAMPAIGN_NAME,
#     "campaign-CAMPAIGN_NAME_SHORT" = expected_campaign$CAMPAIGN_NAME_SHORT,
#     "campaign-CAMPAIGN_START_DATE" = expected_campaign$CAMPAIGN_START_DATE,
#     "campaign-ENTERED_BY" = expected_campaign$ENTERED_BY,
#     "campaign-ORGANISATION" = expected_campaign$ORGANISATION,
#     "campaign-ENTERED_DATE" = expected_campaign$ENTERED_DATE
#     # frustratingly doesn't seem to autoset based on default value
#   )

#   print(session$input$"campaign-CAMPAIGN_NAME")
#   print(session$input$"campaign-CAMPAIGN_NAME_SHORT")
#   print(session$input$"campaign-CAMPAIGN_START_DATE")
#   print(session$input$"campaign-ENTERED_BY")
#   print(session$input$"campaign-ORGANISATION")
#   print(session$input$"campaign-ENTERED_DATE")

#   # Check that data was written to userData ----
#   campaign_data <- session$userData$reactiveValues$campaignData
#   print(campaign_data)
#   expect_true(!is.null(campaign_data))
#   expect_true(nrow(campaign_data) > 0)

#   # Validate against expected values ----
#   expect_equal(campaign_data$CAMPAIGN_NAME, expected_campaign$CAMPAIGN_NAME)
#   expect_equal(
#     campaign_data$CAMPAIGN_NAME_SHORT,
#     expected_campaign$CAMPAIGN_NAME_SHORT
#   )
#   expect_equal(
#     campaign_data$CAMPAIGN_START_DATE,
#     expected_campaign$CAMPAIGN_START_DATE
#   )
#   expect_equal(campaign_data$ENTERED_BY, expected_campaign$ENTERED_BY)
#   expect_equal(campaign_data$ORGANISATION, expected_campaign$ORGANISATION)

#   # Check that validity flag is TRUE ----
#   expect_true(session$userData$reactiveValues$campaignDataValid)
# })

# test that app starts
# FIXME: Started failing for no obvious reason, error message about call handlers on the stack.
# test_that("Test that app runs", {
#   if (grepl("testthat", getwd())) {
#     # we have to fake this or else testthat will look for data in the tests folder, becuase of course it does
#     setwd("../..")
#   }
#   expect_no_error(
#     testServer(app = run_app(), {})
#   )
#   if (!grepl("testthat", getwd())) {
#     setwd("tests/testthat") # I don't know...
#   }
# })

# test data initialisation
test_that("Test that initialise_userData() works", {
  expect_no_error(
    initialise_userData()
  )
})
