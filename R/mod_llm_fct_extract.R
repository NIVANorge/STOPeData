# R/mod_llm_fct_api.R ----

#' Core async function for LLM extraction
#'
#' This function performs the actual PDF extraction using the Anthropic API.
#' It's designed to run inside a mirai() call for async execution.
#'
#' @param pdf_path Character. Path to the PDF file to extract data from.
#' @param api_key Character. Anthropic API key (should start with "sk-ant-").
#' @param extraction_prompt Character. The prompt text instructing the LLM
#'   on what to extract.
#' @param extraction_schema List or S7 class. The structured schema defining
#'   expected output format (e.g., S7 class with properties).
#' @param max_tokens Integer. Maximum tokens for the API response.
#'
#' @returns Named list with three elements:
#'   \describe{
#'     \item{result}{The structured extraction result from the LLM}
#'     \item{metadata}{List containing cost information or error details}
#'     \item{success}{Logical indicating if extraction succeeded}
#'   }
#'
#' @importFrom ellmer chat_anthropic params content_pdf_file
extract_pdf_with_llm <- function(
  pdf_path,
  api_key,
  extraction_prompt,
  extraction_schema,
  max_tokens
) {
  # Set API key ----
  Sys.setenv(ANTHROPIC_API_KEY = api_key)

  # Initialize chat ----
  chat <- chat_anthropic(
    model = "claude-sonnet-4-20250514",
    params = params(max_tokens = max_tokens)
  )

  # Prepare content ----
  pdf_content <- content_pdf_file(pdf_path)

  # Extract data (the blocking operation) ----
  result <- chat$chat_structured(
    extraction_prompt,
    pdf_content,
    type = extraction_schema
  )

  # Get cost info ----
  api_metadata <- tryCatch(
    list(total_cost = chat$get_cost(include = "all")),
    error = function(e) list(cost_error = e$message)
  )

  # Return results ----
  list(
    result = result,
    metadata = api_metadata,
    success = TRUE
  )
}

#' Validate API key format
#'
#' Checks that the provided API key matches expected Anthropic format.
#'
#' @param api_key Character. API key to validate.
#'
#' @returns TRUE if valid (invisibly). Throws error if invalid.
#'
#' @noRd
validate_api_key <- function(api_key) {
  if (!grepl("^sk-ant-", api_key)) {
    stop("API key should start with 'sk-ant-'")
  }
  invisible(TRUE)
}

#' Test API connectivity
#'
#' Performs a minimal API call to verify the key works and connection is live.
#' This is an optional pre-flight check before running expensive operations.
#'
#' @param api_key Character. Anthropic API key to test.
#'
#' @returns TRUE if connection successful (invisibly). Throws error if failed.
#'
#' @importFrom ellmer chat_anthropic params
#'
#' @noRd
test_llm_connection <- function(api_key) {
  # Set API key ----
  Sys.setenv(ANTHROPIC_API_KEY = api_key)

  # Create minimal chat instance ----
  test_chat <- chat_anthropic(
    model = "claude-sonnet-4-20250514",
    params = params(max_tokens = 50)
  )

  # Send test message ----
  test_chat$chat("Hello, please respond with 'API connection successful'")

  invisible(TRUE)
}
