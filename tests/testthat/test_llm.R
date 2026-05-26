# pdf_path <- "paper_extraction_failed.pdf"

# if (!grepl("^sk-ant-", Sys.getenv(c("ANTHROPIC_API_KEY")))) {
#   print("bad api")
#   stop()
# }

# test_chat <- NULL
# test_chat <- chat_anthropic(
#   model = "claude-sonnet-4-5-20250929",
#   params = params(max_tokens = 50)
# )
# test_response <- tryCatch(
#   {
#     test_chat$chat(
#       "Hello, please respond with 'API connection successful'"
#     )
#   },
#   error = function(e) showNotification(paste0("Error:", e), type = "error")
# )

# if (is.null(test_chat)) {
#   return()
# }

# chat <- chat_anthropic(
#   model = "claude-sonnet-4-5-20250929",
#   params = params(max_tokens = 6000)
# )

# pdf_content <- content_pdf_file(pdf_path)

# extraction_schema <- create_extraction_schema()
# system_prompt <- create_extraction_prompt()

# result <- chat$chat_structured(
#   system_prompt,
#   pdf_content,
#   type = extraction_schema
# )

# # Check if extraction failed ----
# # Result is considered failed if all leaf values are empty (NA, NULL, character(0), etc.)

# cost_info <- chat$get_cost(include = "all")
# api_metadata <- list(
#   total_cost = cost_info
# )
