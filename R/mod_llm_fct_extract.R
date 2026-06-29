# R/mod_llm_fct_api.R

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
#' @param params Function. Maximum tokens for the API response.
#'
#' @return Named list with three elements:
#'   \describe{
#'     \item{result}{The structured extraction result from the LLM}
#'     \item{metadata}{List containing cost information or error details}
#'     \item{success}{Logical indicating if extraction succeeded}
#'   }
#'
#' @importFrom ellmer chat_anthropic params content_pdf_file
#' @importFrom glue glue
#' @export
extract_pdf_with_llm <- function(
  pdf_path,
  model_provider,
  model_name,
  env_var,
  chat_fn,
  api_key,
  params = NULL,
  extraction_prompt,
  extraction_schema,
  max_tokens
) {
  tryCatch(
    {
      # Set the provider-specific API key env var
      do.call(Sys.setenv, setNames(list(api_key), env_var))
      # allow for the user to manually specify all params via advanced options if designed
      # will override max_tokens input
      if (is.null(params)) {
        params <- params(max_tokens = max_tokens)
      }

      # Initialise chat using the provider-specific ellmer function
      tryCatch(
        {
          chat <- do.call(
            getExportedValue("ellmer", chat_fn),
            list(model = model_name, params = params)
          )
        },
        error = function(e) stop(glue("Error initialising LLM chat: {e}"))
      )

      # Prepare content
      # Slightly different workflows for Google vs Anthropic & OpenAI
      if (model_provider == "Google") {
        pdf_content <- google_upload(pdf_path)
      } else {
        pdf_content <- content_pdf_file(pdf_path)
      }

      # Extract data (the blocking operation)
      result <- chat$chat_structured(
        extraction_prompt,
        pdf_content,
        type = extraction_schema
      )

      # Get cost info
      # TODO: get_cost() works in a mre, so there's presumably somethign wrong in our logic
      api_metadata <- tryCatch(
        list(total_cost = chat$get_cost(include = "all")),
        error = function(e) list(cost_error = e$message)
      )

      # Return results
      list(
        result = result,
        metadata = api_metadata,
        success = TRUE
      )
    },
    error = function(e) {
      list(
        result = NULL,
        metadata = NULL,
        success = FALSE,
        error = llm_http_error_message(e)
      )
    }
  )
}

#' Parse LLM API errors into user-facing messages
#'
#' Detects HTTP status codes from httr2/ellmer error conditions and returns
#' a string with the code and most likely cause. Falls back to the raw
#' condition message for non-HTTP errors.
#'
#' @param e An error condition.
#' @return Character string.
#' @importFrom glue glue
#' @noRd
llm_http_error_message <- function(e) {
  msg <- conditionMessage(e)
  code <- regmatches(msg, regexpr("[45][0-9]{2}", msg))

  if (length(code) == 1L) {
    explanation <- switch(
      code,
      "400" = "Bad request - prompt or schema may be malformed.",
      "401" = "Unauthorized - API key invalid or has expired.",
      "403" = "Forbidden - API key does not have access to this model.",
      "404" = "Not found - requested model does not exist.",
      "422" = "Unprocessable - request was structurally valid but semantically rejected.",
      "429" = "Rate limit exceeded - wait a moment then try again, and/or check billing account.",
      "500" = "Internal server error - unexpected error occurred on the provider's side.",
      "502" = "Bad gateway - provider's infrastructure returned invalid response; try again shortly.",
      "503" = "Service unavailable - provider temporarily down; try again later.",
      "529" = "API overloaded - the provider under high load; try again in a few minutes.",
      "Unexpected HTTP error."
    )
    glue("HTTP {code}: {explanation}")
  } else {
    msg
  }
}

#' Validate API key format
#'
#' Checks that the provided API key matches expected Anthropic format.
#'
#' @param api_key Character. API key to validate.
#'
#' @return TRUE if valid (invisibly). Throws error if invalid.
#'
#' @importFrom glue glue
#' @noRd
validate_api_key <- function(api_key, provider = "Anthropic") {
  prefix <- switch(
    provider,
    "Anthropic" = "sk-ant-",
    "OpenAI" = "sk-",
    "Google Gemini" = "AQ.",
    NULL
  )
  if (!is.null(prefix) && !startsWith(api_key, prefix)) {
    stop(glue("API key for {provider} should start with '{prefix}'"))
  }
  invisible(TRUE)
}

#' Test LLM connectivity
#'
#' Sends a minimal one-token prompt to the selected model to confirm the key
#' and endpoint are reachable. Designed to run inside a mirai() worker.
#'
#' @param model_provider,model_name,env_var,chat_fn,api_key As passed to
#'   extract_pdf_with_llm().
#'
#' @return Named list: \code{list(success, message)}.
#'
#' @importFrom ellmer params
#' @importFrom glue glue
#' @noRd
test_llm_connection <- function(
  model_provider,
  model_name,
  env_var,
  chat_fn,
  api_key
) {
  tryCatch(
    {
      do.call(Sys.setenv, setNames(list(api_key), env_var))

      test_chat <- do.call(
        getExportedValue("ellmer", chat_fn),
        list(model = model_name, params = params(max_tokens = 20))
      )

      reply <- test_chat$chat("Reply with only the word: OK")

      list(
        success = TRUE,
        message = glue(
          '{model_provider} / {model_name}: connected. Model replied: "{trimws(reply)}"'
        )
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        message = glue(
          "{model_provider} / {model_name}: {llm_http_error_message(e)}"
        )
      )
    }
  )
}

#' Create extraction prompt with controlled vocabulary
#' @description Creates the system prompt for LLM structured extraction
#' @importFrom readr read_file
#' @noRd
create_extraction_prompt <- function() {
  read_file("inst/app/www/md/extraction_prompt.md")
}

#' Clear LLM data from session reactiveValues#
#' @param session Shiny session object
#' @noRd

# TODO: Replace with regex to make more robust to changes in data structure?
clear_llm_data_from_session <- function(session) {
  session$userData$reactiveValues$campaignDataLLM <- NULL
  session$userData$reactiveValues$referenceDataLLM <- NULL
  session$userData$reactiveValues$sitesDataLLM <- NULL
  session$userData$reactiveValues$parametersDataLLM <- NULL
  session$userData$reactiveValues$compartmentsDataLLM <- NULL
  session$userData$reactiveValues$biotaDataLLM <- NULL
  session$userData$reactiveValues$methodsDataLLM <- NULL
  session$userData$reactiveValues$samplesDataLLM <- NULL
  showNotification(
    "Cleared all LLM extracted data from session",
    type = "message"
  )
}


#' Render LLM extraction self-appraisal comments
#'
#' @param named_list Named list of appraisal comments from the LLM.
#' @return A \code{shiny::tagList} containing styled appraisal entries.
#'
#' @importFrom glue glue
#' @importFrom stringr str_extract str_replace
#' @noRd
render_extraction_comments <- function(named_list) {
  pretty_name <- c(
    "paper_relevance" = "Relevance",
    "paper_reliability" = "Reliability",
    "paper_data_source" = "Original Data",
    "paper_data_available" = "Data Availability",
    "extraction_assessement" = "Extraction Suitability"
  )

  # Parse score number and derive Bootstrap contextual colour
  parse_score <- function(text) {
    num <- as.integer(str_extract(text, "(?<=Score: )[1-5]"))
    color <- if (is.na(num)) {
      "secondary"
    } else if (num >= 4) {
      "success"
    } else if (num == 3) {
      "warning"
    } else {
      "danger"
    }
    list(num = num, color = color)
  }

  tagList(
    lapply(names(named_list), function(nm) {
      s <- parse_score(named_list[[nm]])
      clean_text <- str_replace(named_list[[nm]], "\\s*Score: [1-5]\\s*", " ")
      tags$div(
        class = "mb-1",
        tags$span(
          class = glue(
            "px-2 py-1 rounded fw-semibold bg-{s$color}-subtle text-{s$color}-emphasis me-1"
          ),
          glue("{pretty_name[[nm]]} ({s$num}/5):")
        ),
        tags$span(clean_text)
      )
    })
  )
}
