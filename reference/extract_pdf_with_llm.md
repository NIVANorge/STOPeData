# Core async function for LLM extraction

This function performs the actual PDF extraction using the Anthropic,
Google Gemini or OpenAI APIs. It's designed to run inside a
\`mirai::mirai()“ call for async execution.

## Usage

``` r
extract_pdf_with_llm(
  pdf_path,
  model_provider,
  model_name,
  env_var,
  chat_fn,
  api_key,
  params = NULL,
  extraction_prompt,
  extraction_schema,
  max_tokens,
  cache = "none"
)
```

## Arguments

- pdf_path:

  Character. Path to the PDF file to extract data from.

- model_provider:

  Character. The provider of the LLM. One of Anthropic, OpenAI, or
  Google

- model_name:

  Character. The specific model to call (e.g. "claude-sonnet-5").

- env_var:

  Character. Name of the environment variable holding the API key for
  this provider (set via
  [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html) before the
  call).

- chat_fn:

  Character. Name of the `ellmer` chat-constructor function to use for
  this provider (e.g. "chat_anthropic").

- api_key:

  Character. API key.

- params:

  Function. Maximum tokens for the API response.

- extraction_prompt:

  Character. The prompt text instructing the LLM on what to extract.

- extraction_schema:

  List or S7 class. The structured schema defining expected output
  format (e.g., S7 class with properties).

- max_tokens:

  Integer. Maximum tokens for the API response.

- cache:

  Boolean Attempt to use ellmer's built in cache function. Disabled for
  calls to Google LLMs

## Value

Named list with three elements:

- result:

  The structured extraction result from the LLM

- metadata:

  List containing cost information or error details

- success:

  Logical indicating if extraction succeeded
