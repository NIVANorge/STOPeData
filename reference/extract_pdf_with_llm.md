# Core async function for LLM extraction

This function performs the actual PDF extraction using the Anthropic
API. It's designed to run inside a mirai() call for async execution.

## Usage

``` r
extract_pdf_with_llm(
  pdf_path,
  api_key,
  extraction_prompt,
  extraction_schema,
  max_tokens
)
```

## Arguments

- pdf_path:

  Character. Path to the PDF file to extract data from.

- api_key:

  Character. Anthropic API key (should start with "sk-ant-").

- extraction_prompt:

  Character. The prompt text instructing the LLM on what to extract.

- extraction_schema:

  List or S7 class. The structured schema defining expected output
  format (e.g., S7 class with properties).

- max_tokens:

  Integer. Maximum tokens for the API response.

## Value

Named list with three elements:

- result:

  The structured extraction result from the LLM

- metadata:

  List containing cost information or error details

- success:

  Logical indicating if extraction succeeded
