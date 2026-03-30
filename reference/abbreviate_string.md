# Abbreviate string to first n words with case formatting

Takes a string, extracts the first n words (removing special
characters), and formats them according to the specified case style.

## Usage

``` r
abbreviate_string(
  string,
  n_words,
  case = c("lower", "upper", "sentence", "snake", "title", "screamingsnake", "camel")
)
```

## Arguments

- string:

  Character. The input string to abbreviate.

- n_words:

  Integer. Number of words to include in the abbreviation.

- case:

  Character. Case style for the output. One of:

  - "lower" - all lowercase, no separator (e.g., "dogsandcats")

  - "upper" - all uppercase, no separator (e.g., "DOGSANDCATS")

  - "sentence" - sentence case, no separator (e.g., "Dogsandcats")

  - "snake" - lowercase with underscores (e.g., "dogs_and_cats")

  - "title" - title case, no separator (e.g., "DogsAndCats")

  - "screamingsnake" - uppercase with underscores (e.g.,
    "DOGS_AND_CATS")

  - "camel" - camel case (e.g., "dogsAndCats")

## Value

Character. The abbreviated and formatted string.
