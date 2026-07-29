# Statically scans R/ for pkg::fun()/pkg:::fun() calls and checks each
# package is declared in DESCRIPTION and actually exports that function.

find_pkg_calls <- function(expr, file, acc) {
  # as.list() on calls with empty arguments (e.g. `x[, 1]`) yields a
  # missing-argument symbol that errors as soon as it's touched, so guard it.
  is_call <- tryCatch(is.call(expr), error = function(e) FALSE)
  if (is_call) {
    if (identical(expr[[1]], as.name("::")) || identical(expr[[1]], as.name(":::"))) {
      acc[[length(acc) + 1]] <- list(
        file = file,
        pkg = as.character(expr[[2]]),
        fun = as.character(expr[[3]])
      )
    }
    for (part in as.list(expr)) {
      acc <- find_pkg_calls(part, file, acc)
    }
  }
  acc
}

test_that("every pkg::fun()/pkg:::fun() call in R/ is declared and resolvable", {
  pkg_root <- if (grepl("testthat$", getwd())) "../.." else "."
  r_files <- list.files(file.path(pkg_root, "R"), pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)

  calls <- list()
  for (f in r_files) {
    for (e in parse(f)) {
      calls <- find_pkg_calls(e, basename(f), calls)
    }
  }
  expect_true(length(calls) > 0)

  # Packages declared as dependencies (version constraints stripped)
  desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"), fields = c("Imports", "Depends"))
  declared <- unlist(strsplit(desc, ","))
  declared <- trimws(gsub("\\(.*\\)", "", declared))
  declared <- declared[nzchar(declared)]
  # Base/recommended packages ship with every R install
  declared <- c(declared, rownames(installed.packages(priority = "base")))

  problems <- vapply(calls, function(call) {
    if (!(call$pkg %in% declared)) {
      return(sprintf(
        "%s: '%s' is used via %s::%s but is not declared in DESCRIPTION",
        call$file, call$pkg, call$pkg, call$fun
      ))
    }
    if (!requireNamespace(call$pkg, quietly = TRUE)) {
      return(sprintf(
        "%s: package '%s' (used as %s::%s) is not installed",
        call$file, call$pkg, call$pkg, call$fun
      ))
    }
    if (!exists(call$fun, envir = asNamespace(call$pkg), inherits = FALSE)) {
      return(sprintf(
        "%s: '%s' not found in package '%s' (used as %s::%s)",
        call$file, call$fun, call$pkg, call$pkg, call$fun
      ))
    }
    NA_character_
  }, character(1))
  problems <- problems[!is.na(problems)]

  expect_true(length(problems) == 0, info = paste(problems, collapse = "\n"))
})
