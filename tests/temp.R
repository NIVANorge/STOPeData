normalizedPath <- normalizePath(directoryPath, mustWork = TRUE)

if (!is.null(getShinyOption("server", default = NULL))) {
  getShinyOption("server")$setStaticPath(...) # push down to httpuv
}
.globals$resourcePaths[[prefix]] <- staticPath(normalizedPath)
.globals$resources[[prefix]] <- list(
  directoryPath = normalizedPath,
  func = staticHandler(normalizedPath)
)
