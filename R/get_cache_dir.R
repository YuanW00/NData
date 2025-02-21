#' Get Package Directory
get_cache_dir <- function() {
  if (.Platform$OS.type == "windows") {
    cache_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "NData")
  } else {
    cache_dir <- file.path(Sys.getenv("HOME"), ".cache", "NData")
  }
  cache_dir <- normalizePath(cache_dir, winslash = "\\", mustWork = FALSE)
  return(cache_dir)
}

