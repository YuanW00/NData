#' Load Dataset in the Package
load_data <- function(site) {
  data_path <- file.path(get_cache_dir(), paste0(site, "_project_page.rds"))
  if (!file.exists(data_path)) {
    message("Data not found.")
  }
  return(readRDS(data_path))
}
