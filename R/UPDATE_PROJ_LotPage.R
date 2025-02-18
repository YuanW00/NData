#' Sample Lot Page Query
#'
#' Get Sample Lot Matched Page from PFS
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return one data frame including project barcode and corresponding pages
#' @examples
#' UPDATE_project_sample_lot_page("Test", "user", "password");
#' @export
UPDATE_project_sample_lot_page <- function(site, username, password) {

  if (site == "Test") {
    url <- "https://na1test.platformforscience.com/Nextcea_Test_28Mar2024/odata/NEXTCEA_SAMPLE_LOT?$expand=PROJECT"
  } else if (site == "Production") {
    url <- "https://na1.platformforscience.com/Nextcea+Prod/odata/NEXTCEA_SAMPLE_LOT?$expand=PROJECT"
  }

  i <- 0
  page_project_lot <- NULL
  while (TRUE) {
    response <- GET(url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value
    ap_df <- df |>
      unnest(where(is.list), names_sep = "_") |>
      select(Active, Barcode, PROJECT_Barcode)
    ap_df$page = rep(i, length(ap_df$PROJECT_Barcode))
    page_project_lot <- as.data.frame(rbind(page_project_lot, ap_df))
    i <- i + 1
    if (!is.null(data[["@odata.nextLink"]]) ) {
      url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }

  project_page <- page_project_lot |>
    filter(Active == TRUE) |>
    distinct(PROJECT_Barcode, page)
  saveRDS(project_page, file = file.path(get_cache_dir(), paste0(site, "_project_page.rds")))
  message("Data updated successfully!")

  return(list(message, project_page))
}
