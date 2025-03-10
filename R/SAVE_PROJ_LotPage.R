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
#' SAVE_project_sample_lot_page("Test", "user", "password");
#' @export
SAVE_PROJ_LotPage <- function(site, username, password) {

  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    url <- paste0(t_url, "NEXTCEA_SAMPLE_LOT?$expand=PROJECT")
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    url <- paste0(p_url, "NEXTCEA_SAMPLE_LOT?$expand=PROJECT")
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

  message("Data updated successfully!")

  return(project_page)
}





