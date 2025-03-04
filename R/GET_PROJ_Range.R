#' LLOQ Info Query
#'
#' Get Project LLOQ Info from PFS
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @param site The PFS version need to work on: "Test" or "Production", "Test" as default
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return one data frame including species, matrix, LLOQ, ULOQ and other info
#' @examples
#' GET_PROJ_Range("Test", "user", "password");
#' @export
GET_PROJ_Range <- function (site = "Test", username, password) {
  if (site == "Test") {
    url <- "https://na1test.platformforscience.com/Nextcea_Test_28Mar2024/odata/LLOQ_AND_ULOQ_RANGE"
  } else if (site == "Production") {
    url <- "https://na1.platformforscience.com/Nextcea+Prod/odata/LLOQ_AND_ULOQ_RANGE"
  }

  range_table <- NULL
  while (TRUE) {
    response <- GET(url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value
    range_table <- as.data.frame(rbind(range_table, df))
    if (!is.null(data[["@odata.nextLink"]]) ) {
      url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }

  return(range_table)
}
