#' Sample Test Date Query
#'
#' Get Sample Test Date from PFS
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param project The Project Barcode to upload the analyte result
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return one data frame including project barcode and corresponding pages
#' @examples
#' GET_SAMP_TestDate("Test", "user", "password");
#' @export
GET_SAMP_TestDate <- function (site, project, username, password) {
  if (site == "Test") {

    url <- paste0("https://na1test.platformforscience.com/Nextcea_Test_28Mar2024/odata/NEXTCEA_PROJECT('",
                  project, "')/REV_EXPERIMENT_PROJECT")
  } else if (site == "Production") {
    url <- paste0("https://na1.platformforscience.com/Nextcea+Prod/odata/NEXTCEA_PROJECT('",
                  project, "')/REV_EXPERIMENT_PROJECT")
  }

  test_date <- NULL
  while (TRUE) {
    response <- GET(url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value |>
      select(DATE, NOTES)
    test_date <- as.data.frame(rbind(test_date, df))
    if (!is.null(data[["@odata.nextLink"]]) ) {
      url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }
  test_date <- test_date |>
    rename(`Sample Test Date` = DATE)

  return(test_date)
}
