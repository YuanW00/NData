#' Project Experiment Info Query
#'
#' Get Project Experiment Info from PFS
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param project The Project Barcode to upload the analyte result
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return one data frame including sample index, test date, and other info
#' @examples
#' GET_PROJ_ExpmtInfo("Test", "NP1", "user", "password");
#' @export
GET_PROJ_ExpInfo <- function (site, project, username, password) {
  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    url <- paste0(t_url, "NEXTCEA_PROJECT('", project, "')/REV_EXPERIMENT_PROJECT")
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    url <- paste0(p_url, "NEXTCEA_PROJECT('", project, "')/REV_EXPERIMENT_PROJECT")
  }

  info_table <- NULL
  while (TRUE) {
    response <- GET(url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value
    info_table <- as.data.frame(rbind(info_table, df))
    if (!is.null(data[["@odata.nextLink"]]) ) {
      url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }

  info_table <- info_table |>
    rename(`Sample Test Date` = DATE,
           sample_index_note = NUMBERS_OF_SAMPLES_TO_BE_ANALYZED)
  colnames(info_table) <- gsub("^LLOQ_ANALYTE_(\\d+)$", "ANALYTE_\\1_LLOQ", colnames(info_table))
  colnames(info_table) <- gsub("^ULOQ_ANALYTE_(\\d+)$", "ANALYTE_\\1_ULOQ", colnames(info_table))

  return(info_table)
}
