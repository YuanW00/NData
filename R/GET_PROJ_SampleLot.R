#' Sample Lot Query
#'
#' Get Sample Lot Information from PFS
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param project The Project Barcode to upload the analyte result
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return one data frame including all the sample information in the sample lot
#' @examples
#' GET_PROJ_SampleLot("Test", "NP1", "user", "password");
#' @export
GET_PROJ_SampleLot <- function (site, project, username, password) {

  if (site == "Test") {
    data("test", package = "NData", envir = environment())
    test <- force(test)
    proj_pages <- test$page[test$PROJECT_Barcode == project]
    data("t_url", package = "NData", envir = environment())
    int_url <- paste0(t_url, "NEXTCEA_SAMPLE_LOT?$expand=PROJECT&$skiptoken=")
    last_page <- max(test$page)
  } else if (site == "Production") {
    data("prod", package = "NData", envir = environment())
    prod <- force(prod)
    proj_pages <- prod$page[prod$PROJECT_Barcode == project]
    data("p_url", package = "NData", envir = environment())
    int_url <- paste0(p_url, "NEXTCEA_SAMPLE_LOT?$expand=PROJECT&$skiptoken=")
    last_page <- max(prod$page)
  } else {
    print("Wrong Site!")
  }

  sample_lot <- NULL
  url <- paste0(int_url, last_page)
  while (TRUE) {
    response <- GET(url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value |>
      unnest(where(is.list), names_sep = "_") |>
      filter(PROJECT_Barcode == project)
    sample_lot <- as.data.frame(rbind(sample_lot, df))
    if (!is.null(data[["@odata.nextLink"]]) ) {
      url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }

  if (!is.null(proj_pages)) {
    for (i in proj_pages) {
      url <- paste0(int_url, i)
      response <- GET(url, authenticate(username, password))
      data <- fromJSON(content(response, "text"))
      df <- data$value |>
        unnest(where(is.list), names_sep = "_") |>
        filter(PROJECT_Barcode == project)
      sample_lot <- as.data.frame(rbind(sample_lot, df))
    }
  }

  sample_lot <- sample_lot |>
    filter(Active == TRUE) |>
    select(-c(EntityTypeName, Id, Sequence, Created, Modified, Active, LikedBy, FollowedBy,
              Locked, CI_LOT_NUM, PROJECT_EntityTypeName, PROJECT_Id, PROJECT_Sequence,
              PROJECT_Created, PROJECT_Modified, PROJECT_Active, PROJECT_LikedBy,
              PROJECT_FollowedBy, PROJECT_Locked))

  return(sample_lot)
}
