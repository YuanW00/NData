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

  lot_info <- paste0("NEXTCEA_SAMPLE_LOT?$expand=PROJECT&$filter=PROJECT/any(a:contains(a/Name,'", project, "'))")
  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    lot_url <- paste0(t_url, lot_info)
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    lot_url <- paste0(p_url, lot_info)
  } else {
    print("Wrong Site!")
  }

  sample_lot <- NULL
  while (TRUE) {
    response <- GET(lot_url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value |>
      unnest(where(is.list), names_sep = "_") |>
      filter(Active == TRUE) |>
      select(-c(EntityTypeName, Id, Sequence, Created, Modified, Active, LikedBy, FollowedBy,
                Locked, CI_LOT_NUM, PROJECT_EntityTypeName, PROJECT_Id, PROJECT_Sequence,
                PROJECT_Created, PROJECT_Modified, PROJECT_Active, PROJECT_LikedBy,
                PROJECT_FollowedBy, PROJECT_Locked))
    sample_lot <- as.data.frame(rbind(sample_lot, df))
    if (!is.null(data[["@odata.nextLink"]]) ) {
      lot_url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }

  return(sample_lot)
}
