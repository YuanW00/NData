#' Sample Barcode Query
#'
#' Get LCMS Sample Barcode from LIMS
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param ept_barcode The Experiment Barcode of samples
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return one data frame including sample barcodes, sample name, sample lot type and sample active status
#' @examples
#' GET_LCMS_SampleBarcodes("Test", "LCMS1", "user", "password");
#' @export
GET_LCMS_SampleBarcodes <- function (site, ept_barcode, username, password) {
  experiment <- paste0("LC_MSMS_EXPERIMENT('", ept_barcode, "')/EXPERIMENT_SAMPLES?$expand=ENTITY")
  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    url <- paste0(t_url, experiment)
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    url <- paste0(p_url, experiment)
  } else {
    print("Wrong Site!")
  }
  whole_df <- NULL
  while (TRUE) {
    response <- GET(url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value
    df_sub <- df |>
      filter(Active == TRUE) |>
      mutate(SAMPLE_NAME_REF = ENTITY$Name,
             SAMPLE_TYPE_REF = ENTITY$EntityTypeName) |>
      select(Active, Barcode, SAMPLE_NAME_REF, SAMPLE_TYPE_REF) |>
      filter(SAMPLE_TYPE_REF == "NEXTCEA_SAMPLE_LOT") |>
      filter(!str_detect(SAMPLE_NAME_REF, "eQC")) |>
      rename(EXPT_SAMPLE_BARCODE = Barcode)
    whole_df <- rbind(whole_df, df_sub)
    if (!is.null(data[["@odata.nextLink"]])) {
      url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }
  return(whole_df)
}
