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
#' @return Return one data frame including samples information: barcode, name, and active status
#' @examples
#' GET_LCMS_SampleBarcodes("Test", "LCMS1", "user", "password");
#' @export
GET_LCMS_SampleBarcodes <- function (site, ept_barcode, username, password) {

  sample <- paste0("LC_MSMS_EXPERIMENT('", ept_barcode, "')/EXPERIMENT_SAMPLES?$expand=ENTITY")
  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    sample_url <- paste0(t_url, sample)
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    sample_url <- paste0(p_url, sample)
  } else {
    print("Wrong Site!")
  }

  sample_df <- NULL
  while (TRUE) {
    response <- GET(sample_url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value
    df_sub <- df |>
      filter(Active == TRUE) |>
      mutate(EXPT_SAMPLE_BARCODE = Barcode,
             EXPT_SAMPLE_ACT = Active,
             SAMPLE_NAME_REF = ENTITY$Name,
             SAMPLE_TYPE_REF = ENTITY$EntityTypeName,
             SAMPLE_ACT = ENTITY$Active) |>
      filter(EXPT_SAMPLE_ACT == TRUE & SAMPLE_ACT == TRUE) |>
      select(EXPT_SAMPLE_BARCODE, EXPT_SAMPLE_ACT,
             SAMPLE_NAME_REF, SAMPLE_TYPE_REF, SAMPLE_ACT) |>
      filter(SAMPLE_TYPE_REF == "NEXTCEA_SAMPLE_LOT") |>
      filter(!str_detect(SAMPLE_NAME_REF, "eQC"))
    sample_df <- rbind(sample_df, df_sub)
    if (!is.null(data[["@odata.nextLink"]])) {
      sample_url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }

  return(sample_df)
}
