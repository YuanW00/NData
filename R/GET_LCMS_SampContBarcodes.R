#' Sample/Container Barcode Query
#'
#' Get LCMS Sample/Container Barcode from LIMS
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param ept_barcode The Experiment Barcode of samples
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return one data frame including samples and containers information: barcode, name, and active status
#' @examples
#' GET_LCMS_SampContBarcodes("Test", "LCMS1", "user", "password");
#' @export
GET_LCMS_SampContBarcodes <- function (site, ept_barcode, username, password) {
  rct <- paste0("LC_MSMS_EXPERIMENT('", ept_barcode, "')/EXPERIMENT_CONTAINERS?$expand=CONTAINER")
  sample <- paste0("LC_MSMS_EXPERIMENT('", ept_barcode, "')/EXPERIMENT_CONTAINERS?$expand=EXPERIMENT_SAMPLES($expand=ENTITY)")
  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    rct_url <- paste0(t_url, rct)
    sample_url <- paste0(t_url, sample)
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    rct_url <- paste0(p_url, rct)
    sample_url <- paste0(p_url, sample)
  } else {
    print("Wrong Site!")
  }

  rct_df <- NULL
  while (TRUE) {
    response <- GET(rct_url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value
    df_sub <- df |>
      filter(Active == TRUE) |>
      filter(CONTAINER$EntityTypeName == "RECEIPT_CONTAINER") |>
      mutate(RCT_CONT = CONTAINER$Barcode,
             RCT_CONT_ACT = CONTAINER$Active) |>
      filter(RCT_CONT_ACT == TRUE) |>
      select(Active, Barcode, RCT_CONT, RCT_CONT_ACT)
    rct_df <- rbind(rct_df, df_sub)
    if (!is.null(data[["@odata.nextLink"]])) {
      rct_url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }

  sample_df <- NULL
  while (TRUE) {
    response <- GET(sample_url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value |>
      unnest(where(is.list), names_sep = "_")
    df_sub <- df |>
      filter(Active == TRUE) |>
      mutate(EXPT_SAMPLE_BARCODE = EXPERIMENT_SAMPLES_Barcode,
             EXPT_SAMPLE_ACT = EXPERIMENT_SAMPLES_Active,
             SAMPLE_NAME_REF = EXPERIMENT_SAMPLES_ENTITY$Name,
             SAMPLE_TYPE_REF = EXPERIMENT_SAMPLES_ENTITY$EntityTypeName,
             SAMPLE_ACT = EXPERIMENT_SAMPLES_ENTITY$Active) |>
      filter(EXPT_SAMPLE_ACT == TRUE & SAMPLE_ACT == TRUE) |>
      select(Active, Barcode, EXPT_SAMPLE_BARCODE, EXPT_SAMPLE_ACT,
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

  whole_df <- left_join(sample_df, rct_df, by = c("Active", "Barcode"))
  return(whole_df)
}
