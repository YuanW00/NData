#' DTA Error Check
#'
#' Check DTA file expected analytes and BQL values
#' @import dplyr
#' @param data The data frame after data transfer
#' @return Return data frames with expected analytes and BQL values
#' @examples
#' CHECK_SAMP_Error(data);
#' @export

CHECK_SAMP_Error <- function (data) {

  # Expected Analyte
  ea_df <- data |>
    distinct(`Sample Type`, `Test Name`, `Test Result Unit`, `Lab Test LLOQ`,
             `Lab Test ULOQ`, `LLOQ/ULOQ Units`)
  # BQL
  bql <- data |>
    filter(`Test Result Unit` != `LLOQ/ULOQ Units`|
             `Test Result Unit` == `LLOQ/ULOQ Units` & `Test Result` < `Lab Test LLOQ` |
             `Test Result Unit` == `LLOQ/ULOQ Units` & `Test Result` > `Lab Test ULOQ`)

  result <- list(
    EA = ea_df,
    BQL = bql
  )

  return(result)
}
