#' DTA Error Check
#'
#' Check DTA file expected analytes and BQL values
#' @import dplyr
#' @import purrr
#' @import rlang
#' @param format The format of DTA data
#' @param data The data frame after data transfer
#' @param exp_cols The columns match to sample type, analyte, unit, LLOQ/ULOQ
#' @return Return data frames with expected analytes and BQL values
#' @examples
#' CHECK_SAMP_Error(data);
#' @export
CHECK_SAMP_Error <- function (format, data, exp_cols, lloq_opt, lloq = NA, uloq = NA, ulunit = NA) {

  # Expected Analyte
  ea_df <- data %>%
    distinct(across(all_of(exp_cols)))
  # distinct(PCSPEC, LBTEST, LBORRESU)

  if (lloq_opt == TRUE) {
    if (format == "landscape") {
      # BQL
      long_df <- data %>%
        pivot_longer(
          cols = matches(" (Result)$"),
          names_to  = c("Analyte", "Metric"),
          names_pattern = "^(.*) (Result)$",
          values_to = "Value"
        )

      wide_df <- long_df %>%
        pivot_wider(
          names_from  = Metric,
          values_from = Value
        ) %>%
        mutate(
          Result = as.numeric(Result),
          LLOQ   = as.numeric(lloq),
          ULOQ   = as.numeric(uloq)
        )

      bql <- wide_df %>%
        mutate(
          Status = case_when(
            is.na(Result) | is.na(LLOQ) | is.na(ULOQ) ~ "Missing Value",
            Result < LLOQ ~ "Below LLOQ",
            Result > ULOQ ~ "Above ULOQ",
            TRUE          ~ "Within Range"
          )
        ) %>%
        filter(Status %in% c("Below LLOQ", "Above ULOQ", "Missing Value")) |>
        select(-Analyte, -Result, -LLOQ, -ULOQ)

    } else if (format == "vertical") {
      # BQL
      bql <- data |>
        filter(`Test Result Unit` != ulunit |
                 `Test Result Unit` == ulunit & `Test Result` < lloq |
                 `Test Result Unit` == ulunit & `Test Result` > uloq)
    }

  } else if (lloq == FALSE) {
    if (format == "landscape") {
      # BQL
      long_df <- data %>%
        pivot_longer(
          cols = matches(" (Result|LLOQ|ULOQ)$"),
          names_to  = c("Analyte", "Metric"),
          names_pattern = "^(.*) (Result|LLOQ|ULOQ)$",
          values_to = "Value"
        )

      wide_df <- long_df %>%
        pivot_wider(
          names_from  = Metric,
          values_from = Value
        ) %>%
        mutate(
          Result = as.numeric(Result),
          LLOQ   = as.numeric(LLOQ),
          ULOQ   = as.numeric(ULOQ)
        )

      bql <- wide_df %>%
        mutate(
          Status = case_when(
            is.na(Result) | is.na(LLOQ) | is.na(ULOQ) ~ "Missing Value",
            Result < LLOQ ~ "Below LLOQ",
            Result > ULOQ ~ "Above ULOQ",
            TRUE          ~ "Within Range"
          )
        ) %>%
        filter(Status %in% c("Below LLOQ", "Above ULOQ", "Missing Value")) |>
        select(-Analyte, -Result, -LLOQ, -ULOQ)

    } else if (format == "vertical") {
      # BQL
      bql <- data |>
        filter(`Test Result Unit` != `LLOQ/ULOQ Units`|
                 `Test Result Unit` == `LLOQ/ULOQ Units` & `Test Result` < `Lab Test LLOQ` |
                 `Test Result Unit` == `LLOQ/ULOQ Units` & `Test Result` > `Lab Test ULOQ`)
    }
  } else {

    ea_df <- "Not avaliable"
    bql <- "Not avaliable"

  }



  result <- list(
    EA = ea_df,
    BQL = bql
  )

  return(result)
}
