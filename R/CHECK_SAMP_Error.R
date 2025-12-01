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
CHECK_SAMP_Error <- function (format, data, exp_cols) {

  # Expected Analyte
  ea_df <- data %>%
    distinct(across(all_of(exp_cols)))

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
        Range_Status = case_when(
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

  result <- list(
    EA = ea_df,
    BQL = bql
  )

  return(result)
}


# data <- read_xlsx("C:/Users/YuanWang/Downloads/NP587_DTA (1).xlsx")
# exp_cols <- c(
#   "Sample Type",
#   "Total di-22:6 BMP Result Unit",
#   "Total di-22:6 BMP LLOQ",
#   "Total di-22:6 BMP ULOQ",
#   "Total di-22:6 BMP LLOQ/ULOQ Units",
#   "Creatinine Result Unit",
#   "Creatinine LLOQ",
#   "Creatinine ULOQ",
#   "Creatinine LLOQ/ULOQ Units",
#   "Normalized Total di-22:6 BMP Result Unit",
#   "Normalized Total di-22:6 BMP LLOQ",
#   "Normalized Total di-22:6 BMP ULOQ",
#   "Normalized Total di-22:6 BMP LLOQ/ULOQ Units"
# )
# ea_df <- data %>%
#   distinct(across(all_of(exp_cols)))
#
# long_df <- data %>%
#   pivot_longer(
#     cols = matches(" (Result|LLOQ|ULOQ)$"),  # 只抓这三类列
#     names_to  = c("Analyte", "Metric"),      # 拆成两部分
#     names_pattern = "^(.*) (Result|LLOQ|ULOQ)$",
#     values_to = "Value"
#   )
#
# wide_df <- long_df %>%
#   pivot_wider(
#     names_from  = Metric,  # Result / LLOQ / ULOQ
#     values_from = Value
#   ) %>%
#   mutate(
#     Result = as.numeric(Result),
#     LLOQ   = as.numeric(LLOQ),
#     ULOQ   = as.numeric(ULOQ)
#   )
#
# # 4. 标记是否在范围内
# wide_flagged <- wide_df %>%
#   mutate(
#     Range_Status = case_when(
#       is.na(Result) | is.na(LLOQ) | is.na(ULOQ) ~ NA_character_,
#       Result < LLOQ ~ "Below LLOQ",
#       Result > ULOQ ~ "Above ULOQ",
#       TRUE          ~ "Within Range"
#     )
#   )
#
# # 5. 取出“Result 不在 LLOQ/ULOQ 范围内”的行
# out_of_range <- wide_flagged %>%
#   filter(Range_Status %in% c("Below LLOQ", "Above ULOQ"))
