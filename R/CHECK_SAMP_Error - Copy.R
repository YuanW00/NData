#' DTA Error Check
#'
#' Check DTA file expected analytes and BQL values
#' @import dplyr
#' @import purrr
#' @import rlang
#' @param format The format of DTA data
#' @param data The data frame after data transfer
#' @return Return data frames with expected analytes and BQL values
#' @examples
#' CHECK_SAMP_Error(data);
#' @export
CHECK_SAMP_Error_copy <- function (format, data) {
  if (format == "landscape") {
    # Expected Analytes
    ea_cols <- c("Sample Type",
                 paste0("Test Name ", seq(1:15)),
                 paste0("Test Result Unit ", seq(1:15)),
                 paste0("Lab Test LLOQ ", seq(1:15)),
                 paste0("Lab Test ULOQ ", seq(1:15)),
                 paste0("LLOQ/ULOQ Units ", seq(1:15))
    )
    ea_df <- data |>
      distinct(across(intersect(ea_cols, colnames(data))))

    # BQL
    test_cols <- names(data)
    test_nums <- test_cols[str_detect(test_cols, "^Test Name ")] %>%
      str_extract("\\d+") %>%
      as.integer() %>%
      na.omit() %>%
      sort()
    max_test <- ifelse(length(test_nums) == 0, 0, max(test_nums))

    bql_conditions <- lapply(1:15, function(i) {
      unit_col   <- sym(paste0("Test Result Unit ", i))
      lloq_col   <- sym(paste0("Lab Test LLOQ ", i))
      uloq_col   <- sym(paste0("Lab Test ULOQ ", i))
      ref_unit   <- sym(paste0("LLOQ/ULOQ Units ", i))
      result_col <- sym(paste0("Test Result ", i))

      expr(
        (!!unit_col != !!ref_unit) |
          ((!!unit_col == !!ref_unit) & (!!result_col < !!lloq_col)) |
          ((!!unit_col == !!ref_unit) & (!!result_col > !!uloq_col))
      )
    })

    bql_conditions <- lapply(1:max_test, function(i) {
      unit_col   <- sym(paste0("Test Result Unit ", i))
      lloq_col   <- sym(paste0("Lab Test LLOQ ", i))
      uloq_col   <- sym(paste0("Lab Test ULOQ ", i))
      ref_unit   <- sym(paste0("LLOQ/ULOQ Units ", i))
      result_col <- sym(paste0("Test Result ", i))

      expr(
        (!!unit_col != !!ref_unit) |
          ((!!unit_col == !!ref_unit) & (!!result_col < !!lloq_col)) |
          ((!!unit_col == !!ref_unit) & (!!result_col > !!uloq_col))
      )
    })

    final_condition <- purrr::reduce(bql_conditions, function(x, y) expr((!!x) | (!!y)))

    bql <- data %>%
      rowwise() %>%
      filter(eval_tidy(final_condition)) %>%
      ungroup()

  } else if (format == "vertical") {

    # Expected Analyte
    ea_df <- data |>
      distinct(`Sample Type`, `Test Name`, `Test Result Unit`, `Lab Test LLOQ`,
               `Lab Test ULOQ`, `LLOQ/ULOQ Units`)
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
