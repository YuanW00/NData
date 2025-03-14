#' DTA Consistency Check
#'
#' Check whether DTA file is consistent with the selected columns
#' @import dplyr
#' @param data The data frame after data transfer
#' @param std_cols The columns as conditions
#' @param check_cols The columns to check consistency
#' @return Return data frames with expected inconsistent data
#' @examples
#' CHECK_SAMP_Consis(data, std_cols = c("Subject ID", "Visit Name", "Sample Type"), check_cols = c("Nextcea Received Date", "Gender"));
#' @export

CHECK_SAMP_Consis <- function (data, std_cols, check_cols) {
  # Consistency
  consist_check <- data |>
    group_by(across(all_of(std_cols))) |>
    summarise(
      across(all_of(check_cols), n_distinct, .names = "n_unique_{.col}"),
      .groups = "drop"
    )

  inconsistent <- consist_check |>
    filter(if_any(starts_with("n_unique_"), ~ .x > 1))

  data_with_flags <- data |>
    left_join(
      inconsistent |>
        mutate(Consistent_Flag = FALSE),
      by = std_cols
    ) |>
    mutate(Consistent_Flag = if_else(is.na(Consistent_Flag), TRUE, Consistent_Flag))

  inconsistent_data <- data_with_flags |>
    select(-starts_with("n_unique_")) |>
    filter(Consistent_Flag == FALSE) |>
    arrange(across(all_of(std_cols))) |>
    select(all_of(std_cols), everything())

  if (length(inconsistent_data$`Subject ID`) == 0) {
    message <- "There is no inconsistent data for your selection"
    inconsistent_data <- NULL
  } else {
    message <- "The following are inconsistent data for your selected columns"
  }

  result <- list(
    message = message,
    incon_data = inconsistent_data
  )

  return(result)
}
