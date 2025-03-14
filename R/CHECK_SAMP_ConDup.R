#' DTA Consistency/Duplication Check
#'
#' Check whether DTA file is consistent/duplication with the selected columns
#' @import dplyr
#' @param data The data frame after data transfer
#' @param dup_cols The columns to check duplication
#' @param std_cols The columns as conditions
#' @param check_cols The columns to check consistency
#' @return Return data frames with inconsistent/duplicated data
#' @examples
#' CHECK_SAMP_ConDup(data, dup_cols = c("Subject ID", "Visit Name"), std_cols = c("Subject ID", "Visit Name", "Sample Type"), check_cols = c("Nextcea Received Date", "Gender"));
#' @export

CHECK_SAMP_ConDup <- function (data, dup_cols, std_cols, check_cols) {
  # Duplicate Samples
  dup <- data %>%
    group_by(across(all_of(dup_cols))) %>%
    mutate(count = n()) %>%
    filter(count > 1) |>
    ungroup() |>
    arrange(across(all_of(dup_cols))) |>
    select(all_of(dup_cols), everything())

  if (length(dup$`Subject ID`) == 0) {
    message1 <- "There is no duplicate data for your selection"
  } else {
    message1 <- "The following are inconsistent data for your selected columns"
  }

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
    message2 <- "There is no inconsistent data for your selection"
  } else {
    message2 <- "The following are inconsistent data for your selected columns"
  }

  result <- list(
    dup_message = message1,
    dup_data = dup,
    incon_message = message2,
    incon_data = inconsistent_data
  )

  return(result)
}
