
#' OS file Conversion
#'
#' Convert OS file to LIMS template format
#' @import dplyr
#' @import readxl
#' @import writexl
#' @import openxlsx
#' @import tidyverse
#' @param Standard The formula file that is exported from SCIEX OS template and saved as .xlsx
#' @param Test The formula file that is exported from SCIEX OS analyte and saved as .xlsx
#' @return Printed the result of calculation checking: whether there is a difference between the formula in the analyte file and standard file
#' @examples
#' SCIEX_calculation_check(Standard, Test);
#' @export
SCIEX_calculation_check <- function(Standard, Test) {
  standard_path <- "P:/Quality/Document Management/Doc/NXT Script/SCRT13_SCIEX OS Calculation Check/Standards/"
  test_path <- "P:/Quality/Document Management/Doc/NXT Script/SCRT13_SCIEX OS Calculation Check/Test/"
  standard <- read.csv(paste0(standard_path, Standard))
  test <- read.csv(paste0(test_path, Test))
  if (identical(standard, test)) {
    print("Test file is identical to the standard.")
  } else {
    # Compare each element in the two data frames
    comparison <- standard == test

    # Check rows that have differences
    rowwise_result <- apply(comparison, 1, function(x) all(!is.na(x) & x))

    # Combine rows that are different from each file
    result <- rbind(
      data.frame(Source = "Standard", Rows = which(!rowwise_result), Content = standard[!rowwise_result, ]),
      data.frame(Source = "Test", Rows = which(!rowwise_result), Content = test[!rowwise_result, ])
    )

    # Save the results to an Excel file
    output_path <- "P:/Quality/Document Management/Doc/NXT Script/SCRT13_SCIEX OS Calculation Check/"
    date <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
    output_name <- paste0(output_path, "test_result_", date, ".xlsx")
    write_xlsx(result, output_name)
    print(paste0("Differences have been saved to ", output_name))
  }
}
