#' OS Formula Check
#'
#' Convert OS file to LIMS template format
#' @param standard_file The formula file that is exported from SCIEX OS template and saved as .txt.
#' @param test_file The formula file that is exported from SCIEX OS analyte and saved as .txt.
#' @return Return the result of calculation checking: whether there is a difference between the formula in the analyte file and standard file. And if there are difference, return the data frame containing the different formulas from both standard file and test file.
#' @examples
#' SCIEX_calculation_check(Standard, Test);
#' @export
SCIEX_calculation_check <- function(
    standard_file,
    test_file) {
  standard <- read.table(standard_file, sep = "\t")
  standard <- standard[, 1:2]
  test <- read.table(test_file, sep = "\t")
  if (identical(standard, test)) {
    result <- "Test file is identical to the standard."
  } else {
    comparison <- standard == test
    rowwise_result <- apply(comparison, 1, function(x) all(!is.na(x) & x))
    result <- list(
      message = "Test file is different from the standard.",
      # standard_diff = data.frame(Source = "Standard", Rows = which(rowwise_result==FALSE), Formula = standard[!rowwise_result, ]$V2),
      # test_diff = data.frame(Source = "Test", Rows = which(!rowwise_result), Formula = test[!rowwise_result, ]$V2)
      diff = rbind(
        data.frame(Source = "Standard", Rows = which(rowwise_result==FALSE), Formula = standard[!rowwise_result, ]$V2),
        data.frame(Source = "Test", Rows = which(!rowwise_result), Formula = test[!rowwise_result, ]$V2)
      )
    )
  }
  return(result)
}
