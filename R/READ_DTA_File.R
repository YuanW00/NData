#' DTA Read in R
#'
#' Convert DTA file to R data frame
#' @import readxl
#' @param data_file The data file after data transfer and download as .xlsx
#' @return Return one data frame transferred from DTA file
#' @examples
#' READ_DTA_File("NP1_DTA.xlsx");
#' @export
READ_DTA_File <- function(data_file) {
  preview <- read_excel(data_file, n_max = 100)
  num_cols <- ncol(preview)
  col_types <- rep("guess", num_cols)
  col_types[num_cols] <- "text"
  data <- read_excel(data_file, col_types=col_types)
  return(data)
}
