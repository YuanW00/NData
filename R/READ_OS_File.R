#' OS Result Read in R
#'
#' Convert OS file to R data frame
#' @import dplyr
#' @import readr
#' @param OS_file The data file that is exported from SCIEX OS analyte and saved as .txt
#' @return Return one data frame transferred from SCIEX OS result file
#' @examples
#' READ_OS_File("LCMS1OS.txt");
#' @export
READ_OS_File <- function (OS_file) {
  table_start <- grep("Sample Name", readLines(OS_file))
  os <- read_delim(OS_file, delim = "\t",
                   skip = table_start - 1, col_names = TRUE) |>
    mutate(across(contains("Value"), as.numeric))
  return(os)
}
