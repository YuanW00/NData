
#' OS file Conversion
#'
#' Convert OS file to LIMS template format
#' @import dplyr
#' @import readr
#' @import writexl
#' @import tidyverse
#' @import stringr
#' @param path The working directory that stores all the input data and output data
#' @param OS_file The data file that is exported from SCIEX OS
#' @param Template_file The data template needs to be filled to upload the results to the LIMS
#' @return Return nothing but write a .txt file that is ready to be uploaded to the LIMS
#' @examples
#' OS_to_LIMS("~/Data", "OS.txt", "LIMS.txt");
#' @export
DTA <- function(
