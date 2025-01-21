
#' Result convert to DTA format
#'
#' Convert Sample Lot Result to DTA format
#' @import dplyr
#' @import openxlsx
#' @import readxl
#' @import tidyverse
#' @param study_id The study id for current study, acquired from LIMS project
#' @param version The expected version of DTA format: "basic" or "plus", "basic" by default
#' @param data_file The result data file exported from LIMS sample lot
#' @param test_date The data file that recorded the test date for each sample index
#' @return Return nothing but write a .xlsx file that is ready to be sent to client
#' @examples
#' DTA(study_id = "STD1234", version = "basic", data_file = "result", test_date = "Testdate")
#' DTA(study_id = "STD1234", version = "plus", data_file = "result", test_date = "Testdate");
#' @export
DTA <- function (
    study_id, version = "basic",
    data_file, test_date
) {

  data <- read_excel(paste0(data_file, ".xlsx"))
  data$`LLOQ` <- NA
  data$`ULOQ` <- NA
  data$`ULOQU` <- NA

  colnames(data)[colnames(data) == "Analyte Result Comment"] <- "Analysis Result Comment"

  data_long <- data |>
    pivot_longer(
      cols = starts_with("Analyte"),
      names_to = c("AnalyteGroup", ".value"),
      names_pattern = "Analyte (\\d+) (.+)"
    )

  data_long <- data_long |>
    mutate(
      `Test Name` = case_when(
        AnalyteGroup == "1" ~ data$`Analyte 1`[!(is.na(data$`Analyte 1`))][1],
        AnalyteGroup == "2" ~ data$`Analyte 2`[!(is.na(data$`Analyte 2`))][1],
        AnalyteGroup == "3" ~ data$`Analyte 3`[!(is.na(data$`Analyte 3`))][1],
        AnalyteGroup == "4" ~ data$`Analyte 4`[!(is.na(data$`Analyte 4`))][1],
        AnalyteGroup == "5" ~ data$`Analyte 5`[!(is.na(data$`Analyte 5`))][1],
        AnalyteGroup == "6" ~ data$`Analyte 6`[!(is.na(data$`Analyte 6`))][1],
        AnalyteGroup == "7" ~ data$`Analyte 7`[!(is.na(data$`Analyte 7`))][1],
        AnalyteGroup == "8" ~ data$`Analyte 8`[!(is.na(data$`Analyte 8`))][1],
        AnalyteGroup == "9" ~ data$`Analyte 9`[!(is.na(data$`Analyte 9`))][1],
        AnalyteGroup == "10" ~ data$`Analyte 10`[!(is.na(data$`Analyte 10`))][1],
        AnalyteGroup == "11" ~ data$`Analyte 11`[!(is.na(data$`Analyte 11`))][1],
        AnalyteGroup == "12" ~ data$`Analyte 12`[!(is.na(data$`Analyte 12`))][1],
        AnalyteGroup == "13" ~ data$`Analyte 13`[!(is.na(data$`Analyte 13`))][1],
        AnalyteGroup == "14" ~ data$`Analyte 14`[!(is.na(data$`Analyte 14`))][1],
        AnalyteGroup == "15" ~ data$`Analyte 15`[!(is.na(data$`Analyte 15`))][1]
      )
    )
  data_long$`Study ID` <- study_id
  data_long <- data_long[!is.na(data_long$Value), ]

  final_DTA <- data_long |>
    select(`Sample Index`, `Study ID`, `Sponsor Sample Barcode`, `Subject ID`, `Sex`,
           `Group or Route`, `Treatment or Time Point`, `Matrix`,
           `Collection Date (mm/dd/yyyy)`, `Date Received (mm/dd/yyyy)`,
           `Test Name`, Value, Unit, `LLOQ`, `ULOQ`, `ULOQU`, Notes
    ) |>
    rename(
      `Nextcea Sample ID` = `Sample Index`,
      `Gender` = `Sex`,
      `Group` = `Group or Route`,
      `Visit Name` = `Treatment or Time Point`,
      `Sample Type` = `Matrix`,
      `Sample Collection Date` = `Collection Date (mm/dd/yyyy)`,
      `Nextcea Received Date` = `Date Received (mm/dd/yyyy)`,
      `Test Result` = Value,
      `Test Result Unit` = Unit,
      `Lab Test LLOQ` = `LLOQ`,
      `Lab Test ULOQ` = `ULOQ`,
      `LLOQ/ULOQ Units` = `ULOQU`,
      `Comments` = Notes
    )

  testdate <- read_excel(paste0(test_date, ".xlsx")) |>
    select(`Data Acquisition Date`, `Notes`) |>
    rename(`Sample Test Date` = `Data Acquisition Date`)
  expand_ranges <- function(data) {
    data <- gsub("[^0-9#,-]", "", data)
    data <- unlist(strsplit(data, ","))
    expanded <- lapply(data, function(x) {
      if (grepl("-", x)) {
        range <- as.numeric(str_extract_all(x, "\\d+")[[1]])
        seq(range[1], range[2])
      } else {
        as.numeric(str_extract_all(x, "\\d+")[[1]])
      }
    })
    unlist(expanded)
  }

  expanded_data <- testdate |>
    rowwise() |>
    mutate(`Nextcea Sample ID` = list(expand_ranges(Notes))) |>
    unnest(cols = c(`Nextcea Sample ID`)) |>
    select(`Nextcea Sample ID`, `Sample Test Date`)
  expanded_data$`Nextcea Sample ID` <- as.character(expanded_data$`Nextcea Sample ID`)

  final_DTA <- left_join(final_DTA, expanded_data, by = "Nextcea Sample ID")

  if (version == "basic") {
    final_DTA <- final_DTA |>
      select(`Study ID`, `Sponsor Sample Barcode`, `Subject ID`,
             `Visit Name`, `Sample Type`, `Sample Test Date`,
             `Test Name`, `Test Result`, `Test Result Unit`, Comments)
  } else if (version == "plus") {
    final_DTA <- final_DTA |>
      select(`Nextcea Sample ID`, `Study ID`, `Sponsor Sample Barcode`,
             `Subject ID`, Gender, Group, `Visit Name`, `Sample Type`,
             `Sample Collection Date`, `Nextcea Received Date`,
             `Sample Test Date`, `Test Name`, `Test Result`, `Test Result Unit`,
             `Lab Test LLOQ`, `Lab Test ULOQ`, `LLOQ/ULOQ Units`, Comments)
  }

  write.xlsx(final_DTA, paste0(study_id, "_Final_DTA_", version, ".xlsx"))
}
