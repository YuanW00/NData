#' Result to DTA
#'
#' Convert Sample Lot Result to DTA format
#' @import tidyr
#' @import dplyr
#' @import readxl
#' @import stringr
#' @import tidyverse
#' @param study_id The study id for current study, acquired from LIMS project
#' @param version The expected version of DTA format: "basic", "plus", or "user-defined", "basic" by default
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param project The Project Barcode to upload the analyte result
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @param column_choice The required columns needs to be exported in the report for cilent
#' @return Return result data frame in the required data transfer formatting
#' @examples
#' DTA(study_id = "STD1234", version = "basic", site = "Test", project = "NP123", username = "user", password = "1234")
#' DTA(study_id = "STD1234", version = "plus", site = "Test", project = "NP123", username = "user", password = "1234");
#' @export
DTA <- function (
    study_id, version = "basic",
    site, project, username, password,
    column_choice = c("Study ID", "Sponsor Sample ID",
                      "Subject ID", "Visit Name", "Sample Type",
                      "Sample Test Date", "Test Name", "Test Result",
                      "Test Result Unit", "Comments")
) {

  info_table <- GET_PROJ_ExpInfo(site, project, username, password) |>
    filter(Active == TRUE) |>
    select(`Sample Test Date`, NOTES) |>
    filter(!is.na(NOTES))
  expand_ranges <- function(data) {

    data <- gsub("[^0-9a-zA-Z#,-]", "", data)
    data <- unlist(strsplit(data, ","))

    expanded <- lapply(data, function(x) {
      x <- gsub("#", "", x)
      if (grepl("-", x)) {
        parts <- unlist(strsplit(x, "-"))
        start_num <- as.numeric(str_extract(parts[1], "\\d+"))
        start_letter <- str_extract(parts[1], "[a-zA-Z]+$")
        end_num <- as.numeric(str_extract(parts[2], "\\d+"))
        end_letter <- str_extract(parts[2], "[a-zA-Z]+$")

        if (is.na(start_letter) && is.na(end_letter)) {
          as.character(seq(start_num, end_num))
        } else if (identical(start_letter, end_letter)) {
          paste0(seq(start_num, end_num), start_letter)
        } else if (is.na(start_letter) && !is.na(end_letter)) {
          paste0(seq(start_num, end_num), end_letter)
        } else if (!is.na(start_letter) && is.na(end_letter)) {
          paste0(seq(start_num, end_num), start_letter)
        } else {
          warning(paste("Letters in range", x, "are not the same. Check your input."))
          paste0(seq(start_num, end_num), start_letter)
        }
      } else {
        as.character(x)
      }
    })

    unlist(expanded) |> as.character()
  }

  expanded_data <- info_table |>
    rowwise() |>
    mutate(sample_prefix = list(expand_ranges(NOTES))) |>
    unnest(cols = c(sample_prefix)) |>
    select(-NOTES) |>
    select(sample_prefix, everything())
  expanded_data$sample_prefix <- as.character(expanded_data$sample_prefix)

  sample_lot <- GET_PROJ_SampleLot(site, project, username, password) |>
    filter(is.na(NOTES) | !str_detect(NOTES, "ISR"))
  sample_lot$sample_prefix <- sub("_.*", "", sample_lot$Name)

  data <- left_join(sample_lot, expanded_data, by = "sample_prefix")

  colnames(data)[colnames(data) == "Analyte_Result_Comment"] <- "Analysis Result Comment"

  data_long <- data |>
    pivot_longer(
      cols = starts_with("ANALYTE_"),
      names_to = c("AnalyteGroup", ".value"),
      names_pattern = "ANALYTE_(\\d+)_(.+)"
    )

  data_long <- data_long |>
    mutate(
      `Test Name` = case_when(
        AnalyteGroup == "1" ~ data$`ANALYTE_1`[!(is.na(data$`ANALYTE_1`))][1],
        AnalyteGroup == "2" ~ data$`ANALYTE_2`[!(is.na(data$`ANALYTE_2`))][1],
        AnalyteGroup == "3" ~ data$`ANALYTE_3`[!(is.na(data$`ANALYTE_3`))][1],
        AnalyteGroup == "4" ~ data$`ANALYTE_4`[!(is.na(data$`ANALYTE_4`))][1],
        AnalyteGroup == "5" ~ data$`ANALYTE_5`[!(is.na(data$`ANALYTE_5`))][1],
        AnalyteGroup == "6" ~ data$`ANALYTE_6`[!(is.na(data$`ANALYTE_6`))][1],
        AnalyteGroup == "7" ~ data$`ANALYTE_7`[!(is.na(data$`ANALYTE_7`))][1],
        AnalyteGroup == "8" ~ data$`ANALYTE_8`[!(is.na(data$`ANALYTE_8`))][1],
        AnalyteGroup == "9" ~ data$`ANALYTE_9`[!(is.na(data$`ANALYTE_9`))][1],
        AnalyteGroup == "10" ~ data$`ANALYTE_10`[!(is.na(data$`ANALYTE_10`))][1],
        AnalyteGroup == "11" ~ data$`ANALYTE_11`[!(is.na(data$`ANALYTE_11`))][1],
        AnalyteGroup == "12" ~ data$`ANALYTE_12`[!(is.na(data$`ANALYTE_12`))][1],
        AnalyteGroup == "13" ~ data$`ANALYTE_13`[!(is.na(data$`ANALYTE_13`))][1],
        AnalyteGroup == "14" ~ data$`ANALYTE_14`[!(is.na(data$`ANALYTE_14`))][1],
        AnalyteGroup == "15" ~ data$`ANALYTE_15`[!(is.na(data$`ANALYTE_15`))][1]
      )
    )

  data_long$`Study ID` <- study_id
  data_long <- data_long[!is.na(data_long$VALUE), ]

  range_table <- GET_PROJ_Range(site="Test", username, password) |>
    group_by(`TEST_NAME`, MATRIX) |>
    mutate(duplicate = n() > 1,
           is103 = grepl("103", ASSAY)) |>
    mutate(prevail = case_when(
      duplicate == F | (duplicate == T & is103 == T) ~ T,
      (duplicate == T & is103 == F) ~ F
    ),
    other = case_when(
      is103 == F ~ T,
      is103 == T ~ F
    )) |>
    select(-c(duplicate, is103))

  client <- sub("_.*", "", unique(sample_lot$PROJECT_Name))
  test_name <- unique(data_long$`Test Name`)
  species <- unique(sample_lot$NXC_SPECIES)

  if (client == 103) {
    proj_range <- range_table |>
      filter(Active == TRUE) |>
      filter(prevail == T) |>
      filter(SPECIES %in% species) |>
      filter(TEST_NAME %in% test_name) |>
      select(TEST_NAME, SPECIES, MATRIX, LLOQ, ULOQ, ULOQU) |>
      rename(NXC_MATRIX = MATRIX,
             NXC_SPECIES = SPECIES,
             `Test Name` = TEST_NAME)
  } else {
    proj_range <- range_table |>
      filter(Active == TRUE) |>
      filter(other == T) |>
      filter(SPECIES %in% species) |>
      filter(TEST_NAME %in% test_name) |>
      select(TEST_NAME, SPECIES, MATRIX, LLOQ, ULOQ, ULOQU)|>
      rename(NXC_MATRIX = MATRIX,
             NXC_SPECIES = SPECIES,
             `Test Name` = TEST_NAME)
  }

  data_long_p <- left_join(data_long, proj_range,
                           by = c("NXC_MATRIX", "NXC_SPECIES", "Test Name"))

  final_DTA <- data_long_p |>
    select(`NXC_SAMPLE_INDEX`, `Study ID`, NXC_SPONSOR_SAMPLE_ID, `SPONSOR_SAMPLE_BARCODE`,
           `NXC_SUBJECT`, `NXC_SEX`,
           `NXC_GROUP`, `NXC_TREATMENT`, `NXC_MATRIX`,
           `NXC_COLLECTION_DATE`, `DATE_RECEIVED`, `Sample Test Date`,
           `Test Name`, VALUE, UNIT, LLOQ, ULOQ, ULOQU, NOTES
    ) |>
    rename(
      `Nextcea Sample ID` = `NXC_SAMPLE_INDEX`,
      `Sponsor Sample ID` = NXC_SPONSOR_SAMPLE_ID,
      `Sponsor Sample Barcode` = `SPONSOR_SAMPLE_BARCODE`,
      `Subject ID` = `NXC_SUBJECT`,
      `Gender` = `NXC_SEX`,
      `Group` = `NXC_GROUP`,
      `Visit Name` = `NXC_TREATMENT`,
      `Sample Type` = `NXC_MATRIX`,
      `Sample Collection Date` = `NXC_COLLECTION_DATE`,
      `Nextcea Received Date` = `DATE_RECEIVED`,
      `Test Result` = VALUE,
      `Test Result Unit` = UNIT,
      `Lab Test LLOQ` = LLOQ,
      `Lab Test ULOQ` = ULOQ,
      `LLOQ/ULOQ Units` = ULOQU,
      `Comments` = NOTES
    )

  if (version == "basic") {
    final_DTA <- final_DTA |>
      select(`Study ID`, `Sponsor Sample ID`, `Subject ID`,
             `Visit Name`, `Sample Type`, `Sample Test Date`,
             `Test Name`, `Test Result`, `Test Result Unit`, Comments)
  } else if (version == "plus") {
    final_DTA <- final_DTA |>
      select(`Nextcea Sample ID`, `Study ID`, `Sponsor Sample ID`, `Sponsor Sample Barcode`,
             `Subject ID`, Gender, Group, `Visit Name`, `Sample Type`,
             `Sample Collection Date`, `Nextcea Received Date`,
             `Sample Test Date`, `Test Name`, `Test Result`, `Test Result Unit`,
             `Lab Test LLOQ`, `Lab Test ULOQ`, `LLOQ/ULOQ Units`, Comments)
  } else if (version == "user-defined") {
    final_DTA <- final_DTA |>
      select(any_of(column_choice))
  }

  message_result <- "Processing complete. Results are displayed below."

  return(
    list(
      message = message_result,
      lot_length = message1,
      final_df = as.data.frame(final_DTA)
      )
    )

}
