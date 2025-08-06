#' Result to DTA
#'
#' Convert Sample Lot Result to DTA format
#' @import tidyr
#' @import dplyr
#' @import readxl
#' @import stringr
#' @import tidyverse
#' @param study_id The study id for current study, acquired from LIMS project
#' @param format The expected format of DTA: "landscape" or "vertical", "landscape" by default
#' @param version The expected version of DTA format: "basic", "plus", or "user-defined", "basic" by default
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param project The Project Barcode to upload the analyte result
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @param column_choice The required columns needs to be exported in the report for cilent
#' @return Return result data frame in the required data transfer formatting
#' @examples
#' DTA(study_id = "STD1234", format = "vertical", version = "basic", site = "Test", project = "NP123", username = "user", password = "1234")
#' DTA(study_id = "STD1234", version = "plus", site = "Test", project = "NP123", username = "user", password = "1234");
#' @export
DTA <- function (
    study_id, format = "landscape", version = "basic",
    site, project, username, password,
    column_choice = c("Study ID", "Sponsor Sample ID",
                      "Subject ID", "Visit Name", "Sample Type",
                      "Sample Test Date", "Test Name", "Test Result",
                      "Test Result Unit", "Comments")
) {

  info_table <- GET_PROJ_ExpInfo(site, project, username, password) |>
    filter(Active == TRUE) |>
    select(`Sample Test Date`, sample_index_note) |>
    filter(!is.na(sample_index_note))
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
    mutate(sample_prefix = list(expand_ranges(sample_index_note))) |>
    unnest(cols = c(sample_prefix)) |>
    select(-sample_index_note) |>
    select(sample_prefix, everything())
  expanded_data$sample_prefix <- as.character(expanded_data$sample_prefix)

  sample_lot <- GET_PROJ_SampleLot(site, project, username, password)
  message1 <- paste0("Total number of sample lot list: ", length(sample_lot$NXC_SAMPLE_INDEX))

  sample_lot <- sample_lot |>
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

  analyte_num <- data_long |>
    distinct(AnalyteGroup, `Test Name`)

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
    select(`NXC_SAMPLE_INDEX`, `Study ID`, NXC_SPONSOR_SAMPLE_ID,
           `SPONSOR_SAMPLE_BARCODE`,
           `NXC_SUBJECT`, `NXC_SEX`,
           `NXC_GROUP`, `NXC_TREATMENT`, `NXC_MATRIX`,
           `NXC_COLLECTION_DATE`, `NXC_COLLECTION_TIME_INTERVAL`,
           `DATE_RECEIVED`, `Sample Test Date`,
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
      `Sample Collection Time Interval` = `NXC_COLLECTION_TIME_INTERVAL`, 
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
             `Sample Collection Date`, `Sample Collection Time Interval`, `Nextcea Received Date`,
             `Sample Test Date`, `Test Name`, `Test Result`, `Test Result Unit`,
             `Lab Test LLOQ`, `Lab Test ULOQ`, `LLOQ/ULOQ Units`, Comments)
  } else if (version == "user-defined") {
    final_DTA <- final_DTA |>
      select(any_of(column_choice))
  }

  message_result <- "Processing complete. Results are displayed below."

  if (format == "landscape") {

    data_numbered <- left_join(final_DTA, analyte_num, by = "Test Name")
    col_convert <- c("Test Name", "Test Result", "Test Result Unit", "Lab Test LLOQ",
                     "Lab Test ULOQ", "LLOQ/ULOQ Units")
    land_DTA <- pivot_wider(data_numbered,
                            names_from = "AnalyteGroup",
                            values_from = intersect(col_convert, colnames(final_DTA)),
                            names_glue = "{.value} {`AnalyteGroup`}",
                            names_sort = TRUE)

    col_convert_expanded <- as.vector(
      sapply(1:15, function(x) paste(col_convert, x))
    )
    cols <- c("Nextcea Sample ID", "Study ID", "Sponsor Sample ID",
              "Sponsor Sample Barcode", "Subject ID", "Gender", "Group",
              "Visit Name", "Sample Type", "Sample Collection Date",
              "Nextcea Received Date", "Sample Test Date", col_convert_expanded,
              "Comments"
    )

    land_DTA <- land_DTA |>
      select(all_of(intersect(cols, colnames(land_DTA))))

    rename_test_cols <- function(df) {
      df_new <- df
      # df_new <- land_DTA
      test_name_cols <- grep("^Test Name \\d+$", names(df_new), value = TRUE)

      for (test_col in test_name_cols) {
        # test_col <- test_name_cols[1]
        test_num <- gsub("Test Name ", "", test_col)

        analyte_name <- unique(df_new[[test_col]])
        analyte_name <- analyte_name[!is.na(analyte_name)][1]

        if (!is.na(analyte_name) && nzchar(analyte_name)) {
          df_new <- df_new[, names(df_new) != test_col]

          names(df_new) <- gsub(
            paste0("^(Test|Lab Test) (.*?) ", test_num, "$"),
            paste0(analyte_name, " \\2"),
            names(df_new)
          )

          names(df_new) <- gsub(
            paste0("^LLOQ/ULOQ Units ", test_num, "$"),
            paste0(analyte_name, " LLOQ/ULOQ Units"),
            names(df_new)
          )
        }
      }
      return(df_new)
    }

    land_DTA <- rename_test_cols(land_DTA)

    result <- list(
      message = message_result,
      lot_length = message1,
      final_df = as.data.frame(land_DTA)
    )

  } else if (format == "vertical") {
    result <- list(message = message_result,
                   lot_length = message1,
                   final_df = as.data.frame(final_DTA)
    )
  }

  return(result)

}
