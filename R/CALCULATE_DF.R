#' eQC/Test Sample DF Calculator
#'
#' Calculate normalized dilution factor for different batch
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import stringr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @param analyte Target analyte name
#' @param species Species of the experiment
#' @param matrix Matrix of the experiment
#' @param OS_file Experiment OS result
#' @return Return two data frame including calculated dilution factor
#' @examples
#' CALCULATE_DF(site, username, password, analyte, species, matrix, OS_file)
#' @export
CALCULATE_DF <- function (site, username, password, analyte, species, matrix, OS_file) {

  suffix <- paste0("EQC_REFERENCE?$filter=ASSAY%20eq%20'", analyte, "'")
  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    ref_url <- paste0(t_url, suffix)
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    ref_url <- paste0(p_url, suffix)
  } else {
    print("Wrong Site!")
  }

  ref_table <- NULL
  for (i in ref_url) {
    # i = ref_url[1]
    i <- modify_url(i)
    while (TRUE) {
      response <- GET(i, authenticate(username, password))
      data <- fromJSON(content(response, "text"))
      df <- data$value |>
        filter(Active == TRUE) |>
        filter(NXC_MATRIX == matrix) |>
        filter(NXC_SPECIES == species) |>
        select(EQC_LEVEL, NXC_SPECIES, NXC_MATRIX, ASSAY, ANALYTE_12_VALUE, ANALYTE_13_VALUE) |>
        rename(
          Type = EQC_LEVEL,
          Species = NXC_SPECIES,
          Matrix = NXC_MATRIX,
          Analyte = ASSAY,
          Area_Ratio_REF = ANALYTE_12_VALUE,
          Slope_REF = ANALYTE_13_VALUE
        ) |>
        distinct()
      ref_table <- as.data.frame(rbind(ref_table, df))
      if (!is.null(data[["@odata.nextLink"]]) ) {
        i <- data[["@odata.nextLink"]]
      } else {
        break
      }
    }
  }

  lines <- readLines(OS_file)
  slope_df <- NULL
  message1 <- NULL
  for (i in analyte) {
    target <- paste0("Peak Name: ", i)
    start_index <- which(lines == target)
    if (length(start_index) > 0) {
      slope_line <- grep("^Slope", lines[start_index:length(lines)], value = TRUE)[1]
      slope_value <- as.numeric(sub(".*Slope\\s+", "", slope_line))
      # transition_line <- grep("^Extraction: ", lines[start_index:length(lines)], value = TRUE)[1]
      # transition_value <- sub(".*:\\s*", "", transition_line)
      df <- data.frame(Analyte = i,
                       Slope = slope_value)
      slope_df <- rbind(slope_df, df)
      message1 <- c(message1, paste0(i, ": slope found; "))
    } else {
      message1 <- c(message1,  paste0(i, ": slope not found; "))
    }
  }

  os <- READ_OS_File(OS_file)
  col_need <- c("Use Record", "Sample Name", "Sample Type", "Analyte Peak Name", "Area Ratio",
                "Dilution Factor", "Calculated Concentration", "Calculated Concentration (ng/mL)")
  data <- os |>
    filter(`Sample Type`=="Unknown") |>
    filter(`Analyte Peak Name` %in% analyte) |>
    select(intersect(col_need, colnames(os))) |>
    filter(`Use Record` == 1) |>
    rename(Analyte = `Analyte Peak Name`)

  calc_col <- grep("Calculated Concentration", names(data), value = TRUE)
  names(data)[names(data) == calc_col] <- "Original_eQC_Value"

  eQC <- data |>
    filter(str_detect(`Sample Name`, "eQC")) |>
    group_by(`Sample Name`, Analyte) |>
    mutate(Type = str_extract(`Sample Name`, "^eQC\\d+"),
           Matrix = matrix,
           AVE_PAR = mean(as.numeric(`Area Ratio`)))

  eQC <- left_join(eQC, slope_df, by = "Analyte")
  eQC <- left_join(eQC, ref_table, by = c("Analyte", "Type", "Matrix"))
  eQC$area_ratio <- eQC$Area_Ratio_REF/eQC$AVE_PAR
  eQC$slope_ratio <- eQC$Slope/eQC$Slope_REF
  eQC <- eQC |>
    group_by(Analyte) |>
    mutate(eQC_Factor = round(mean(area_ratio)*mean(slope_ratio), 3))

  eQC$Updated_eQC_Value <- round(eQC$eQC_Factor*as.numeric(eQC$Original_eQC_Value), 3)
  eQC <- eQC |>
    arrange(Analyte)

  test <- data |>
    filter(!str_detect(`Sample Name`, "eQC")) |>
    rename(Actual_Sample_DF = `Dilution Factor`) |>
    group_by(Actual_Sample_DF, Analyte)|>
    summarise(Matrix = matrix,
              # Slope = slope_value
              #Slope_REF = unique(ref_table$Slope_REF)
    )
  test <- left_join(test, slope_df, by = "Analyte")
  ref_table_test <- ref_table |>
    select(-Type, -Area_Ratio_REF) |>
    distinct()
  test <- left_join(test, ref_table_test, by = c("Analyte", "Matrix")) |>
    select(Actual_Sample_DF, Analyte, Species, Matrix, Slope, Slope_REF)
  test$Test_Sample_Factor <- round(test$Slope/test$Slope_REF*as.numeric(test$Actual_Sample_DF), 3)

  test <- test |>
    arrange(Analyte)

  result <- list(message = message1,
                 eQC_DF = eQC,
                 Test_Sample_DF = test)
  return(result)
}
