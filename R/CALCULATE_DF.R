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
  while (TRUE) {
    response <- GET(ref_url, authenticate(username, password))
    data <- fromJSON(content(response, "text"))
    df <- data$value |>
      filter(Active == TRUE) |>
      filter(NXC_MATRIX == matrix) |>
      filter(NXC_SPECIES == species) |>
      select(SPECIES, NXC_SPECIES, NXC_MATRIX, ASSAY, ANALYTE_12_VALUE, ANALYTE_13_VALUE) |>
      rename(
        Type = SPECIES,
        Species = NXC_SPECIES,
        Matrix = NXC_MATRIX,
        Analyte = ASSAY,
        Area_Ratio_REF = ANALYTE_12_VALUE,
        Slope_REF = ANALYTE_13_VALUE
      ) |>
      distinct()
    ref_table <- as.data.frame(rbind(ref_table, df))
    if (!is.null(data[["@odata.nextLink"]]) ) {
      url <- data[["@odata.nextLink"]]
    } else {
      break
    }
  }

  lines <- readLines(OS_file)
  target_slope <- paste0("Peak Name: ", analyte)
  start_index <- grep(paste0("^", target_slope, "$"), lines)
  if (length(start_index) > 0) {
    slope_line <- grep("^Slope", lines[start_index:length(lines)], value = TRUE)[1]
    slope_value <- as.numeric(sub(".*Slope\\s+", "", slope_line))
    # transition_line <- grep("^Extraction: ", lines[start_index:length(lines)], value = TRUE)[1]
    # transition_value <- sub(".*:\\s*", "", transition_line)
    message1 <- paste0("Slope Extracted as ", slope_value)
  } else {
    message1 <- "Target peak name not found in the file."
  }

  os <- READ_OS_File(OS_file)
  col_need <- c("Sample Name", "Sample Type", "Analyte Peak Name", "Area Ratio",
                "Dilution Factor")
  data <- os |>
    filter(`Sample Type`=="Unknown") |>
    filter(`Analyte Peak Name` == analyte) |>
    select(intersect(col_need, colnames(os))) |>
    rename(Analyte = `Analyte Peak Name`)

  eQC <- data |>
    filter(str_detect(`Sample Name`, "eQC")) |>
    group_by(`Sample Name`) |>
    summarise(Analyte = unique(Analyte),
              Matrix = matrix,
              AVE_PAR = mean(as.numeric(`Area Ratio`)),
              Slope = slope_value) |>
    mutate(Type = str_extract(`Sample Name`, "^eQC\\d+"))

  eQC <- left_join(eQC, ref_table, by = c("Analyte", "Type", "Matrix"))
  eQC$area_ratio <- eQC$Area_Ratio_REF/eQC$AVE_PAR
  eQC$slope_ratio <- eQC$Slope/eQC$Slope_REF
  eQC <- eQC |>
    mutate(eQC_DF = mean(area_ratio)*slope_ratio)

  test <- data |>
    filter(!str_detect(`Sample Name`, "eQC")) |>
    rename(Actual_Sample_DF = `Dilution Factor`) |>
    group_by(Actual_Sample_DF)|>
    summarise(Analyte = analyte,
              Matrix = matrix,
              Slope = slope_value,
              Slope_REF = unique(ref_table$Slope_REF)
    )
  test$Test_Sample_DF <- test$Slope/test$Slope_REF*as.numeric(test$Actual_Sample_DF)

  result <- list(message = message1,
                 eQC_DF = eQC,
                 Test_Sample_DF = test)
  return(result)
}
