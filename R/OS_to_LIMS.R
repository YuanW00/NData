
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
OS_to_LIMS <- function(path, OS_file, Template_file) {
  setwd(path)
  col_select <- c("Sample Name", "Sample Type", "Use Record",
                  "Analyte 1", "Analyte 1 Value", "Analyte 1 Unit", "Update Lot Analyte 1",
                  "Analyte 2", "Analyte 2 Value", "Analyte 2 Unit", "Update Lot Analyte 2",
                  "Analyte 3", "Analyte 3 Value", "Analyte 3 Unit", "Update Lot Analyte 3",
                  "Analyte 4", "Analyte 4 Value", "Analyte 4 Unit", "Update Lot Analyte 4",
                  "Analyte 5", "Analyte 5 Value", "Analyte 5 Unit", "Update Lot Analyte 5",
                  "Analyte 6", "Analyte 6 Value", "Analyte 6 Unit", "Update Lot Analyte 6",
                  "Analyte 7", "Analyte 7 Value", "Analyte 7 Unit", "Update Lot Analyte 7",
                  "Analyte 8", "Analyte 8 Value", "Analyte 8 Unit", "Update Lot Analyte 8",
                  "Analyte 9", "Analyte 9 Value", "Analyte 9 Unit", "Update Lot Analyte 9",
                  "Analyte 10", "Analyte 10 Value", "Analyte 10 Unit", "Update Lot Analyte 10",
                  "Analyte 11", "Analyte 11 Value", "Analyte 11 Unit", "Update Lot Analyte 11",
                  "Analyte 12", "Analyte 12 Value", "Analyte 12 Unit", "Update Lot Analyte 12",
                  "Analyte 13", "Analyte 13 Value", "Analyte 13 Unit", "Update Lot Analyte 13",
                  "Analyte 14", "Analyte 14 Value", "Analyte 14 Unit", "Update Lot Analyte 14",
                  "Analyte 15", "Analyte 15 Value", "Analyte 15 Unit", "Update Lot Analyte 15"
  )
  table_start <- grep("Sample Name", readLines(OS_file))
  os <- read_delim(OS_file, delim = "\t",
                   skip = table_start - 1, col_names = TRUE) |>
    mutate(across(contains("Value"), as.numeric))
  if ("EXPT_SAMPLE_BARCODE" %in% colnames(os)) {
    os <- os |>
      select(-EXPT_SAMPLE_BARCODE)
  }

  os_sub <- os |>
    filter(`Sample Type` == "Unknown") |>
    select(intersect(col_select, colnames(os)))

  # colnames(os_sub) <- gsub("Update Lot Analyte (\\d+)", "Analyte \\1 Update Lot", colnames(os_sub))
  # columns_to_update <- grep("Analyte (\\d+) Update Lot", colnames(os_sub), value = TRUE)
  # os_sub[columns_to_update] <- os_sub$`Use Record`
  # summary(os_sub)

  lims <- read.table(Template_file, header = TRUE, sep = "\t", check.names = FALSE)
  lims <- lims[, colnames(lims) != ""] |>
    select(SAMPLE_NAME_REF,SAMPLE_TYPE_REF,EXPT_SAMPLE_BARCODE) |>
    filter(SAMPLE_TYPE_REF == "NEXTCEA SAMPLE LOT")

  lims <- lims[order(as.numeric(sub("_.*", "", lims$SAMPLE_NAME_REF))), ]
  os_sub <- os_sub[order(as.numeric(sub("_.*", "", os_sub$`Sample Name`))), ]
  name_check <- NULL
  lims$Match_Name <- paste0("Not Match-", sub("_.*", "", lims$SAMPLE_NAME_REF))
  os_sub$Match_Name <- paste0("Not Match-", sub("_.*", "", os_sub$`Sample Name`))

  for (match_index in intersect(lims$Match_Name, os_sub$Match_Name)) {
    # match_index = "Not Match-75"
    i_lims <- which(lims$Match_Name == match_index)
    i_os <- which(os_sub$Match_Name == match_index)
    if (length(i_os) > 1) {
      for (index in i_os) {
        sample_name_extract <- sub("-\\d$", "", lims$SAMPLE_NAME_REF[i_lims])
        if (str_detect(os_sub$`Sample Name`[index], sample_name_extract)) {
          lims$Match_Name[i_lims] <- paste0("Duplicate Sample - ", sample_name_extract)
          os_sub$Match_Name[index] <- paste0("Duplicate Sample - ", sample_name_extract)
        }
      }
      print("Extra samples found in the OS Analyte. Please check 'Duplicate Sample - sample' in the uploaded file.")
    } else {
      sample_name_extract <- sub("-\\d$", "", lims$SAMPLE_NAME_REF[i_lims])
      if (str_detect(os_sub$`Sample Name`[i_os], sample_name_extract)) {
        lims$Match_Name[i_lims] <- sample_name_extract
        os_sub$Match_Name[i_os] <- sample_name_extract
      }
    }
  }

  if (length(lims$SAMPLE_NAME_REF) > length(os_sub$`Sample Name`)) {
    print("Missing samples in the OS Analyte. Please check 'Not Match - sample' in the uploaded file.")
  }

  upload <- full_join(lims, os_sub, by = "Match_Name") |>
    select(Match_Name, everything())

  for (col in names(upload)[str_detect(names(upload), "Value")]) {

    # Extract the prefix (e.g., "Analyte 1", "Analyte 2")
    prefix <- str_extract(col, "Analyte \\d+")

    # Define the corresponding columns to update
    analyte_col <- prefix
    lot_col <- paste0(prefix, " Update Lot")

    # Update the columns based on the condition
    upload[[analyte_col]] <- ifelse(is.na(upload[[col]]), "", upload[[analyte_col]])
    # upload[[lot_col]] <- ifelse(is.na(upload[[col]]), 0, upload[[lot_col]])
  }
  upload[is.na(upload)] <- ""

  output_name <- sub("\\.txt$", "", Template_file)
  write.table(upload, paste0(output_name, "_upload.txt"), sep = "\t", row.names = FALSE, col.names = TRUE)

}


