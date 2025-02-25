#' OS Result Upload
#'
#' Convert OS file to LIMS LCMS template format
#' @import dplyr
#' @import readr
#' @param OS_file The data file that is exported from SCIEX OS analyte and saved as .txt
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param ept_barcode The Experiment Barcode of samples
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return two data frames: one is alert data frame if there is any missing or unmatched samples and the other one is the result data frame with analyte result filled in the LIMS template
#' @examples
#' OS_to_LIMS("OS.txt", "Test", "LCMS1", "user", "password");
#' @export
OS_to_LIMS <- function(OS_file, site, ept_barcode, username, password) {

  col_select <- c("Sample Name", "Sample Type", "Use Record",
                  paste0("Analyte ", seq(1:15)),
                  paste0("Analyte ", seq(1:15), " Value"),
                  paste0("Analyte ", seq(1:15), " Unit")
  )

  os <- READ_OS_File(OS_file)
  if ("EXPT_SAMPLE_BARCODE" %in% colnames(os)) {
    os <- os |>
      select(-EXPT_SAMPLE_BARCODE)
  }

  os_sub <- os |>
    filter(`Sample Type` == "Unknown") |>
    select(intersect(col_select, colnames(os)))

  lims <- GET_LCMS_SampleBarcodes(site, ept_barcode, username, password) |>
    select(-Active)

  lims$order <- gsub("\\D", "", sub("_.*", "", lims$SAMPLE_NAME_REF))
  os_sub$order <- sub("_.*", "", os_sub$`Sample Name`)
  os_sub <- os_sub[!grepl("eQC", os_sub$order), ]
  os_sub$order <- gsub("\\D", "", sub("_.*", "", os_sub$order))
  lims <- lims[order(as.numeric(lims$order)), ]
  os_sub <- os_sub[order(as.numeric(os_sub$order)), ]
  name_check <- NULL
  lims$Match_Name <- paste0("Not Match-", sub("_.*", "", lims$SAMPLE_NAME_REF))
  os_sub$Match_Name <- paste0("Not Match-", sub("_.*", "", os_sub$`Sample Name`))

  alert_df <- data.frame()

  for (match_index in intersect(lims$Match_Name, os_sub$Match_Name)) {
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
      alert_df <- rbind(alert_df, data.frame(Alert_Check_Result = "Alert 1", Content = "Duplicate samples found. Please check 'Duplicate Sample - sample' in the uploaded file."))
      # print("Duplicate samples found. Please check 'Duplicate Sample - sample' in the uploaded file.")
    } else {
      sample_name_extract <- sub("-\\d$", "", lims$SAMPLE_NAME_REF[i_lims])
      if (str_detect(os_sub$`Sample Name`[i_os], sample_name_extract)) {
        lims$Match_Name[i_lims] <- sample_name_extract
        os_sub$Match_Name[i_os] <- sample_name_extract
      }
    }
  }

  # Check if the sample index is sequential
  lims$Sequence <- sub("_.*$", "", lims$SAMPLE_NAME_REF)
  lims <- lims %>%
    mutate(Sequence = as.numeric(str_extract(Sequence, "\\d+")))
  expected_sequence <- seq(min(lims$Sequence), max(lims$Sequence))
  lims_missing <- setdiff(expected_sequence, lims$Sequence)

  if (length(lims_missing) == 0) {
    alert_df <- rbind(alert_df, data.frame(Alert_Check_Result = "Normal", Content = "The sample is sequential."))
    # print("The sample is sequential.")
  } else {
    alert_df <- rbind(alert_df, data.frame(Alert_Check_Result = "Alert 2", Content = paste0("The samples are not sequential in the LIMS template and missing sample index are ", paste(lims_missing, collapse = ", "))))
    # print(paste0("The samples are not sequential in the LIMS template and missing sample index are ", paste(lims_missing, collapse = ", ")))
  }

  for (missing in lims_missing) {
    missing_match_name <- paste0("Not Match-", missing)
    if (!(missing_match_name %in% lims$Match_Name)) {
      missing_data <- data.frame(
        Match_Name = paste0("Not Match-", missing),
        SAMPLE_NAME_REF = rep(NA, 1),
        SAMPLE_TYPE_REF = rep(NA, 1),
        EXPT_SAMPLE_BARCODE = rep(NA, 1),
        Sequence = rep(NA, 1),
        order = missing
      )
      lims <- rbind(lims, missing_data)
    }
  }

  upload <- full_join(lims, os_sub, by = "Match_Name") |>
    select(Match_Name, everything()) |>
    select(-Sequence)

  # Check if there is missing sample in the OS file
  if (any(str_detect(upload$Match_Name, "Not Match") & is.na(upload$EXPT_SAMPLE_BARCODE))) {
    alert_df <- rbind(alert_df,
                      data.frame(Alert_Check_Result = "Alert 3",
                                 Content = "Missing samples found in the LIMS template. Please check 'Not Match - sample' in the uploaded file.")
                      )
    # print("Missing samples found in the LIMS template. Please check 'Not Match - sample' in the uploaded file.")
  }

  if (any(str_detect(upload$Match_Name, "Not Match") & !is.na(upload$EXPT_SAMPLE_BARCODE))) {
    alert_df <- rbind(alert_df,
                      data.frame(Alert_Check_Result = "Alert 4",
                                 Content = "Missing samples found in the OS result file. Please check 'Not Match - sample' in the uploaded file.")
                      )
    # print("Missing samples found in the OS result file. Please check 'Not Match - sample' in the uploaded file.")
  }

  for (col in names(upload)[str_detect(names(upload), "Value")]) {
    prefix <- str_extract(col, "Analyte \\d+")
    analyte_col <- prefix
    lot_col <- paste0(prefix, " Update Lot")
    upload[[analyte_col]] <- ifelse(is.na(upload[[col]]), "", upload[[analyte_col]])
  }

  upload[is.na(upload)] <- ""
  upload <- upload |>
    select(-c(order.x, order.y))

  result <- list(
    alert = alert_df,
    result_dataset = upload
  )
  return(result)
  # output_name <- sub("\\.txt$", "", Template_file)
  # write.table(upload, paste0(output_name, "_upload.txt"), sep = "\t", row.names = FALSE, col.names = TRUE)

}
