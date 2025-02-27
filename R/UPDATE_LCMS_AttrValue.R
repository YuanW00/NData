#' LCMS TestDate LLOQ Update
#'
#' Update LCMS Testdate, LLOQ, ULOQ Values
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import lubridate
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @param ept_barcode The Experiment Barcode of samples
#' @param OS_file The data file that is exported from SCIEX OS analyte and saved as .txt
#' @return Return the updated information from PFS
#' @examples
#' UPDATE_LCMS_AttrValue("Test", "user", "password", "LCMS1", "LCMS1OS.txt");
#' @export
UPDATE_LCMS_AttrValue <- function(site, username, password, ept_barcode, OS_file) {
  col_select <- c("Sample Name", "Acquisition Date",
                  "Analyte Name", "Analyte Actual Concentration")

  os <- READ_OS_File(OS_file)
  os_sub <- os |>
    filter(`Sample Type` == "Standard") |>
    select(intersect(col_select, colnames(os))) |>
    filter(str_detect(`Sample Name`, "STD 1|STD 8|STD1|STD8")) |>
    mutate(`Acquisition Date` = as.Date(mdy_hm(`Acquisition Date`))) |>
    filter(`Acquisition Date` == min(`Acquisition Date`)) |>
    distinct()

  os_sub$`Sample Name`[str_detect(os_sub$`Sample Name`, "STD 1|STD1")] <- "LLOQ"
  os_sub$`Sample Name`[str_detect(os_sub$`Sample Name`, "STD 8|STD8")] <- "ULOQ"

  analyte_cols <- paste0("Analyte ", seq(1:15))
  info_table <- data.frame(
    analyte = intersect(colnames(os), analyte_cols),
    test_name = rep(NA, length(intersect(colnames(os), analyte_cols))),
    LLOQ = rep(NA, length(intersect(colnames(os), analyte_cols))),
    ULOQ = rep(NA, length(intersect(colnames(os), analyte_cols)))
  )
  for (i in info_table$analyte) {
    info_table$test_name[info_table$analyte == i] <- unique(os[[i]][!is.na(os[[i]])])
  }
  for (j in info_table$test_name) {
    lloq_value <- os_sub$`Analyte Actual Concentration`[os_sub$`Sample Name` == "LLOQ" & os_sub$`Analyte Name` == j]
    uloq_value <- os_sub$`Analyte Actual Concentration`[os_sub$`Sample Name` == "ULOQ" & os_sub$`Analyte Name` == j]

    if (length(lloq_value) > 0) {
      info_table$LLOQ[info_table$test_name == j] <- lloq_value
    }

    if (length(uloq_value) > 0) {
      info_table$ULOQ[info_table$test_name == j] <- uloq_value
    }
  }

  # Get values from LIMS
  if (site == "Test") {
    url <- paste0("https://na1test.platformforscience.com/Nextcea_Test_28Mar2024/odata/LC_MSMS_EXPERIMENT('", ept_barcode, "')")
  } else if (site == "Production") {
    url <- paste0("https://na1.platformforscience.com/Nextcea+Prod/odata/LC_MSMS_EXPERIMENT('", ept_barcode, "')")
  }
  response <- GET(url, authenticate(username, password))
  data <- fromJSON(content(response, "text"))

  # Update Values
  data$DATE <- unique(os_sub$`Acquisition Date`)
  for (k in info_table$analyte) {
    index <- str_extract(k, "\\d+")
    l_col_name <- paste0("LLOQ_ANALYTE_", index)
    u_col_name <- paste0("ULOQ_ANALYTE_", index)
    data[[l_col_name]] <- ifelse(is.na(as.numeric(info_table$LLOQ[info_table$analyte == k])), data[[l_col_name]], as.numeric(info_table$LLOQ[info_table$analyte == k]))
    data[[u_col_name]] <- ifelse(is.na(as.numeric(info_table$ULOQ[info_table$analyte == k])), data[[u_col_name]], as.numeric(info_table$ULOQ[info_table$analyte == k]))
  }

  # Upload new values
  update_payload <- toJSON(data, auto_unbox = T, null = "null")
  header <- c("Content-Type" = "application/json", "If-Match" = "*")
  put_response <- PUT(url,
                      body = update_payload,
                      authenticate(username, password),
                      add_headers(header))

  if (put_response[["status_code"]] == 200) {
    message = "Updating completed!"
  } else {
    message = "Updating failed!"
  }

  # Get updated values from LIMS
  if (site == "Test") {
    get_url <- paste0("https://na1test.platformforscience.com/Nextcea_Test_28Mar2024/odata/LC_MSMS_EXPERIMENT?$filter=Name%20eq%20'", ept_barcode, "'")
  } else if (site == "Production") {
    get_url <- paste0("https://na1.platformforscience.com/Nextcea+Prod/odata/LC_MSMS_EXPERIMENT?$filter=Name%20eq%20'", ept_barcode, "'")
  }
  new_response <- GET(get_url, authenticate(username, password))
  new_data <- fromJSON(content(new_response, "text"))
  cols <- c("DATE", paste0("LLOQ_ANALYTE_", seq(1:15)), paste0("ULOQ_ANALYTE_", seq(1:15)))
  new_df <- new_data$value |>
    select(intersect(cols, names(new_data$value)))

  result <- list(message, new_df)
  return(result)

}
