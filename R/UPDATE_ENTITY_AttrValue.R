#' Entity Atrribute Update
#'
#' Edit Entity Attributes' Value
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import stringr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @param entity The Entity name
#' @param barcode The barcode of the entity
#' @param attributes The vector of attribute names need to be edited
#' @param values The vector of updated values, order matches to attributes
#' @return Return the updated information from PFS
#' @examples
#' UPDATE_ENTITY_AttrValue("Test", "user", "password", "Entity", "entity123", c("A", "B"), c("1", "2"));
#' @export
UPDATE_ENTITY_AttrValue <- function(site, username, password, entity,
                                    barcode, attributes, values) {

  entity <- gsub("\\s+", "_", toupper(entity))

  # Get values from LIMS
  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    url <- paste0(t_url, entity, "('", barcode, "')")
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    url <- paste0(p_url, entity, "('", barcode, "')")
  }
  response <- GET(url, authenticate(username, password))
  data <- fromJSON(content(response, "text"))

  if (!is.null(data$PUBLISHED_DATE)) {
    pub_message <- "This is a published experiment. No uploading."
  } else {
    pub_message <- "This is not a published experiment."
  }

  # Update Values
  message1 <- NULL
  for (i in 1:length(attributes)) {
    col_name <- gsub("\\s+", "_", toupper(attributes[i]))
    col_value <- values[i]
    if (col_name %in% names(data)) {
      data[[col_name]] <- ifelse(is.na(col_value), data[[col_name]], col_value)
    } else {
      message1 <- c(message1, attributes[i])
    }
  }

  # Upload new values
  update_payload <- toJSON(data, auto_unbox = T, null = "null")
  header <- c("Content-Type" = "application/json", "If-Match" = "*")
  put_response <- PUT(url,
                      body = update_payload,
                      authenticate(username, password),
                      add_headers(header))

  if (put_response[["status_code"]] == 200) {
    message2 = "Updating completed!"
  } else {
    message2 = "Updating failed!"
  }

  # Get updated values from LIMS
  if (site == "Test") {
    get_url <- paste0(t_url, entity, "?$filter=Name%20eq%20'", barcode, "'")
  } else if (site == "Production") {
    get_url <- paste0(p_url, entity, "?$filter=Name%20eq%20'", barcode, "'")
  }
  new_response <- GET(get_url, authenticate(username, password))
  new_data <- fromJSON(content(new_response, "text"))
  cols <- gsub("\\s+", "_", toupper(attributes))
  new_df <- new_data$value |>
    select(Name, intersect(cols, names(new_data$value)))

  if (is.null(message1)) {
    result <- list(publish = pub_message,
                   message = message2,
                   df = new_df)
  } else {
    message1 <- paste0("Entity Not Found: ", paste(message1, collapse = ", "))
    result <- list(publish = pub_message,
                   message = c(message1, message2),
                   df = new_df)
  }

  return(result)

}
