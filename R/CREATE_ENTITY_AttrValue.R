#' Entity Record Create
#'
#' Create Entity Record with Attributes' Value
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import stringr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @param entity The Entity name
#' @param body Values in a list to fill the Entity attribute
#' @return Return POST result message
#' @examples
#' CREATE_ENETITY_AttrValue("Test", "user", "password", Entity, body);
#' @export
CREATE_ENETITY_AttrValue <- function(site, username, password, entity, body) {
  entity <- gsub("\\s+", "_", toupper(entity))

  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    url <- paste0(t_url, entity)
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    url <- paste0(p_url, entity)
  }

  header <- c("Content-Type" = "application/json")
  create_payload <- toJSON(body, auto_unbox = T, null = "null")
  post_response <- POST(url,
                        body = create_payload,
                        authenticate(username, password),
                        add_headers(header))


  if (!http_error(post_response)) {
    entity_id <- content(post_response, as = "parsed")$Barcode
    message = paste0(entity_id, " - Record Created!")

    get_url <- paste0(url, "?$filter=Barcode%20eq%20'", entity_id, "'")
    new_response <- GET(get_url, authenticate(username, password))
    new_data <- fromJSON(content(new_response, "text"))$value

  } else {
    message = http_status(post_response)$message
    new_data <- NULL
  }

  result <- list(
    message = message,
    newdf = new_data
  )
  return(result)
}
