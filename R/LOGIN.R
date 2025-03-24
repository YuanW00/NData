#' Login
#'
#' Login Info check and Record
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @param site The PFS version need to work on: "Test" or "Production"
#' @param username The username to log in PFS
#' @param password The password to log in PFS
#' @return Return connection result
#' @examples
#' login_PFS(site, username, password);
#' @export
login_PFS <- function(site, username, password) {

  if (site == "Test") {
    data("t_url", package = "NData", envir = environment())
    log_url <- t_url
  } else if (site == "Production") {
    data("p_url", package = "NData", envir = environment())
    log_url <- p_url
  } else {
    print("Wrong Site!")
  }

  response <- GET(log_url, authenticate(username, password))
  if (!http_error(response)) {
    message = "Login Successfully!"
  } else {
    message = http_status(response)$message
  }

  return(message)
}
