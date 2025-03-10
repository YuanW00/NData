# # update project_page data
# #
# library(httr)
# library(dplyr)
# library(purrr)
# library(tidyr)
# library(jsonlite)
# library(NData)
#
# username <- "ywang"
# password <- "0809Ann@"
#
# site <- "Test"
# test <- SAVE_PROJ_LotPage(site, username, password)
#
# site <- "Production"
# prod <- SAVE_PROJ_LotPage(site, username, password)
#
# usethis::use_data(test, prod, overwrite = TRUE, internal = TRUE)
# usethis::use_data(test, prod, overwrite = TRUE)

