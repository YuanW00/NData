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

t_url <- "https://na1test.platformforscience.com/Nextcea_Test_28Mar2024/odata/"
p_url <- "https://na1.platformforscience.com/Nextcea+Prod/odata/"
usethis::use_data(t_url, p_url, overwrite = TRUE)
