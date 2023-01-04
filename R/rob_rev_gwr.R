#' rob_rev_gwr
#'
#' The ouput of the function is a tibble with the buildings characteristics from housing-stat.ch. Either use egid number or the postal address.
#'
#' @param egid: the egid number of the building
#' @param street the street name of the building (not yet available)
#' @param street_nr the street nr of the building (not yet available)
#' @param plz the postal code (not yet available)
#' @return a tibble
#' @import tidyverse rvest
#' @export



# function ----------------------------------------------------------------
rob_rev_gwr <- function(egid) {
# d_egid <- read_csv2("BE.csv")
# egid <- d_egid %>%
#   filter(STRNAME == street & DEINR == street_nr & DPLZ4 == plz) %>%
#   select(EGID) %>%
#   as.numeric()

link <- paste0("https://api.geo.admin.ch/rest/services/ech/MapServer/ch.bfs.gebaeude_wohnungs_register/",
               egid, "_0/extendedHtmlPopup?lang=de")

page <- read_html(link)
gwr <- page %>% html_node("table") %>% html_table()
}
