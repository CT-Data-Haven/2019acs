library(purrr)
library(cwi)

##################### VARIABLES ############
year <- 2019
############################################

# regions_short <- cwi::regions[c("Greater New Haven", "Greater Waterbury", "Greater Bridgeport", "Lower Naugatuck Valley", "Greater Hartford")]
regions2 <- cwi::regions[stringr::str_subset(names(cwi::regions), "^\\D.+(County|cities)$", negate = TRUE)]

fetch <- purrr::map(basic_table_nums, function(num) {
  dplyr::bind_rows(
    multi_geo_acs(table = num, year = year, towns = "all", regions = regions2, tracts = "all"),
    tidycensus::get_acs("public use microdata area", table = num, year = year, state = "09") %>%
      janitor::clean_names() %>%
      dplyr::select(-name) %>%
      dplyr::mutate(level = "9_puma", year = year, name = geoid)
  )
})

saveRDS(fetch, stringr::str_glue("./fetch_data/acs_basic_{year}_fetch_towns.rds"))
