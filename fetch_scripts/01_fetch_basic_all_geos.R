library(purrr)
library(cwi)

##################### VARIABLES ############
year <- 2019
############################################

# regions_short <- cwi::regions[c("Greater New Haven", "Greater Waterbury", "Greater Bridgeport", "Lower Naugatuck Valley", "Greater Hartford")]
regions2 <- cwi::regions[stringr::str_subset(names(cwi::regions), "^\\D.+(County|cities)$", negate = TRUE)]

fetch <- purrr::map(basic_table_nums, 
                    ~multi_geo_acs(table = ., year = year, towns = "all", regions = regions2, tracts = "all"))

saveRDS(fetch, stringr::str_glue("./fetch_data/acs_basic_{year}_fetch_towns.rds"))

