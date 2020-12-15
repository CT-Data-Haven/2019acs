library(purrr)
library(dplyr)
library(cwi)

##################### VARIABLES ############
year <- 2019
############################################

# this gets all the data for neighborhood profiles for New Haven, Hartford/West Hartford, Bridgeport, and Stamford. 
# Hierarchy:
# State, Greater New Haven, Fairfield County, cities, all tracts in cities
# separately, do all block groups in New Haven for tables where available
regions_short <- cwi::regions[c("Greater New Haven", "Greater Hartford")]
cities <- c("New Haven", "Bridgeport", "Hartford", "West Hartford", "Stamford")

nhood_lookup <- tibble::lst(bridgeport_tracts, hartford_tracts, stamford_tracts, nhv_tracts) %>%
  set_names(stringr::str_remove, "_tracts$") %>%
  bind_rows(.id = "city") %>%
  tidyr::unite(name, city, name) %>%
  select(name, geoid, weight)

fetch <- map(basic_table_nums,
             ~multi_geo_acs(., year = year, towns = cities, regions = regions_short, neighborhoods = nhood_lookup)) %>%
  modify_at("median_income", function(df) {
    df %>%
      mutate(across(estimate:moe, ~ifelse(stringr::str_detect(level, "(neighborhoods|regions)"), NA, .)))
  })

saveRDS(fetch, stringr::str_glue("./fetch_data/acs_basic_{year}_fetch_nhoods.rds"))

############ block groups, only New Haven

fetch_bg <- map(basic_table_nums,
                ~multi_geo_acs(., year = year, towns = "New Haven", neighborhoods = nhv_bgrps))

saveRDS(fetch_bg, stringr::str_glue("./fetch_data/acs_basic_{year}_fetch_nhv.rds"))





