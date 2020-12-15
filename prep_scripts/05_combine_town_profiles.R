library(tidyverse)
library(camiller)

################# VARIABLES ##################
acs_year <- 2019
##############################################

town_county <- cwi::xwalk %>%
  distinct(town, county) %>%
  mutate(county = str_remove(county, " County.+$"))

prof18 <- read_csv("https://raw.githubusercontent.com/CT-Data-Haven/2018acs/master/output/5year2018town_profile_expanded_CWS.csv", col_types = cols(.default = "c"))

acs <- read_csv(str_glue("output_data/acs_town_basic_profile_{acs_year}.csv")) %>%
  mutate(across(c(group, level, indicator), as_factor),
         level = fct_relevel(level, "1_state", "3_regions", "2_counties", "4_towns")) %>%
  distinct(name, group, .keep_all = TRUE) %>%
  select(-indicator, -year) %>%
  make_wide(estimate:sharemoe, group = group) %>%
  left_join(town_county, by = c("name" = "town")) %>%
  select(Town = name, County = county, everything(), -level)

cws_df <- read_csv("input_data/cws_2018_all_town_data.csv") %>%
  pivot_wider(names_from = indicator, names_prefix = "cws_")

meta <- read_csv("input_data/website_meta.csv") %>%
  mutate(Town = str_remove(Town, ", Connecticut"))

prof_out <- acs %>%
  left_join(cws_df, by = c("Town" = "name")) %>%
  left_join(meta, by = "Town") %>%
  mutate(Town = str_replace(Town, "(?<= County)$", ", Connecticut")) %>%
  select(Town, County, `Key Facts`, `Wellbeing, Population 18 years and over`,
         # cws indicators
         starts_with("cws_"), `Maximum MoE on above estimates`,
         `Demographic, Total Population`,
         total_pop_estimate:female_sharemoe,
         `Race and Ethnicity, Total Population`,
         hispanic_estimate:other_race_sharemoe,
         `Place of Birth, Total Population`,
         foreign_born_estimate:foreign_born_sharemoe,
         Households,
         total_households_estimate:has_vehicle_sharemoe,
         `Educational Attainment, Population 25 years and over`,
         ages25plus_estimate:bachelors_plus_sharemoe,
         `Median Income`,
         median_household_income_estimate:median_household_income_moe,
         `Poverty and Low-Income, Total Population`,
         poverty_status_determined_estimate:low_income_sharemoe,
         `Poverty and Low-Income, Population 0 to 17 years`,
         ages0_17_poverty_status_determined_estimate:ages0_17_low_income_sharemoe,
         `Poverty and Low-Income, Population 65 years and over`,
         ages65plus_poverty_status_determined_estimate:ages65plus_low_income_sharemoe,
         Source:`Demographic Characteristics`
  ) %>%
  mutate(across(matches("share(moe)?$"), scales::percent, accuracy = 0.1),
         across(matches("^cws_"), scales::percent, accuracy = 1)) %>%
  set_names(names(prof18))

write_csv(prof_out, str_glue("output_data/5year{acs_year}town_profile_expanded_CWS.csv"), na = "")