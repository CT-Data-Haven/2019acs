library(tidyverse)

year <- 2019

dps <- jsonlite::fromJSON(str_glue("https://api.census.gov/data/{year}/acs/acs5/profile/variables.json"), simplifyVector = TRUE, flatten = TRUE) %>%
  flatten() %>%
  map_dfr(as_tibble, .id = "variable") %>%
  filter(str_detect(group, "^DP\\d{2}$")) %>%
  distinct(group, concept) %>%
  mutate(concept = str_to_title(concept) %>%
           str_remove("^Selected") %>%
           str_remove("In The United States") %>%
           str_trim() %>%
           recode(`Acs Demographic And Housing Estimates` = "Demographic Characteristics")) %>%
  arrange(group)

geos <- openxlsx::read.xlsx(str_glue("https://www2.census.gov/programs-surveys/popest/geographies/{year}/all-geocodes-v{year}.xlsx"), startRow = 5) %>%
  as_tibble() %>%
  rename_with(~str_remove(., "\\(.+$")) %>%
  janitor::clean_names() %>% 
  rename_with(~str_remove(., "_code.*$")) %>%
  filter(state == "09", summary_level %in% c("040", "050", "061")) %>%
  mutate(area_name = str_remove(area_name, " town") %>%
           str_replace(" County", " County, Connecticut"),
         summary_level = str_sub(summary_level, 1, 2) %>% str_pad(7, "right", "0")) %>%
  select(summary_level:county_subdivision, name = area_name) %>%
  mutate(us = "US") %>%
  select(summary_level, us, everything()) %>%
  pivot_longer(-name, names_to = "variable") %>%
  filter(!str_detect(value, "^0+$")) %>%
  group_by(name) %>%
  summarise(code = paste(value, collapse = ""))

base_url <- "https://data.census.gov/cedsci/table"
# add g = geo code, table = DP02, tid ACSDP5Y2018.DP03
base_q <- list(vintage = year, d = "ACS 5-Year Estimates Data Profiles")

urls <- nest_join(geos, dps, by = character()) %>%
  unnest(dps) %>%
  mutate(tid = str_glue("ACSDP5Y{year}.{group}")) %>%
  mutate(q = pmap(list(table = group, g = code, tid = tid), c, base_q),
         url = map_chr(q, ~httr::modify_url(base_url, query = .))) %>%
  select(name, concept, url) %>%
  pivot_wider(id_cols = name, names_from = concept, values_from = url)

# don't need to calculate cws max moes since they're in prof18
prof18 <- read_csv("https://raw.githubusercontent.com/CT-Data-Haven/2018acs/master/output/5year2018town_profile_expanded_CWS.csv", col_types = cols(.default = "c"))

has_digits <- function(x) all((str_detect(x, "^\\d")), na.rm = TRUE)
not_digits <- function(x) !has_digits(x)

meta <- prof18 %>%
  select(where(not_digits), starts_with("Maximum MoE"), -ends_with("Characteristics")) %>%
  select(1:4, starts_with("Maximum MoE"), everything(), -County) %>%
  left_join(urls, by = c("Town" = "name"))

write_csv(meta, "input_data/website_meta.csv")
