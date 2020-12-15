library(tidyverse)
library(camiller)
library(cwi)

########### VARIABLES #############
acs_year <- 2019

params <- list(
  bridgeport = list(region = "Fairfield County", town = "Bridgeport"),
  hartford = list(region = "Greater Hartford", town = c("Hartford", "West Hartford")),
  stamford = list(region = "Fairfield County", town = "Stamford"),
  new_haven = list(region = "Greater New Haven", town = "New Haven")
)
###################################
headings <- read_csv(file.path("utils", "indicator_headings.txt"))
nhoods <- lst(new_haven = nhv_bgrps, hartford_tracts, bridgeport_tracts, stamford_tracts) %>%
  set_names(str_remove, "_tracts") %>%
  map(select, any_of(c("name", "town"))) %>%
  map_dfr(distinct, .id = "city")

# nhv has some coming from bgs, some from tracts
fetch <- list.files("fetch_data", pattern = ".rds", full.names = TRUE) %>%
  set_names(str_extract, "(?<=fetch_)(\\w+)(?=\\.rds$)") %>%
  `[`(c("nhoods", "nhv")) %>%
  map(readRDS) %>%
  map_depth(2, select, -any_of("name_2")) %>%
  map_depth(2, extract, name, into = c("city", "name"), regex = "(\\w*?)?(?:_?)([A-Z].+)") %>%
  map_depth(2, mutate, level = level %>% 
        fct_relabel(str_remove, "^\\d_") %>% 
        fct_collapse(regions = c("regions", "counties")))

fetch$nhv <- fetch$nhv %>%
  map(mutate, city = if_else(str_detect(level, "neighborhoods"), "nhv", "")) %>%
  map(filter, !is.na(estimate))



fetch <- pmap(fetch, bind_rows, .id = "src") %>%
  map(mutate, src = as_factor(src) %>% fct_relevel("nhv")) %>%
  map(arrange, src, level, name) %>%
  map(distinct, level, city, name, variable, year, .keep_all = TRUE) %>%
  map(label_acs, year = acs_year) %>%
  map(mutate, city = fct_recode(city, new_haven = "nhv")) %>%
  map(group_by, level, city, name)

out <- list()

# AGE
# population under 18, 18+, 65+
out$age <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_pop = 1:23, ages0_17 = 1:4, ages18plus = 5:23, ages65plus = 18:23)) %>%
  calc_shares(digits = 2)


# RACE / HISPANIC
# hispanic, white non-hispanic, black non-hispanic, other non-hispanic
out$race <- fetch$race %>%
  add_grps(list(total_pop = 1, hispanic = 12, white = 3, black = 4, other_race = 5:9), group = label) %>%
  calc_shares(digits = 2, group = label) %>%
  rename(group = label)


# FOREIGN-BORN
out$immigration <- fetch$foreign_born %>%
  separate(label, into = c("total", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_pop = 1:5, foreign_born = 4:5)) %>%
  calc_shares(digits = 2)


# TENURE
# owner-occupied households
tenure <- fetch$tenure %>%
  add_grps(list(total_households = 1, owner_occupied = 2), group = label) %>%
  calc_shares(digits = 2, group = label, denom = "total_households") %>%
  rename(group = label)


# HOUSING COST
# cost-burdened, not by tenure
housing_cost <- fetch$housing_cost %>%
  separate(label, into = c("total", "tenure", "income", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_households = 1:3, cost_burden = 3)) %>%
  calc_shares(digits = 2, denom = "total_households") %>%
  filter(group != "total_households")

out$housing <- bind_rows(tenure, housing_cost)


# POVERTY & LOW-INCOME
# poverty determined; below 1x fpl, below 2x fpl
out$income <- fetch$poverty %>%
  add_grps(list(poverty_status_determined = 1, poverty = 2:3, low_income = 2:7), group = label) %>%
  calc_shares(digits = 2, group = label, denom = "poverty_status_determined") %>%
  rename(group = label)


# POVERTY & LOW-INCOME BY AGE
# ages 0-17, ages 65+
pov_age <- fetch$pov_age %>%
  separate(label, into = c("total", "age", "ratio"), sep = "!!") %>%
  filter(!is.na(ratio)) %>%
  mutate(across(c(age, ratio), as_factor)) %>%
  group_by(ratio, .add = TRUE) %>%
  add_grps(list(ages0_17 = 1:3, ages65plus = 9:10), group = age) %>%
  group_by(level, city, name, age) %>%
  add_grps(list(poverty_status_determined = 1:12, poverty = 1:3, low_income = 1:8), group = ratio) %>%
  calc_shares(digits = 2, group = ratio, denom = "poverty_status_determined") %>%
  ungroup() %>%
  unite(group, age, ratio)
out$income_children <- pov_age %>% filter(str_detect(group, "^ages0_17"))
out$income_seniors <- pov_age %>% filter(str_detect(group, "^ages65plus"))


######### BIND EVERYTHING
out_df <- bind_rows(out, .id = "topic") %>%
  ungroup() %>%
  left_join(nhoods, by = c("city", "name"))

out_by_city <- params %>%
  # set_names(clean_titles, cap_all = TRUE) %>%
  imap(function(p, cty) {
    fltr <- c(unlist(p), "Connecticut")
    out_df %>%
      filter(name %in% fltr | city == cty)
  }) %>%
  map(select, topic, level, city, town, everything())

saveRDS(out_by_city, str_glue("output_data/acs_to_prep_for_viz_{acs_year}.rds"))

out_by_city %>%
  map(pivot_longer, estimate:share, names_to = "type") %>%
  map(unite, indicator, type, group, sep = " ") %>%
  map(filter, !is.na(value)) %>%
  map(left_join, headings, by = c("topic", "indicator")) %>%
  map(distinct, level, city, town, name, indicator, .keep_all = TRUE) %>%
  map(pivot_wider, id_cols = c(level, city, town, name), names_from = display) %>%
  map(mutate, city = clean_titles(city, cap_all = TRUE)) %>%
  map(arrange, level, town) %>%
  map(janitor::remove_empty, "cols") %>%
  iwalk(~write_csv(.x, str_glue("./to_distro/{.y}_acs_basic_neighborhood_{acs_year}.csv"), na = ""))
