library(tidyverse)
library(camiller)
library(cwi)

################# VARIABLES ##################
acs_year <- 2019
##############################################
headings <- file.path("utils", c("indicator_headings.txt", "headings_extra.txt")) %>%
  map_dfr(read_csv) %>%
  distinct(indicator, display)

fetch <- readRDS(str_glue("./fetch_data/acs_basic_{acs_year}_fetch_towns.rds")) %>%
  map(filter, str_detect(level, "(state|counties|regions|towns)")) %>%
  map(label_acs, acs_year) %>%
  map(group_by, level, name, year) %>%
  map(~replace_na(., list(moe = 0)))

out <- list()


# TOTAL POPULATION
out$total_pop <- fetch$total_pop %>%
  mutate(group = "total_pop") %>%
  select(level, name, year, group, estimate, moe)


# SEX & AGE
# population under 18, 65+, male, female
# age
out$age <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "age"), sep = "!!") %>%
  filter(!is.na(age)) %>%
  # show_uniq(age) %>%
  add_grps(list(total_pop = 1:23, ages0_17 = 1:4, ages65plus = 18:23), group = age, moe = moe) %>%
  calc_shares(group = age, moe = moe) %>%
  rename(group = age)

# sex
out$sex <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "age"), sep = "!!") %>%
  filter(is.na(age), !is.na(sex)) %>%
  add_grps(list(total_pop = 1:2, male = 1, female = 2), group = sex, moe = moe) %>%
  calc_shares(group = sex, moe = moe) %>%
  rename(group = sex)


# RACE / HISPANIC
# hispanic, white non-hispanic, black non-hispanic, other non-hispanic
out$race <- fetch$race %>%
  # show_uniq(label) %>%
  add_grps(list(total_pop = 1, hispanic = 12, white = 3, black = 4, other_race = 5:9), group = label, moe = moe) %>%
  calc_shares(group = label, moe = moe) %>%
  rename(group = label)


# FOREIGN-BORN
out$foreign_born <- fetch$foreign_born %>%
  # show_uniq(label) %>%
  add_grps(list(total_pop = 1, foreign_born = 5:6), group = label, moe = moe) %>%
  calc_shares(group = label, moe = moe) %>%
  rename(group = label)


# TENURE
# owner-occupied households
out$tenure <- fetch$tenure %>%
  # show_uniq(label) %>%
  add_grps(list(total_households = 1, owner_occupied = 2), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "total_households", moe = moe) %>%
  rename(group = label)


# HOUSING COST
# cost-burdened, not by tenure
out$housing_cost <- fetch$housing_cost %>%
  separate(label, into = c("total", "tenure", "income", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  # show_uniq(group) %>%
  add_grps(list(total_households = 1:3, cost_burden = 3), moe = moe) %>%
  calc_shares(denom = "total_households", moe = moe)


# VEHICLES
# households with at least 1 car
out$vehicles <- fetch$vehicles %>%
  # show_uniq(label) %>%
  add_grps(list(total_households = 1, has_vehicle = 3:6), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "total_households", moe = moe) %>%
  rename(group = label)


# EDUCATIONAL ATTAINMENT
# ages 25+; share with less than high school, bachelors+
out$education <- fetch$education %>%
  # show_uniq(label) %>%
  add_grps(list(ages25plus = 1, less_than_high_school = 2, bachelors_plus = 5:6), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "ages25plus", moe = moe) %>%
  rename(group = label)


# MEDIAN HOUSEHOLD INCOME
# drop regions
out$median_household_income <- fetch$median_income %>%
  filter(!str_detect(level, "regions")) %>%
  mutate(group = "median_household_income") %>%
  select(level, name, year, group, estimate, moe)


# POVERTY & LOW-INCOME
# poverty determined; below 1x fpl, below 2x fpl
out$poverty <- fetch$poverty %>%
  # show_uniq(label) %>%
  add_grps(list(poverty_status_determined = 1, poverty = 2:3, low_income = 2:7), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "poverty_status_determined", moe = moe) %>%
  rename(group = label)


# POVERTY & LOW-INCOME BY AGE
# ages 0-17, ages 65+
out$pov_age <- fetch$pov_age %>%
  separate(label, into = c("total", "age", "ratio"), sep = "!!") %>%
  filter(!is.na(ratio)) %>%
  mutate(across(c(age, ratio), as_factor)) %>%
  group_by(ratio, .add = TRUE) %>%
  # show_uniq(age) %>%
  add_grps(list(ages0_17 = 1:3, ages65plus = 9:10), group = age, moe = moe) %>%
  group_by(level, name, year, age) %>%
  # show_uniq(ratio) %>%
  add_grps(list(poverty_status_determined = 1:12, poverty = 1:3, low_income = 1:8), group = ratio, moe = moe) %>%
  calc_shares(group = ratio, denom = "poverty_status_determined", moe = moe) %>%
  unite(group, age, ratio)


# BIND ALL TOGETHER
# will assemble with cws, headings in separate script
out_df <- out %>%
  bind_rows(.id = "indicator")

###### OUTPUT
# regular working versions
saveRDS(ungroup(out_df), str_glue("output_data/acs_town_basic_profile_{acs_year}.rds"))

out_df %>%
  filter(!str_detect(level, "puma")) %>%
  write_csv(str_glue("output_data/acs_town_basic_profile_{acs_year}.csv"))

# distro version
out_df %>%
  ungroup() %>%
  select(level, name, group, estimate, share) %>%
  filter(!str_detect(level, "puma")) %>%
  pivot_longer(estimate:share, names_to = "type") %>%
  unite(indicator, type, group, sep = " ") %>%
  filter(!is.na(value)) %>%
  left_join(headings, by = "indicator") %>%
  distinct(level, name, indicator, .keep_all = TRUE) %>%
  pivot_wider(id_cols = c(level, name), names_from = display) %>%
  write_csv(str_glue("to_distro/town_acs_basic_distro_{acs_year}.csv"))

