library(testthat)
library(purrr)

test_that("neighborhood profiles have correct number of rows", {
  # should have nrow = CT + regions + towns + neighborhoods
  params <- list(
    bridgeport = list(region = "Fairfield County", town = "Bridgeport"),
    hartford = list(region = "Greater Hartford", town = c("Hartford", "West Hartford")),
    stamford = list(region = "Fairfield County", town = "Stamford"),
    new_haven = list(region = "Greater New Haven", town = "New Haven")
  ) %>%
    .[order(names(.))]
  
  dfs <- list.files("to_distro", pattern = "neighborhood_\\d{4}", full.names = TRUE) %>%
    set_names(stringr::str_extract, "(\\w*?)(?=_acs)") %>%
    map(readr::read_csv) %>%
    .[order(names(.))]
  
  nhoods <- tibble::lst(cwi::nhv_tracts, cwi::bridgeport_tracts, cwi::hartford_tracts, cwi::stamford_tracts) %>%
    set_names(stringr::str_extract, "\\w+(?=_tracts)") %>%
    set_names(stringr::str_replace, "nhv", "new_haven") %>%
    map(~dplyr::n_distinct(.$name)) %>%
    .[order(names(.))]
  
  ns <- pmap(list(params, dfs, nhoods), function(p, d, n) {
    regs <- length(c(unlist(p), "Connecticut"))
    nrow(d) == regs + n
  })
  
  expect_true(all(ns))
})

test_that("town profile has correct number of rows", {
  # don't worry about regions, but should have nrow = CT + 8 counties + 169 towns
  df <- readr::read_csv(list.files("to_distro", pattern = "town", full.names = TRUE)) %>%
    dplyr::filter(!grepl("regions", level))
  n_expect <- 1 + 8 + 169
  n <- nrow(df)
  expect_equal(n, n_expect)
})

test_that("no blank columns", {
  dfs <- list.files("to_distro", full.names = TRUE) %>%
    map(readr::read_csv) %>%
    map_depth(2, is.na) %>%
    map_depth(2, all) %>%
    map(flatten_lgl) %>%
    map_lgl(any)
  expect_false(any(dfs))
})
