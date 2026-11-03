# ==============================================================================
# 01_build/11_create_temp_thresholds.R
# (formerly texas/01k_create_temp_thresholds.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
################################################################################
# Combining PRISM temperatures with DIC
# National Center for Environmental Economics
# Last edited: 4/9/2025
################################################################################

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  readr,
  tidyverse,
  dplyr,
  janitor,
  sf,
  tigris
)

# detach("package:sf", unload = TRUE),

#set directory

## input files:
# 1. daily cluster_id X temperature files
# 2.

## output files:
# 1. state-level hot- and cold- temperature thresholds
# 2. city-level hot- and cold- temperature thresholds
# 3. county-level hot- and cold-temperature thresholds
rm(list = ls())

################################################################################
## Load and prepare data
################################################################################

tx_temps <- readRDS("data/final/cluster_daily_temps.rds")

dic_locs <- readRDS(
  "data/final/cluster_locations.rds"
) %>%
  distinct(city, county, cluster_id) %>%
  st_drop_geometry()

# state threshold

state_threshold <- tx_temps %>%
  left_join(dic_locs, relationship = 'many-to-many') %>%
  mutate(
    max99 = quantile(tmax, probs = .99, na.rm = TRUE),
    min99 = quantile(tmin, probs = .99, na.rm = TRUE),
    mean99 = quantile(tmean, probs = .99, na.rm = TRUE),
    dp99 = quantile(tdmean, probs = .99, na.rm = TRUE),
    max95 = quantile(tmax, probs = .95, na.rm = TRUE),
    min95 = quantile(tmin, probs = .95, na.rm = TRUE),
    mean95 = quantile(tmean, probs = .95, na.rm = TRUE),
    dp95 = quantile(tdmean, probs = .95, na.rm = TRUE),
    min01 = quantile(tmin, probs = .01, na.rm = TRUE),
    mean01 = quantile(tmean, probs = .01, na.rm = TRUE),
    max01 = quantile(tmax, probs = .01, na.rm = TRUE),
    dp01 = quantile(tdmean, probs = .01, na.rm = TRUE),
    min05 = quantile(tmin, probs = .05, na.rm = TRUE),
    mean05 = quantile(tmean, probs = .05, na.rm = TRUE),
    max05 = quantile(tmax, probs = .05, na.rm = TRUE),
    dp05 = quantile(tdmean, probs = .05, na.rm = TRUE)
  ) %>%
  distinct(cluster_id, .keep_all = T) %>%
  dplyr::select(-c('date', 'cluster_id', starts_with(c("tm", "td")))) %>%
  mutate(
    county = str_to_title(county),
    city_old = as.factor(city),
    city = paste(city_old, county, sep = '_') %>% as.factor()
  )

saveRDS(state_threshold, "data/final/temp_thresholds_state.rds")

# Note: I am keeping city and county info so that the state threshold can be used in either analysis

# city

city_threshold <- tx_temps %>%
  left_join(dic_locs, relationship = 'many-to-many') %>%
  mutate(
    county = str_to_title(county),
    city_old = as.factor(city)
  ) %>%
  #create this new city x county variable to account for some cities that
  #are in two counties. want to keep the city x county combo separate.
  mutate(city = paste(city_old, county, sep = '_') %>% as.factor()) %>%
  group_by(city) %>%
  mutate(
    max99 = quantile(tmax, probs = .99, na.rm = TRUE),
    min99 = quantile(tmin, probs = .99, na.rm = TRUE),
    mean99 = quantile(tmean, probs = .99, na.rm = TRUE),
    dp99 = quantile(tdmean, probs = .99, na.rm = TRUE),
    max95 = quantile(tmax, probs = .95, na.rm = TRUE),
    min95 = quantile(tmin, probs = .95, na.rm = TRUE),
    mean95 = quantile(tmean, probs = .95, na.rm = TRUE),
    dp95 = quantile(tdmean, probs = .95, na.rm = TRUE),
    min01 = quantile(tmin, probs = .01, na.rm = TRUE),
    mean01 = quantile(tmean, probs = .01, na.rm = TRUE),
    max01 = quantile(tmax, probs = .01, na.rm = TRUE),
    dp01 = quantile(tdmean, probs = .01, na.rm = TRUE),
    min05 = quantile(tmin, probs = .05, na.rm = TRUE),
    mean05 = quantile(tmean, probs = .05, na.rm = TRUE),
    max05 = quantile(tmax, probs = .05, na.rm = TRUE),
    dp05 = quantile(tdmean, probs = .05, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(city, .keep_all = T) %>%
  dplyr::select(-c('date', 'county', 'cluster_id', starts_with(c("tm", "td"))))

saveRDS(city_threshold, "data/final/temp_thresholds_city.rds")

###
# County

county_threshold <- tx_temps %>%
  left_join(dic_locs, relationship = 'many-to-many') %>%
  group_by(county) %>%
  mutate(
    max99 = quantile(tmax, probs = .99, na.rm = TRUE),
    min99 = quantile(tmin, probs = .99, na.rm = TRUE),
    mean99 = quantile(tmean, probs = .99, na.rm = TRUE),
    dp99 = quantile(tdmean, probs = .99, na.rm = TRUE),
    max95 = quantile(tmax, probs = .95, na.rm = TRUE),
    min95 = quantile(tmin, probs = .95, na.rm = TRUE),
    mean95 = quantile(tmean, probs = .95, na.rm = TRUE),
    dp95 = quantile(tdmean, probs = .95, na.rm = TRUE),
    min01 = quantile(tmin, probs = .01, na.rm = TRUE),
    mean01 = quantile(tmean, probs = .01, na.rm = TRUE),
    max01 = quantile(tmax, probs = .01, na.rm = TRUE),
    dp01 = quantile(tdmean, probs = .01, na.rm = TRUE),
    min05 = quantile(tmin, probs = .05, na.rm = TRUE),
    mean05 = quantile(tmean, probs = .05, na.rm = TRUE),
    max05 = quantile(tmax, probs = .05, na.rm = TRUE),
    dp05 = quantile(tdmean, probs = .05, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(county, .keep_all = T) %>%
  dplyr::select(-c('date', 'city', 'cluster_id', starts_with(c("tm", "td"))))

saveRDS(county_threshold, "data/final/temp_thresholds_county.rds")
