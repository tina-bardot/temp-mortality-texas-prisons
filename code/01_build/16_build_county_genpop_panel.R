################################################################################
# 01_build/16_build_county_genpop_panel.R
# (formerly texas/01i_construct_county_mortality_temp_data_for_interaction_model.R)
# Build the county x day panel that stacks prison deaths in custody (DIC) with
# general-population (DSHS) deaths, for the censored binned comparison model.
#
# Inputs:
#   data/final/panel_county_day_prison.rds                        (13_aggregate_prison_city_county.R)
#   data/raw/dshs/T2_TB_DM-24-0447_daily_deaths_2000-2023.xlsx    (TX DSHS, restricted)
#   data/intermediate/temperature/county_daily_temps.rds          (15_extract_prism_county_temps.R)
# Output:
#   data/final/panel_county_day_prison_genpop.rds
#
# Peiley Lau (lau.peiley@epa.gov), created 10/17/2024 | updated 12/19/2024
# Repo cleanup: input switched from the retired prison_dic_temps_by_county.rds
# (written only by combine_dic_temps_old.R) to panel_county_day_prison.rds.
################################################################################

#0. SET UP
library(tidyverse)
library(readxl)

#LOAD DATA
  #01) prison DIC data (county x day; temperatures averaged over prison clusters)
  dic_df <- read_rds('data/final/panel_county_day_prison.rds') %>%
    mutate(custody = 'PRISON') %>%
    dplyr::select(date, tmax, tmin, tmean, custody,
                  county, total_death_county) %>%
    mutate(date = ymd(date),
           county = str_to_title(county))

  #02) gen pop mortality data (counts 1-9 are suppressed and read as NA)
  genpop_df <- read_xlsx('data/raw/dshs/T2_TB_DM-24-0447_daily_deaths_2000-2023.xlsx',
                         sheet = 2,
                         skip = 2,
                         na = '*') %>%
    mutate(date = ymd(`Date of Death`),
           year = year(date)) %>%
    rename(county = `Residence County`) %>%
    dplyr::select(-Year, -`Date of Death`)

  #03) county-wide temperatures to match to genpop_df
  temp_df <- read_rds('data/intermediate/temperature/county_daily_temps.rds') %>%
    mutate(date = ymd(date))

#1. JOIN DATA TOGETHER
  genpop_temp_df <- genpop_df %>%
    left_join(temp_df, by = c('date', 'county'))

  #clean columns so they are the same across both datasets
  genpop_temp_df <- genpop_temp_df %>%
    rename(total_death_county = Deaths) %>%
    dplyr::select(-any_of(c('GEOID', 'vpdmax'))) %>%
    mutate(location = 'free',
           dow = wday(date, label = T))

  # sanity check: county names must match between DSHS and PRISM county temps
  n_unmatched <- genpop_temp_df %>% filter(is.na(tmin)) %>% distinct(county) %>% nrow()
  if (n_unmatched > 0) message(n_unmatched, " DSHS counties have no temperature match; check county name formats.")

  dic_df <- dic_df %>%
    mutate(year = year(date),
           dow = wday(date, label = T)) %>%
    rename(location = custody)

  main_df <- dic_df %>%
    bind_rows(genpop_temp_df) %>%
    filter(!is.na(tmin))

  prison_cty_vec <- main_df %>%
    filter(location == 'PRISON') %>%
    pull(county) %>% unique()

  main_df <- main_df %>%
    #keep only counties that are in both prison + free living population
    filter(county %in% prison_cty_vec)

#2. SAVE
  write_rds(main_df, 'data/final/panel_county_day_prison_genpop.rds')
