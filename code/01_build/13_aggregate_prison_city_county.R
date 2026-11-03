# ==============================================================================
# 01_build/13_aggregate_prison_city_county.R
# (formerly texas/01j_create_city_county_temp_data.R). Run from the repo root; see README.md for the data map.
# ==============================================================================

################################################################################
# Creating city, county level daily temperature data for mortality in prisons*
# National Center for Environmental Economics
# Last edited: 12/18/2025
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  foreign,
  tidyverse,
  readxl,
  janitor,
  sf,
  weathermetrics
)

# detach("package:sf", unload = TRUE)

## input files: 
# 1. cluster_id X temperature X dic data

## output files: 
# 1. city  X temperature X dic data
# 2. county X temperature X dic data

## clean house 
rm(list = ls())

################################################################################
## Load and prepare data
################################################################################

## load the address and location data to match back to cluster IDs
tx_temp_dic_prison <- read_rds("data/final/panel_cluster_day_prison.rds")


######################################
##1) PRISON DEATHS
##
  #city level deaths
  city_temps <- tx_temp_dic_prison %>%
    group_by(city, date, county) %>%
    summarise(total_death_city = sum(num_death, na.rm = TRUE), # death count on a given day
           death = ifelse(any(num_death == 1), T, F), # create a binary variable to establish if there was a death
           tmean = mean(tmean, na.rm=T), #average temperature values by city/day
           tmin= mean(tmin, na.rm=T),
           tmax = mean(tmax, na.rm =T),
           tdmean = mean(tdmean, na.rm=T)) %>%
    ungroup 
    
  saveRDS(city_temps, "data/final/panel_city_day_prison.rds")

##
  #county level deaths  
  county_temps <- tx_temp_dic_prison %>%
    group_by(date, county) %>%
    summarise(total_death_county= sum(num_death, na.rm = TRUE), # death count on a given day
              death = ifelse(any(num_death == 1), T, F), # create a binary variable to establish if there was a death
              tmean = mean(tmean, na.rm=T), #average temperature values by city/day
              tmin= mean(tmin, na.rm=T),
              tmax = mean(tmax, na.rm =T),
              tdmean = mean(tdmean, na.rm=T)) %>%
    ungroup 
  
  saveRDS(county_temps, "data/final/panel_county_day_prison.rds")
  
  


























