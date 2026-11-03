# ==============================================================================
# 01_build/10_combine_cluster_temps.R
# (formerly texas/01h_create_daily_cluster_temps_data.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
################################################################################
# Combine Texas PRISM datasets 
# National Center for Environmental Economics
# Last edited: 11/08/2023
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  dplyr, # data wrangling
  sf,
  lubridate,
  arrow
)

## input data: 
# 1. day-level cluster id-level temperature data sets

## output data: 
# 1. single data.frame with all dates and temperature variables 


################################################################################
## Script to combine PRISM datasets for DIC 
################################################################################

## Start with a base upon which you're going to build the datasets

# base <- read_feather('Data/Raw/Climate data/prism/tmax/2005/maxt_2005-01-01.feather')
# 
# write_feather(base, paste0('Data/Intermediate/Climate data/prism/tmax/', 'tx_max_temp', '.feather'))

# "tmean", 

vars <- c("tmax", "tmin", 'tmean', 'tdmean')

base <- data.frame()

## Base file
for (i in vars) {
  
  base <- data.frame()
  
  for (DATE in as.character(seq(ymd("2005-01-01"), ymd("2024-11-30"), "days"))) {
    
    ## Where are the raw files
    dir = paste0('data/intermediate/temperature/cluster_daily/', i,'/', year(DATE))
    
    ## the file being read
    file  = read_feather(paste0(dir, '/', i, "_", DATE, '.feather')) %>%
      mutate(date = DATE)
    
    ##Add file to base file 
    
    base = bind_rows(base, file)
    
    # rm(file)
    
  }
  
  # make sure we don't have duplicate observations
  
  base <- filter(base, !is.na(date)) %>%
    distinct(cluster_id, date, .keep_all = TRUE)
  
  write_feather(base, paste0('data/intermediate/temperature/cluster_combined/', i,'_tx.feather'))
  
  saveRDS(base, paste0('data/intermediate/temperature/cluster_combined/', i,'_tx.rds'))
  
}

unit_locs <- readRDS('data/intermediate/temperature/cluster_polys_prism_crs.rds')

tmax_tx = readRDS('data/intermediate/temperature/cluster_combined/tmax_tx.rds') %>%
  rename(tmax = mean) %>%
  write_rds('data/intermediate/temperature/cluster_combined/tmax_tx_final.rds')

tmin_tx = read_rds('data/intermediate/temperature/cluster_combined/tmin_tx.rds') %>%
  rename(tmin = mean) %>%
  write_rds('data/intermediate/temperature/cluster_combined/tmin_tx_final.rds')

tmean_tx = read_rds('data/intermediate/temperature/cluster_combined/tmean_tx.rds') %>%
  rename(tmean = mean)  %>%
  write_rds('data/intermediate/temperature/cluster_combined/tmean_tx_final.rds')

tdmean_tx = read_rds('data/intermediate/temperature/cluster_combined/tdmean_tx.rds') %>%
  rename(tdmean = mean) %>%
  write_rds('data/intermediate/temperature/cluster_combined/tdmean_tx_final.rds')

# Combine into one dataset

data_list <- list(tmax_tx, tmin_tx, tmean_tx, tdmean_tx)

merged <- data_list %>% reduce(full_join, by = c("cluster_id", "date"))

# empty <- merged %>%
#   st_as_sf() %>%
#   filter(st_is_empty(.))

temps_tx <- merged %>%
  relocate(date, .before = tmax)

saveRDS(temps_tx, 'data/final/cluster_daily_temps.rds')


###

rm(list = ls())
