# ==============================================================================
# 01_build/12_join_dic_cluster_temps.R
# (formerly texas/01i_combine_dic_w_temp.R). Run from the repo root; see README.md for the data map.
# ==============================================================================

## Last update: 4/15/2025
# Combine daily cluster_id X temperature data with the individual mortality observations

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, # data wrangling
  tidyr,
  sf,
  units,
  janitor,
  sfheaders
)

### 

rm(list = ls())

## input files: 
# 1. daily cluster_id X temperature data.frame 
# 2. final data.frame with dic dates and updated location information (city and county) 

## output files:
# 1. full panel data on temperatures with uncollapsed dic observations (clusterid X temp X dic)
#    data/final/panel_cluster_day_dic_temp.rds
# 2. same panel restricted to prison clusters, with num_death indicator
#    data/final/panel_cluster_day_prison.rds

# Read in temperature data
tx_temps <- readRDS("data/final/cluster_daily_temps.rds")

# read in DIC + location data
dic_cluster_id <- readRDS("data/final/dic_cluster_crosswalk.rds") %>% rename(date = death_ymd)

tx_temp_dic <- tx_temps %>%
  left_join(dic_cluster_id) %>% 
  mutate(month = format(as.Date(date), format="%m"), # Date formatting for grouping and fixed effects
         year = format(as.Date(date, format = "%Y-%m-%d"), "%Y"),
         dow = weekdays(as.Date(date)))  %>%
  group_by(cluster_id) %>%
  fill(city, county, death_location_type, match_tier, match_type, hifld_add, .direction = "updown") %>%
  ungroup

saveRDS(tx_temp_dic, "data/final/panel_cluster_day_dic_temp.rds")

################################################################################
# Prison-only cluster x day panel (input to 13_aggregate_prison_city_county.R)
#
# ADDED IN REPO CLEANUP: no script in the working repo wrote
# cluster_id_DIC_prison_temp.rds, which 01j read. This step reconstructs it
# using the same rule as the retired combine_dic_temps_old.R (Part 2):
#   - a cluster is a prison cluster if any death matched to it has custody == "PRISON"
#     (custody was collapsed to JAIL / PRISON in 07_match_and_cluster_locations.R;
#      private and multi-use facilities are coded PRISON)
#   - deaths with custody == "JAIL" are not counted
# VERIFY against the original panel before release, e.g. the number of prison
# deaths (04_prison_dic_summary_stats.R reports 9,387) and the city-panel
# sample size in 21_model_binned_dic.R (lreg6 reported 337,763 obs).
# The old script also required !is.na(unit_geo); add that filter below if the
# counts do not match.
################################################################################

prison_clusters <- dic_cluster_id %>%
  filter(custody == "PRISON") %>%
  distinct(cluster_id) %>%
  pull(cluster_id)

tx_temp_dic_prison <- tx_temp_dic %>%
  filter(cluster_id %in% prison_clusters) %>%
  mutate(num_death = ifelse(!is.na(record_id) & !(custody %in% "JAIL"), 1, 0))

message("Prison clusters: ", length(prison_clusters),
        " | prison deaths counted: ", sum(tx_temp_dic_prison$num_death))

saveRDS(tx_temp_dic_prison, "data/final/panel_cluster_day_prison.rds")

