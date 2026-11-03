# ==============================================================================
# 02_descriptives/05_prison_location_summary_stats.R
# (formerly texas/04z_create_prison_loc_summary_stats_for_paper.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
####
# Title: 04a_create_prison_loc_summary_stats_for_paper.R
# created: 2/10/2025 | updated: 5/8/2025
# Peiley Lau, lau.peiley@epa.gov
# Memo: Construct descriptive statistics of the prison facilities in our paper
# 
####

####
#0. SET UP
library(tidyverse)
library(sf)
library(mapview)
library(kableExtra) # create LaTeX table from df

#load data
  #intermediate data path
  int_data_path <- "data/intermediate/dic"
  
  # #unique HIFLD faciliites
  # units_df <- read_rds(file.path(int_data_path, 'tdcj_hifld_comb.rds')) 
  # 
  # #clustered HIFLD facilities + geocoded addresses
  # combo_units_df <- read_rds(file.path(int_data_path, 'combined_units.rds'))
  
  #unique DIC locations matched to HIFLD locations/clustered HIFLD locations
  loc_df <- read_rds("data/final/cluster_locations.rds") %>%
    mutate(pm.uid = as.integer(pm.uid))
  
  #DIC data w/ pm.uid
  dic_pmuid_df <- read_rds("data/intermediate/dic/dic_with_pm_uid.rds")
                     
####
#1. SUBSET AND CLEAN DATA
  #PRISON ONLY LOCATIONS AND DEATHS. 
  prison_pmuid <- loc_df %>%
    filter(custody == 'PRISON') %>%
    pull(pm.uid) %>%
    unique()
  
  prison_dic_df <- dic_pmuid_df %>%
    filter(pm.uid %in% prison_pmuid) %>%
    filter(custody != 'JAIL') %>%
    mutate(death_ymd = ymd(death_ymd),
           year = year(death_ymd)) %>%
    left_join(loc_df %>%
                dplyr::select(pm.uid, cluster_id), 
              by = 'pm.uid')
  
###
#2. PRISON LOCATION SUMMARY STATS
  #number of units
  loc_df %>%
    st_drop_geometry() %>%
    filter(custody == 'PRISON') %>%
    pull(unit_id) %>%
    unique() %>%
    length()
  
  # [1] 125
    
  #number of clusters
  loc_df %>%
    st_drop_geometry() %>%
    filter(custody == 'PRISON') %>%
    pull(cluster_id) %>%
    unique() %>%
    length()
  
  #210 clusters
  
    #FIGURE - MAP
    #map of the clusters
    loc_df %>%
      filter(custody == 'PRISON') %>%
      mapview()
    
  #number of clusters by city
  
  #number of cities
  prison_dic_df %>%
    pull(city) %>%
    unique() %>%
    length()
  
  #127 cities
    
  #number of counties
  prison_dic_df %>%
    pull(county) %>%
    unique() %>%
    length()  
  #96 counties
  
###
#3. 
#PRISON LOCATION - NUMBER OF YEARS EACH THAT EACH CLUSTER SHOWS UP
  #how many years in the DIC data did each cluster show up
  prison_dic_df %>%
    dplyr::select(year, cluster_id) %>%
    distinct() %>%
    group_by(cluster_id) %>%
    mutate(num_year = n_distinct(year)) %>%
    ungroup() %>%
    arrange(cluster_id) 
  
  num_cluster_yr_table <- prison_dic_df %>%
    dplyr::select(year, cluster_id) %>%
    distinct() %>%
    group_by(cluster_id) %>%
    summarise(num_year = n_distinct(year)) %>%
    ungroup() 
  
  #FIGURE - MAP
  #color the prison cluster map by number of years it shows up
  loc_df %>%
    filter(custody == 'PRISON') %>%
    left_join(num_cluster_yr_table, by = 'cluster_id') %>%
    mapview(zcol = 'num_year')
  
#CITY LOCATION - NUMBER OF YEARS THAT EACH CITY SHOWS UP FOR.
  prison_dic_df %>%
    dplyr::select(year, city) %>%
    distinct() %>%
    group_by(city) %>%
    mutate(num_year = n_distinct(year)) %>%
    ungroup() %>%
    arrange(city) %>%
    print()
  
  num_city_yr_table <- prison_dic_df %>%
    dplyr::select(year, city) %>%
    distinct() %>%
    group_by(city) %>%
    summarise(num_year = n_distinct(year)) %>%
    ungroup() 
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  