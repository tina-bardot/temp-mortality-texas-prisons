# ==============================================================================
# 01_build/06_combine_facility_polygons.R
# (formerly texas/01d_combine_fac_polys.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
# Matching facilities to DIC locations
######
#0) SET-UP
rm(list=ls())

# Load Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  dplyr, # data wrangling
  lubridate, # Date object handling
  readxl,
  sf,
  units,
  spatialEco,
  sfheaders
)

# Source function
source('code/functions/m_to_mile.R') 

## input files: 
# 1. ungrouped facility polygons and point locations: tdcj_hifld_comb.rds

## output files: 
# 1. grouped facility polys : combined_units.rds

#Load data

  #unique parsed HIFLD faciliites
  tx_units_parsed <- readRDS("data/intermediate/facilities/facilities_tdcj_hifld.rds") 
  
  #PL - what about: all_locs_w_pm <- read_rds("data/intermediate/geocoding/dic_locs_geocoded.rds")

######
# 1)
# First, create grouped geometries to get facilities more aggregated
# Part 1: HIFLD locations
  
#separate HIFLD facilities by jail / prison
  units_prison = tx_units_parsed %>%
      filter(custody != "JAIL") # includes multi-use facilities
  
  units_jail = tx_units_parsed %>%
      filter(custody == "JAIL")

##PRISONS
#combine proximal prison units.

  df <- units_prison
  # df$input_units <- NA
  # df$units_ac <- NA
  df2 = data.frame()
  dist = 3000
  units = "m"

  dist_matrix <- st_distance(units_prison)
  
  # Define a threshold distance
  threshold <- as_units(dist, units)
  
  # Create a list to store combined geometries
  combined_geometries <- list()
  
  # Track which points have been combined
  combined <- rep(FALSE, nrow(units_prison))
  
  for (i in 1:nrow(units_prison)) {
    if (!combined[i]) {
      
      # Find points within the threshold distance
      close_points <- which(dist_matrix[i, ] <= threshold)
      
      # Combine geometries
      combined_geometry <- st_union(units_prison[close_points, ])
      
      # Add to the list
      combined_geometries <- append(combined_geometries, list(combined_geometry))
      
      # Mark points as combined
      combined[close_points] <- TRUE
    }
  }

# Convert the list to an sf object

  combined_sf <- do.call(rbind, combined_geometries)
  
  combined_sf2 <- sf::st_sf(geometry = st_sfc(combined_sf), crs = st_crs(units_prison))
  
  comb_pris <- st_join(combined_sf2, units_prison) %>% 
    rename(geometry = geometry)# %>%
    # group_by(geometry) %>%
    # mutate(geo_id = cur_group_id()) %>% #PL _ creating geo_id after we combine w/ jails.
    # ungroup()
  # 
  # unique_pris <- comb_pris %>% 
  #   distinct(geo_id, .keep_all =T)

## Save 
  # saveRDS(unique_pris,"Data/Intermediate/TX/geocoding/combined_prison.rds")

##JAILS
  # combine nearby jail facilities
  df <- units_jail
  # df$input_units <- NA
  # df$units_ac <- NA
  df2 = data.frame()
  dist = 3000
  units = "m"
  
  dist_matrix <- st_distance(units_jail)
  
  # Define a threshold distance
  threshold <- as_units(dist, units)
  
  # Create a list to store combined geometries
  combined_geometries <- list()
  
  # Track which points have been combined
  combined <- rep(FALSE, nrow(units_jail))
  
  for (i in 1:nrow(units_jail)) {
    if (!combined[i]) {
      
      # Find points within the threshold distance
      close_points <- which(dist_matrix[i, ] <= threshold)
      
      # Combine geometries
      combined_geometry <- st_union(units_jail[close_points, ])
      
      # Add to the list
      combined_geometries <- append(combined_geometries, list(combined_geometry))
      
      # Mark points as combined
      combined[close_points] <- TRUE
    }
  }
  
  # Convert the list to an sf object
  
  combined_sf <- do.call(rbind, combined_geometries)
  
  combined_sf2 <- sf::st_sf(geometry = st_sfc(combined_sf), crs = st_crs(units_jail))
  
  comb_jail <- st_join(combined_sf2, units_jail) %>% 
    rename(geometry = geometry) #%>%
  #   group_by(geometry) %>%
  #   mutate(geo_id = cur_group_id()) 
  # 
  # unique_jail <- comb_jail %>% 
  #   distinct(geo_id, .keep_all =T)

### Save 
  # saveRDS(unique_jail,"Data/Intermediate/TX/geocoding/combined_jail.rds")
  
  combined_units <- rbind(comb_pris, comb_jail) 
  
  # getting error for bind_rows, but careful 
  
  # id_cols <- combined_units %>%
  #   bind_rows(comb_jail) %>%
  #   group_by(geometry) %>%
  #   mutate(geo_id = cur_group_id()) %>%
  #   ungroup() %>%
  #   distinct(geo_id, unit_id, .keep_all =T)
    
  saveRDS(combined_units, "data/intermediate/facilities/facilities_combined.rds")
  