# ==============================================================================
# 01_build/07_match_and_cluster_locations.R
# (formerly texas/01e_match_locs_and_cluster.R). Run from the repo root; see README.md for the data map.
# ==============================================================================

################################################################################
# Match unmatched locations based on distance to polygon
# National Center for Environmental Economics
# Last edited: 10/28/2024
################################################################################

#0. SET-UP. 
# Load Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, # data wrangling
  tidyr,
  sf,
  units,
  janitor,
  sfheaders
)

# Clean house

rm(list = ls())

#Uses RProj, 'Heat Waves and Inmate Mortality'


######
#set up functions
#####

source('code/functions/fix_address.R') 

source('code/functions/negate.R') 

source('code/functions/m_to_mile.R') 

source('code/functions/rem_dup.R') 


## input files ## NOTE ADD WHERE INPUT CREATED
# 1. dic observations w/ pm.uid : dic_with_pm_uid.rds
# 2. grouped facility polygons and points: combined_units.rds
# 3. phase 1 geocoded locations: locs_geocoded_all_w_pm.rds

## output files
# 1. matched and clustered location files : dic_locs_cluster_id.rds

##########
#0) SET UP
#load data

#DIC observations with pm.uid 
tx_dic_pm_uid <- readRDS("data/intermediate/dic/dic_with_pm_uid.rds") 

#extract the type of location and custody associated w/ the address

##########
#1) CLEAN DATA.
#pm.uid linked to custody type (jail or prison)
death_loc_type <- tx_dic_pm_uid %>%
  mutate(pm.uid = as.character(pm.uid)) %>%
  dplyr::select(pm.uid, death_location_type, custody)%>%
  group_by(pm.uid) %>%
  mutate(death_location_type = ifelse(any(str_detect(death_location_type, "MEDICAL")), "MEDICAL", death_location_type),
         custody = ifelse(str_detect(custody, "JAIL"), "JAIL", "PRISON")) %>% ## ASSUME private facilities are prisons
  ungroup %>%
  distinct(pm.uid, .keep_all = T)

levels(factor(death_loc_type$death_location_type))

#######
#Part 2: match locations to nearby facilities

# rm(list = ls()[sapply(mget(ls()), is.data.frame)])

combined_units <- readRDS("data/intermediate/facilities/facilities_combined.rds")
## has unit_id col which needs to be saved as a crosswalk once the matching has been done

# units_unique <- combined_units %>% distinct(geometry, .keep_all=T)
## Use the unique geos to standardize

#load PM.UID <--> HIFLD/geocoded
all_locs_w_pm <- readRDS("data/intermediate/geocoding/dic_locs_geocoded.rds")  %>%
  st_as_sf() %>%
  sf_cast(to = "MULTIPOLYGON") %>%
  left_join(death_loc_type) %>%
  ungroup

# Separate : Locations matched to city
city_locs <- all_locs_w_pm %>% filter(match_type == "city") %>%
  mutate(pm.city = city)

## Matched locations: re-assign grouped polygon

matched_locs <- all_locs_w_pm %>% filter(!is.na(unit_id)) %>%
  st_drop_geometry %>%
  left_join(combined_units %>% dplyr::select(unit_id))

## Distance-match : remaining (unmatched locations)

unit_locs <- all_locs_w_pm %>%
  filter(is.na(unit_id) & match_type != "city")
  
## Set distance thresholds

dist_med <- as_units(15000, 'm') #15 km for medical assuming 

dist_un <- as_units(5000, 'm') # 5 km for other (i.e., unit)

locs_no_geo <- st_drop_geometry(unit_locs) 

for (i  in seq_len(nrow(unit_locs))) { 
  
  # find closest unit
  
  nearest = st_nearest_feature(unit_locs[i,], combined_units)
  
  # Create distance column
  unit_locs[i, 'distance'] <- st_distance(unit_locs[i,], combined_units[nearest, ])
  
  dist_to_poly <- st_distance(unit_locs[i,], combined_units[nearest, ], by_element = T)
  
  #replace geometry if closer than 60 km for medical facilities
  
  if (locs_no_geo[i,'death_location_type'] == "MEDICAL") { 
    
    if(dist_to_poly <= dist_med ){
      # replace geometry with hifld
      st_geometry(unit_locs[i, ]) <- st_geometry(combined_units[nearest, ])
      
      # add unit name
      unit_name <- st_drop_geometry(combined_units[nearest, 'unit_name'])
      unit_locs[i, 'unit_geo'] <- unit_name }
    
  }
  
  # location matching for non-medical facilities (for poor address types)
  
  else if (locs_no_geo[i,'death_location_type'] != "MEDICAL") {
    
    if (dist_to_poly <= dist_un) {
      # replace geometry with hifld
      st_geometry(unit_locs[i, ]) <- st_geometry(combined_units[nearest, ])
      
      # add unit name
      unit_name <- st_drop_geometry(combined_units[nearest, 'unit_name'])
      unit_locs[i, 'unit_geo'] <- unit_name
    }
  }
  
  #na for non-unit matches
  
  else {
    unit_locs[i, 'unit_geo'] <- NA
  }
  
  
}

sum(is.na(unit_locs$unit_geo))
# 69

length(unique(unit_locs$unit_geo))
# 206

summary(unit_locs$distance)
# 50% are within 2 km

no_unit_match <- filter(unit_locs, is.na(unit_geo))
# 69

unit_match <- filter(unit_locs, !is.na(unit_geo))

####
####

# Create geometry-based ids to connect dic with locations 
# will be used to fit into the PRISM data

dic_locs <- bind_rows(unit_locs, city_locs, matched_locs) %>%
  group_by(geometry) %>%
  mutate(cluster_id = cur_group_id()) %>%
  ungroup() %>%
  dplyr::select(-c(2:4))

saveRDS(dic_locs, "data/intermediate/clusters/dic_locs_cluster_id.rds")
