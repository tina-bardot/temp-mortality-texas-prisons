# ==============================================================================
# 01_build/05_geocode_dic_locations.R
# (formerly texas/01c_geocode_dic_loc_parsed.R). Run from the repo root; see README.md for the data map.
# ==============================================================================

################################################################################
# Geocode death in custody locations
# National Center for Environmental Economics
# Last edited: 10/28/2024
################################################################################

#0. SET-UP. 
# Load Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  dplyr, # data wrangling
  lubridate, # Date object handling
  fuzzyjoin,
  tidyr,
  stringr,
  sf,
  tidygeocoder
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


## input files: 
# 1. parsed DIC addresses: intermediate_pm_add.rds
# 2. city-only dic addresses: pm_only_city.rds
# 3. death locations with partial address or unit names: pm_no_address.rds
# 4. parsed TDCJ/HIFLD addresses

## output files: 
# 1. geocoded addresses (phase 1), distinct unit/address points and polygons : locs_geocoded_all_w_pm.rds

################################################################################
# This first step matches DIC addresses with prison and jail units based on address 
# Step 1:
#############
# Step 1/Part 0: Set-up
#############
# Find potential unit matches from address and unit names
# Load unit directory, combined TDCJ and HIFLD data 
tdcj_hifld_comb <- readRDS("data/intermediate/facilities/facilities_tdcj_hifld.rds") %>% 
  mutate(pm.address = fix_address(pm.address)) %>%
  mutate(across(is.character, toupper)) %>%
  mutate(custody = ifelse(is.na(custody), "PRISON", custody)) # some units are NA, checked and these (4) are prisons

tx_units_parsed <- tdcj_hifld_comb  %>%
  dplyr::select(unit_name, city, pm.address, unit_id) %>%
  rename(hifld_add = pm.address) %>% # for matching
  st_drop_geometry()

# data subset: locations with pm address

# pm_add <- readRDS("data/intermediate/dic/dic_with_pm_uid.rds") %>%
#   mutate(pm.uid = as.character(pm.uid)) %>%
#   distinct(pm.uid, custody, address_full) ## Check to see whether we have multiple custody in a single pm.uid

# load parsed addresses from DIC data   
dic_pm_add <- readRDS("data/intermediate/geocoding/dic_locs_parsed_address.rds")

# %>% # parsed addresses
#   left_join(pm_add) 

### CHECK CHECK: No random NA's in the address
# View(dic_pm_add)

# Extract the unit name from the address when it exists, and use that when possible 
  #this is used in Step 1/Part 2
  dic_pm_add_unit <- dic_pm_add %>% 
    filter(str_detect(pm.address, " UNIT"))
  #pull out the unit names
  unit_list <- unlist(lapply(strsplit(dic_pm_add_unit$pm.address, " "), function(x) x[pmax(0, which(x == "UNIT") - 1)]))

  dic_pm_add_unit <- dic_pm_add_unit %>%
    bind_cols("units" = unit_list) %>%
    mutate(pm.address = paste(unit_list, "UNIT")) %>% 
    dplyr::select(-units) 

# Remove the observations from the main data.frame
  dic_pm_add <- filter(dic_pm_add, pm.uid %!in% dic_pm_add_unit$pm.uid)
#############
# Step 1/Part 1: 
# Address fuzzy string match
#############
# Fuzzy join on address
# address_matches <- fuzzyjoin::stringdist_inner_join(dic_pm_add, tx_units_parsed,
#                                          method = "lv", 
#                                          max_dist = 4, by = c("pm.address"="hifld_add", "pm.city" = "city"), 
#                                          distance_col = "dist") %>% distinct(pm.uid, .keep_all=T)
  #address match between DIC and HIFLD/TDCJ addresses
  address_matches <- dic_pm_add %>%
    fuzzyjoin::stringdist_inner_join(tx_units_parsed,
                                     method = "lv", 
                                     max_dist = 5, 
                                     by = c("pm.address"="hifld_add", "pm.city" = "city"), 
                                     distance_col = "dist") %>%
    filter(pm.city == city) %>% #ensure that the matches are in the same city
    group_by(pm.uid) %>% 
    arrange(pm.address.dist) %>% 
    distinct(pm.uid, .keep_all=T) %>% 
    ungroup()

  #693

## Check address matches with distance >=4

  address_match_distance <- address_matches %>% 
    filter(pm.address.dist >= 4) %>%
    dplyr::select(pm.uid, pm.address, hifld_add, pm.address.dist, pm.city, city) %>% 
    mutate(pm.uid = as.numeric(pm.uid))
  
  # 83 observations 
  
  # 19: 1600 11th St, Wichita Falls, TX 76301 -> medical facility, 4 min away from 900 7TH ST
  
  # 103: 104 S SPRING ST, Tyler, TX -> 0.2 miles from 227 N SPRING ST (NOTE this address is wrong, it is Spring AVE)
  
  # 133: 1201 W 38TH ST, Austin, TX -> medical facility, 2.8 miles from 500 W 10TH ST
  
  # 505: 615 E 15TH ST, Austin, TX -> 1.2 miles away from 500 W 10TH ST
  
  # 773: 1500 N MAIN, Fort Worth, TX -> 2 miles away from 100 N LAMAR
  
  # 889: 715 19TH ST, Galveston, TX -> 3 miles, 10 min away from 601 54TH ST
  
  # 959: 601 E 15TH ST, Austin, Texas -> 1.2 miles away from 500 W 10TH ST
  
  # 1475: 655 FM 2821, Hunstville, TX -> 0.4 miles away from 751 FM 2821 W (Note: what the heck)
  
  # 1265: 3100  AVE E, Hondo, TX -> medical facility, 1.4 miles from 801 AVE Y
  
  ## Gut check: these addresses are ok

########
# Step 1/Part 2: 
# Match locations with no address with prison unit names
########
# Load DIC observations that don't have address but have unit names
# Some of these don't have an address, but have a unit name
# First extract the unit name from all the observations
  pm_no_address_unit <- readRDS("data/intermediate/geocoding/dic_locs_no_address.rds")  %>%
    mutate(pm.uid = as.character(pm.uid)) %>%
    bind_rows(dic_pm_add_unit %>% 
                # select(-c(custody, address_full)) %>% 
                mutate(pm.uid = as.character(pm.uid))) %>%
    mutate(unit_name = case_when(
      # create a variable that identifies whether we have a unit name in the address
      str_detect(pm.address, "UNIT") == TRUE ~ pm.address,
      str_detect(pm.address, "JESTER") == TRUE ~ pm.address,
      str_detect(pm.address, "RICHMOND") == TRUE ~ pm.address,
      TRUE ~ NA
    )) %>%
    mutate(unit_name = gsub(" UNIT", "", unit_name)) %>%
    filter(!is.na(unit_name)) %>%
    mutate_all(trimws) %>%
    # fixing up some of the addesses and unit names
    separate(unit_name, into = c("unit_name", "extra"), sep = "^\\S*\\K\\s+") %>%
    mutate(unit_name = ifelse(unit_name == "ROUTE", "HUGHES", unit_name)) %>% # some hard coding: need to clean up some of the names
    mutate(unit_name = ifelse(unit_name == "FM", "SKYVIEW", unit_name)) %>%
    mutate(unit_name = ifelse(unit_name == "I", "JESTER", unit_name)) %>%
    mutate(unit_name = ifelse(unit_name == "RICHMOND", "JESTER", unit_name), 
           unit_name = ifelse(unit_name == "EHAM", "EASTHAM", unit_name))

#pull out unit name and address of HIFLD/TDCJ facilities.
tx_units_parsed <- tdcj_hifld_comb %>% 
  # filter(status == "OPEN") %>% # limit to in-use units (for clarity, and since we don't know when these units closed)
  dplyr::select(unit_name, city, pm.address, unit_id) %>%
  rename(hifld_add = pm.address) %>% # for matching
  st_drop_geometry()

#rename unit names in DIC data with the unit names in the HIFLD data
for(i in 1:nrow(pm_no_address_unit)) {
  
  # extract unit name
  str <- pm_no_address_unit[i, 'unit_name']
  
  #find unit in hifld data
  unit <- tx_units_parsed[grep(str, tx_units_parsed$unit_name)[1], ]
  
  # replace in DIC locations data
  pm_no_address_unit[i, 'unit_name'] <- unit$unit_name
  
}

unit_matches <- pm_no_address_unit %>% 
  dplyr::select(-extra) %>%
  left_join(tx_units_parsed, by = c("unit_name")) %>%
  distinct(pm.uid, .keep_all = TRUE) 

# we've replaced all unit names with those from HIFLD using string matching
  # 50

# Combine the pm.uid with address matches and the unit matches 

good_matches <- address_matches %>%
  mutate(pm.uid = as.character(pm.uid)) %>%
  bind_rows(unit_matches) %>%
  distinct(pm.uid, .keep_all = T) %>%
  mutate(match_tier = "1",
         match_type = "unit match") %>%
  dplyr::select(-c(contains("dist"))) %>%
  left_join(tdcj_hifld_comb %>% dplyr::select(unit_id))

  # saveRDS(good_matches, "Data/Intermediate/TX/geocoding/hifld_pm_add_t1_match.rds")
  
  # 727 obs for distinct locations

# Clean up workspace
## KEEP: good_matches, tx_units_parsed, dic_pm_add
rm(pm_no_address_unit, address_matches, str, unit, unit_matches, dic_pm_add_unit)

################################################################################
# Step 2: Geocode DIC addresses and
# find matches based on geospatial intersection w/ HIFLD/TDCJ unit locations
################################################################################
# Step 2, Part 1: GEOCODE PARSED DIC ADDRESS AND INTERSECT WITH HIFLD/TDCJ
########
# GEOCODE ADDRESSES
# parsed DIC observations w/ known addresses but not fuzzy matched in Step 1
pm_add_to_geocode <- dic_pm_add %>%
  filter(pm.uid %!in% good_matches$pm.uid) %>% # remove already-matched observations
  mutate(address_full = paste(pm.address, pm.city, pm.state, sep = ", "))
# 672 remaining obs in intermediate pm

# Geocode parsed tx dic addresses
pm_lat_long <- pm_add_to_geocode %>%
  geocode_combine(
    queries = list(
      list(method = 'census'),
      list(method = 'osm'),
      list(method = 'arcgis')
    ),
    global_params = list(address = 'address_full')
  )


  # geocode(
  #   method = "osm",
  #   address = address_full,
  #   full_results = T)

# sum(is.na(pm_lat_long$long))

# saveRDS(lat_long, file = "Data/Intermediate/TX/geocoding/lat_longs_dic.rds") 
# 
# lat_long <- readRDS("Data/Intermediate/TX/geocoding/lat_longs_dic.rds")

#geocoded DIC addresses 
  pm_add_geocoded <- pm_lat_long %>%
    filter(!is.na(long)) %>%
    st_as_sf(
      coords = c("long", "lat"),
      crs = 4326) %>%
    st_transform(crs = 3081) %>%
    st_buffer(dist = m_to_mile(1)) # create 1 mile buffer around point locations 

#load texas state boundary
  tx_boundary <- tigris::counties("TX") %>%
    st_transform(crs = 3081)

#filter out any addresses outside of TX boundary
  pm_add_geocoded_tx <- pm_add_geocoded %>%
    st_crop(st_bbox(tx_boundary)) # create boundary for TX 


###
# Mapview to look at addresses

# library(mapview)

# mapView(pm_add_geocoded_tx)

## NOT RUN:: All addresses matched when using multiple queries

# pm_add_no_geocode <- pm_add_to_geocode %>% filter(pm.uid %!in% pm_add_geocoded_tx$pm.uid) %>%
#   left_join(tx_units_parsed, by = c('pm.city'='city')) %>%
#   mutate(match_tier = "3",
#          match_type = "city") 

# Set this observation to the unit in the same city, but demote match tier

#INTERSECT WITH HIFLD/TDCJ LOCATIONS
# Now take the geocoded locations and see if they overlap with prison units

tx_units_parsed <- tdcj_hifld_comb %>% 
  # filter(status == "OPEN") %>% # limit to in-use units (for clarity, and since we don't know when these units closed)
  dplyr::select(unit_name, city, pm.address) %>%
  rename(hifld_add = pm.address) # for matching

#SF object: geocoded DIC addresses matched to HIFLD polygon
pm_hifld_intersect <- pm_add_geocoded_tx %>%
  st_intersection(tx_units_parsed)  %>%
  distinct(pm.uid, .keep_all =T) %>%
  mutate(match_tier = "1",
         match_type = "unit intersect") 

    #217 unique prison unit connections 
 
# remaining observations representing address matches 
# SF object: geocoded DIC not matched to HIFLD polygons.
  pm_no_intersect <- pm_add_geocoded_tx %>% # find remaining locations where we don't intersect w/ a unit
    filter(pm.uid %!in% pm_hifld_intersect$pm.uid) %>%
    mutate(match_tier = "2",
           match_type = "address geocode")
    # 446

## KEEP : pm_no_intersect, pm_hifld_intersect
rm(pm_add_geocoded, pm_add_to_geocode, address_match_distance, pm_add_geocoded_tx, pm_lat_long)

#######
# Part 2, Step 2: 
# Geocode DIC locations with unparsed addresses
#######

#load DIC observations w/ unparsed addresses
pm_no_address_to_geocode <- readRDS("data/intermediate/geocoding/dic_locs_no_address.rds") %>%
  mutate(pm.uid = as.character(pm.uid)) %>%
  distinct(pm.uid, .keep_all=T) %>%
  filter(pm.uid %!in% good_matches$pm.uid) %>% # remove observations already matched
  mutate(pm.city = ifelse(is.na(pm.city), '', pm.city)) %>%
  mutate(address_full = ifelse(pm.city != "", paste(pm.address, pm.city, pm.state, sep = ", "), paste(pm.address, pm.state, sep = ", "))) %>% # construct address for geocoding
  # dplyr::select(pm.uid, pm.address, pm.city, pm.state) %>%
  mutate(address_full = toupper(address_full)) %>%
  mutate(across(everything(), trimws)) %>%
  mutate(across(everything(), ~rem_dup(.))) %>% # remove extra spaces
  mutate(address_full = gsub("UNKNOWN, ", "", address_full))

# 84 remaining observations

# Pass through tidygeocoder: arcgis (faster and more complete than OSM)

lat_long_no_add <- pm_no_address_to_geocode %>%
  geocode_combine(
    queries = list(
      list(method = 'census'),
      list(method = 'osm'),
      list(method = 'arcgis')
    ),
    global_params = list(address = 'address_full')
  )

sum(is.na(lat_long_no_add$long))

## All addresses found a match!

# Now set as spatial object

lat_long_no_add_tx <- lat_long_no_add %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326) %>%
  st_transform(crs = 3081) %>%
  st_buffer(dist = m_to_mile(1)) %>%
  st_crop(tx_boundary)
  
# mapView(lat_long_no_add_tx)

# intersect locs w/ known HIFLD units
intersect_no_add <- lat_long_no_add_tx %>%
  st_intersection(tx_units_parsed) %>%
  mutate(match_tier = "3",
         match_type = "unit intersect") # demote to tier 3 but assign unit intersect

#55

# intersect_city_diff <- intersect_no_add %>% filter(pm.city != city)

## All non-matches are parsing issues/typos

no_add_geocode <- lat_long_no_add_tx %>%
  filter(pm.uid %!in% intersect_no_add$pm.uid)  %>%
  mutate(match_tier = "3",
         match_type = "address geocode") # remaining observations as coordinates 
# 45 

######
#2 obs outside

# ggplot() +
#   geom_sf(data = tx_boundary, fill = NA, color = scales::alpha("black", alpha = 0.3)) +
#   geom_sf(data = lat_long_no_add_tx, colour = "red") +
#   coord_sf() +
#   theme_void()

######

lat_long_not_tx <- lat_long_no_add %>%
  filter(pm.uid %!in% lat_long_no_add_tx$pm.uid) 

## These two observations are in LA, so drop

# Stitch everything back together

combined_no_add <- bind_rows(no_add_geocode, intersect_no_add) %>%
  distinct(pm.uid, .keep_all = T) %>%
  filter(!st_is_empty(geometry))

# 82, checks out

# saveRDS(combined_no_add, "Data/Intermediate/TX/geocoding/lat_long_no_add_geocoded.rds")

########################
# Part 3: Address the DIC observations with only city information 
########################


#load DIC locations that only have city information
pm_only_city <- readRDS("data/intermediate/geocoding/dic_locs_city_only.rds") %>% 
  dplyr::select(-county) %>%
  distinct()

# 51 cities: assign all polygons in that city 
## assign a polygon that is all the polygons for a city's prisons/jails

city_tdcj_polys <- tdcj_hifld_comb %>%
  group_by(city, custody) %>%
  summarize(geometry=st_combine(geometry)) %>% 
  ungroup %>% # Combine all city x custody type polygons into one multipolygon
  st_as_sf() 

city_fac_match <- pm_only_city %>% 
  dplyr::select(-address, -address_full) %>%
  distinct(pm.uid, custody, .keep_all=T) %>% 
  inner_join(city_tdcj_polys, by = c("city", "custody")) %>% 
  st_as_sf()

## 4 observations aren't matched 

# map to see what it looks like

# mapView(city_fac_geom, zcol = "city")

city_fac_geom <- city_fac_match %>%
  mutate(match_type = "city",
         match_tier = "3",
         pm.uid = as.character(pm.uid)) # 3 because not very precise

################################################################################

# rm(list=ls())

# rm(list = ls()[sapply(mget(ls()), is.data.frame)])

# Part 4: 

# Clean up datasets to prepare to combine

# good_matches 
# pm_hifld_intersect
# pm_no_intersect
# combined_no_add

cols <- colnames(good_matches)

## Tier 1 address or unit match w/ HIFLD 

## Tier 2 unit matches by intersection w/ HIFLD 

## Tier 3 missing addresses geocoded

# Combine matched locations

all_locs_w_pm <- bind_rows(good_matches, 
                           pm_hifld_intersect, 
                           pm_no_intersect,
                           combined_no_add,
                           city_fac_geom %>% mutate(pm.state = "TX",
                                                    unit_name = "CITY MATCH")) 

# dups <- filter(all_locs_w_pm, n == 2)

saveRDS(all_locs_w_pm, "data/intermediate/geocoding/dic_locs_geocoded.rds") 

########################

# Clean house

rm(list = ls()[sapply(mget(ls()), is.data.frame)])

