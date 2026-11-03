# ==============================================================================
# 01_build/08_assign_city_county_to_clusters.R
# (formerly texas/01f_combine_location_dic.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
# Combine cluster information with DIC to get finalized location dataset
## Some locations are the same place but have a different city, 
## So we need to create some geographical basis to locate and 
## group clusters
## Using the TX DOT city boundaries, left join and overlay


# install packages: 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, # data wrangling
  tidyr,
  sf,
  janitor
)

## input files: 
# 1. texas dot city boundaries (sf)
# 2. texas dit county boundaries (sf)
# 3. dic locs with cluster id
# 4. parsed dic data with pm.uid: dic_with_pm_uid.rds

## output files: 
# 1. final cluster id data with homogenized city and county by cluster id : locs_w_pmuid_cluster_id_final.rds
# 2. dic data with cluster ids (final): dic_cluster_id_locs_final.rds

# data source: https://gis-txdot.opendata.arcgis.com/datasets/TXDOT::txdot-city-boundaries/about

# step one: clean tx city boundaries data
# (Un-commented in repo cleanup so the boundaries file is rebuilt from raw.)
# st_layers("data/raw/txdot/TxDOT_City_Boundaries.gdb")

tx_city_boundaries <- st_read("data/raw/txdot/TxDOT_City_Boundaries.gdb",
                              layer = "Cities") %>%
  clean_names()

tx_city_boundaries <- tx_city_boundaries %>%
  mutate_at("city_nm", .funs=toupper) %>%
  mutate(city_nm = gsub("THE COLONY", "TENNESSEE C", city_nm))

dir.create("data/intermediate/boundaries", recursive = TRUE, showWarnings = FALSE)
saveRDS(tx_city_boundaries, "data/intermediate/boundaries/txdot_city_boundaries.rds")

tx_city_boundaries <- readRDS("data/intermediate/boundaries/txdot_city_boundaries.rds") %>%
  dplyr::select(city_nm) # geometry column is kept automatically (sf) 

# Step 2: read DIC location data

# read location data 

dic_locs <- readRDS("data/intermediate/clusters/dic_locs_cluster_id.rds")

dic_locs_city <- st_intersection(dic_locs, tx_city_boundaries %>%
                                   st_transform(crs = st_crs(dic_locs))) %>%
  distinct(city_nm, cluster_id, .keep_all =T) %>% 
  mutate(area = st_area(.)) %>% 
  st_drop_geometry() %>% 
  arrange(desc(area)) %>% ## some have overlapping polygons, select largest city boundary
  group_by(cluster_id) %>%
  filter(row_number()==1) %>% ungroup %>% 
  dplyr::select(city_nm, cluster_id)

### Do the same for county boundaries

# Downloaded from : 
# https://gis-txdot.opendata.arcgis.com/datasets/texas-county-boundaries/explore

tx_counties <- st_read("data/raw/txdot/Texas_County_Boundaries.gdb") %>% st_as_sf() %>%
  rename(county = CNTY_NM) %>% dplyr::select(county) %>% 
  mutate(county = toupper(county)) %>% st_transform(crs = st_crs(dic_locs))

dic_locs_county <- st_intersection(dic_locs %>% 
                                     dplyr::select(cluster_id), tx_counties) %>%
  st_drop_geometry %>% # remove geometry type 
  count(cluster_id, county) %>%
  group_by(cluster_id) %>% arrange(cluster_id, desc(n)) %>% filter(row_number() == 1) %>% ungroup %>% 
  dplyr::select(-n)

##### CHECKING OVERLAP (OR LACK THEREOF)

# cluster_no_match <- filter(dic_locs, cluster_id %!in% dic_locs_city$cluster_id) %>% distinct(geometry, .keep_all=T) %>%
#   st_as_sf(crs=4326)
# 
# library(mapview)
# 
# options(viewer = NULL)
# 
# mapview(cluster_no_match) + mapview(tx_city_boundaries, legend = F)

##############

dic_locs_city2 <- dic_locs  %>%
  # group_by(cluster_id) %>% fill(city, .direction = "updown") %>% ungroup() %>%
  left_join(dic_locs_city, relationship = "many-to-many") %>%
  rename(city_og = city) %>%
  rename(city = city_nm) %>%
  mutate(city = ifelse(is.na(city), city_og, city)) %>%
  left_join(dic_locs_county, relationship = "many-to-many") %>%
  group_by(cluster_id) %>% fill(city, county, .direction = "updown") %>% ungroup() 


## for remaining cluster ids with >1 cities (n =6), pick the city that is most common 

city_count <- dic_locs_city2 %>% st_drop_geometry() %>% count(cluster_id, city) %>%
  group_by(cluster_id) %>% arrange(cluster_id, desc(n)) %>% filter(row_number() == 1) %>% ungroup
# dic_locs_city2 %>% distinct(city, cluster_id) %>% View()

dic_locs_city2 <- dic_locs_city2 %>% dplyr::select(-city) %>%
  left_join(city_count %>% dplyr::select(-n))

saveRDS(dic_locs_city2, "data/final/cluster_locations.rds")

#####
# Crosswalk with cluster IDs, locations and geo ids

# Now match DICs to unit ids for geometry matching (when we add in temp data)

cluster_address <- dic_locs_city2 %>%
  st_drop_geometry() %>%
  distinct(cluster_id, pm.uid, city, .keep_all =TRUE)

## Add in cluster id's to dic data

# Note: we are now completely replacing the location data with the matched location data

tx_dic_pm_uid <- readRDS("data/intermediate/dic/dic_with_pm_uid.rds")

tx_dic_cluster_id <- tx_dic_pm_uid %>%
  dplyr::select(pm.uid, record_id, death_ymd, sex, race, age) %>%
  left_join(cluster_address %>% mutate(pm.uid = as.integer(pm.uid)) %>%
              dplyr::select(-c(city_og, unit_geo, query, distance, starts_with("unit"), starts_with("address")))) %>%
  group_by(cluster_id) %>%
  filter(!is.na(cluster_id)) %>% ungroup

###########
### CHECK CHECK : city by cluster id

# city_cluster_id_unique <- distinct(dic_locs, city, cluster_id, .keep_all = T)
# 
# city_cluster_id_unique <- distinct(tx_dic_cluster_id, city, cluster_id, .keep_all = T)

## 11648 remaining observations

saveRDS(tx_dic_cluster_id, "data/final/dic_cluster_crosswalk.rds")

# tx_dic_cluster_id %>% filter(is.na(cluster_id)) %>% View()
