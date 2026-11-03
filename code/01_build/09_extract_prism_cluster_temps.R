# ==============================================================================
# 01_build/09_extract_prism_cluster_temps.R
# (formerly texas/01g_extract_prism_temps.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
################################################################################
# Getting prism data for temperatures
# National Center for Environmental Economics
# Last edited: 8/29/2024
################################################################################

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  magrittr,
  tidyverse,
  dplyr, 
  data.table,
  lubridate,
  arrow,
  prism,
  sf,
  raster,
  exactextractr,
  archive
)

## input data:
# 1. final dataset with cluster_id locations: locs_w_pmuid_cluster_id_final.rds

## output data: 
# 1. daily temperature data for cluster ids

################################################################################
# Set up environment
################################################################################

cluster_ids <- readRDS('data/final/cluster_locations.rds') %>%
  st_make_valid %>%
  distinct(cluster_id, .keep_all=T)

# 416 locations

options(timeout = max(10000, getOption("timeout")))

# Set directory that the prism data will be saved to

if (!dir.exists('data/raw/prism_tmp/')) {dir.create('data/raw/prism_tmp/', recursive = T)}

prism::prism_set_dl_dir('data/raw/prism_tmp')

# get_prism_dailys(
#   type = "tmean", 
#   minDate = "2013-06-01", 
#   maxDate = "2013-06-14", 
#   keepZip = FALSE
# )

get_prism_dailys(type = 'tmean', minDate = "1999-01-31", maxDate = "1999-01-31", keepZip = F)
dir    = 'data/raw/prism_tmp'
subdir = list.files(dir)
path   = list.files(dir, full.names = T)
raster = raster(paste(path, "/", subdir, ".bil", sep = ""))
polys =
  cluster_ids %>%
  st_as_sf() %>%
  st_transform(st_crs(raster))
unlink(dir, recursive = T)

## export to skip computation time above

polys %>% 
  dplyr::select(cluster_id) %>%
  # mutate(geo_id = row_number()) %>%
  saveRDS('data/intermediate/temperature/cluster_polys_prism_crs.rds')


# polys %>% 
#   dplyr::select(geo_id, geometry) %>%
#   mutate(geo_id = row_number()) %>%
#   saveRDS('Data/cluster_id_temps/tx_locs_w_PRISM.rds')


## check projections
# plot(polys)
# plot(raster, add = T)

# Loop for daily the variables across all years

var_type <- c(
  "tmean",  
  'tmax',
  'tmin',
  'tdmean')
# var_type <- 'tmean'

# var_type <- 'tmin'

# var_type <- 'tmax'

# var_type <- 'tdmean'

## read from above
polys = 
  readRDS('data/intermediate/temperature/cluster_polys_prism_crs.rds') 


for(i in var_type) {
  
  
  for (DATE in as.character(seq(ymd("2005-01-01"), ymd("2024-11-30"), "days"))){
    
    # ## test date parameter
    # DATE = "1999-12-02"
     
     # test tmean
     # i = 'tmean'
    
    ## create temporary directory if it doesn't yet exist
    if (!dir.exists(paste0('data/raw/prism_tmp/',i,"/", DATE))) {dir.create(paste0('data/raw/prism_tmp/',i,"/", DATE), recursive = T)}
    dir = paste0('data/raw/prism_tmp/',i,"/", DATE)

    prism::prism_set_dl_dir(paste0(dir))

    
    ## set destination for PRISM raw .bil files
    options(prism.path = dir)
    get_prism_dailys(type = i, minDate = DATE, maxDate = DATE, keepZip = F)

    ## paths
    subdir = list.files(dir)
    path   = list.files(dir, full.names = T)
    file   = paste(path, "/", subdir, ".bil", sep = "")
    
    ## create directory if it doesn't yet exist
    if (!dir.exists(paste0('data/intermediate/temperature/cluster_daily/',i,'/', year(DATE)))) {dir.create(paste0('data/intermediate/temperature/cluster_daily/',i,'/', year(DATE)), recursive = T)}
    
    ## extract
    polys %>% 
      mutate(mean = exact_extract(raster(file), polys, 'mean')) %>% 
      st_drop_geometry() %>% 
      write_feather(paste0('data/intermediate/temperature/cluster_daily/', i,'/', year(DATE), '/',i,'_', DATE, '.feather'))
    
    ## delete prism file directory
    unlink(dir, recursive = T)
    
    ## clean house
    gc()
  }
}  

###


## OPTIONAL PATCH: re-download a single date/variable if a daily download failed
## above (originally used for tmin on 2012-08-03). Set RUN_PATCH <- TRUE to use.
RUN_PATCH <- FALSE
if (RUN_PATCH) {
  for (DATE in as.character(seq(ymd("2012-08-03"), ymd("2012-08-03"), "days"))){

    # ## test date parameter
    # DATE = "1999-12-02"

    # test tmean
    i = 'tmin'

    ## create temporary directory if it doesn't yet exist
    if (!dir.exists(paste0('data/raw/prism_tmp/',i,"/", DATE))) {dir.create(paste0('data/raw/prism_tmp/',i,"/", DATE), recursive = T)}
    dir = paste0('data/raw/prism_tmp/',i,"/", DATE)

    prism::prism_set_dl_dir(paste0(dir))


    ## set destination for PRISM raw .bil files
    options(prism.path = dir)
    get_prism_dailys(type = i, minDate = DATE, maxDate = DATE, keepZip = F)

    ## paths
    subdir = list.files(dir)
    path   = list.files(dir, full.names = T)
    file   = paste(path, "/", subdir, ".bil", sep = "")

    ## create directory if it doesn't yet exist
    if (!dir.exists(paste0('data/intermediate/temperature/cluster_daily/',i,'/', year(DATE)))) {dir.create(paste0('data/intermediate/temperature/cluster_daily/',i,'/', year(DATE)), recursive = T)}

    ## extract
    polys %>%
      mutate(mean = exact_extract(raster(file), polys, 'mean')) %>%
      st_drop_geometry() %>%
      write_feather(paste0('data/intermediate/temperature/cluster_daily/', i,'/', year(DATE), '/',i,'_', DATE, '.feather'))

    ## delete prism file directory
    unlink(dir, recursive = T)

    ## clean house
    gc()
  }
}
