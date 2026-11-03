# ==============================================================================
# 01_build/02_parse_dic_addresses.R
# (formerly texas/01b_parse_dic_address.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
# Memo: this script parses the address names from the DIC mortality dataset
# to create uniform addresses

## 0. SET-UP
# Load packages

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    readr,
    sf, # vector data operations
    dplyr, # data wrangling
    lubridate, # Date object handling
    janitor
  )

# Parse addresses 

  # install.packages("remotes")
  # remotes::install_github("chris-prener/postmastr")
  
  library(postmastr)
  
  
## input files: 
# 1. partially cleaned DIC file : tx_dic_for_parse.rds
  
  
## output files: 
  # 1. parsed addresses for death locations: intermediate_pm_add.rds
  # 2. dic data with pm.uid identifier: dic_with_pm_uid.rds
  # 3. death locations with only city information: pm_only_city_pm_uid.rds
  # 4. death locations with partial address or unit names: pm_no_address.rds

## Clean house 

rm(list = ls()[sapply(mget(ls()), is.data.frame)])

###
source("code/functions/rem_dup.R")
source("code/functions/negate.R")

## Load data: 

  tx_dic_for_parse <- readRDS("data/intermediate/dic/dic_clean.rds")
  # %>%
    # mutate(address_full = rem_dup(address_full))
  
  # 
  
  cities <- unique(tx_dic_for_parse$city)

# 2,372 observations do not have an address

## Step 1: prep

  pm_add <- pm_identify(tx_dic_for_parse, var = "address_full")
  
  dic_no_add <- filter(pm_add, address == "") %>%
    dplyr::select(pm.uid, city, address, county, address_full, custody)
  
  saveRDS(dic_no_add, "data/intermediate/geocoding/dic_locs_city_only.rds")
  ## Separate locations with no address for city level matching

  saveRDS(pm_add, "data/intermediate/dic/dic_with_pm_uid.rds")
  
  pm_add <- filter(pm_add, pm.uid %!in% dic_no_add$pm.uid) # remove the observations with no address 
  
  pm_add_min <- pm_prep(pm_add, var = "address_full", type = "street")
  
## Step 2: postal code
  #Determine whether the postal code test returns any matches
  pm_postal_any(pm_add_min)
  
  # FALSE # no - data do not contain postal code

## Step 3: state

# Create dictionary

  txDict <- pm_dictionary(locale = "us", type = "state", filter = "TX", case = "upper")

# Parse state

  pm_add_min <- pm_state_parse(pm_add_min, dictionary = txDict, locale ="us")

## Step 4: city

# Create dictionary

  cityDict <- pm_dictionary(locale = "us", type = "city", filter = "TX", case = "upper")
  
  cityDict[nrow(cityDict) +1, ] <- "DEL VALLE"
  cityDict[nrow(cityDict) +1, ] <- "DFW AIRPORT"
  cityDict[nrow(cityDict) +1, ] <- "PENWELL"
  

# check cities

  pm_city_all(pm_add_min, dictionary = cityDict)
  
  pm_no_city <- pm_city_none(pm_add_min, dictionary = cityDict) 

# save the addresses with no cities separately
  
  # 44 observations

# saveRDS(pm_no_city, file = "Data/Intermediate/TX/geocoding/pm_no_city.rds")

# Omit the addresses that can't be parsed: will figure out later

## pm_add_min <- filter(pm_add_min, pm.address %!in% pm_no_city$pm.address)

pm_add_min <- pm_city_parse(pm_add_min, dictionary = cityDict)

## Step 5: unit

# Function not enabled yet

## Step 6: house number

pm_house_all(pm_add_min)

pm_no_house <- pm_house_none(pm_add_min)

pm_no_address <- bind_rows(pm_no_city, pm_no_house) #combine observations without full address

## 159 obs

saveRDS(pm_no_address, "data/intermediate/geocoding/dic_locs_no_address.rds")

pm_add_min <- filter(pm_add_min, pm_add_min$pm.uid %!in% pm_no_address$pm.uid) # remove observations that we split off 

pm_add_min <- pm_house_parse(pm_add_min)

## Step 7: street directionals

dirs <- pm_dictionary(type = "directional", case = "upper", locale = "us")
  
pm_add_min <- pm_streetDir_parse(pm_add_min, dictionary = dirs)

## Step 8: street suffix

pm_add_min <- pm_streetSuf_parse(pm_add_min)

## Step 9: street name

## Intermediate: fix up values that got messed up

pm_add_min_2 <- pm_add_min %>%
  mutate(pm.street = pm.address)

# saveRDS(pm_add_min_2, "Data/Intermediate/TX/geocoding/intermediate_dic_pm_add.rds")

# dic_pm_add <- readRDS("Data/Intermediate/TX/geocoding/intermediate_dic_pm_add.rds") # parsed addresses

dic_pm_add <- pm_add_min_2

dic_pm_add[is.na(dic_pm_add)] <- ""

dic_pm_add <- dic_pm_add %>%
  mutate(pm.address = paste(pm.house, pm.preDir, pm.street, pm.streetSuf,  pm.sufDir)) %>%
  dplyr::select(pm.uid, pm.address, pm.city, pm.state) %>%
  # mutate(pm.address = gsub(" NA ", "", pm.address)) %>%
  mutate(pm.address = toupper(pm.address)) %>%
  mutate(across(everything(), trimws)) %>%
  mutate(across(everything(), ~str_replace_all( .x, "  ", " "))) %>%
  distinct(pm.uid, .keep_all=T) # get unique locations based on pm. unique id

saveRDS(dic_pm_add, "data/intermediate/geocoding/dic_locs_parsed_address.rds")
