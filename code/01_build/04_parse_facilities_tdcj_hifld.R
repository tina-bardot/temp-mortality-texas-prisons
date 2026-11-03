# ==============================================================================
# 01_build/04_parse_facilities_tdcj_hifld.R
# (formerly texas/01b_parse_tdcj_add.R). Run from the repo root; see README.md for the data map.
# ==============================================================================

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  sf, # vector data operations
  dplyr, # data wrangling
  lubridate, # Date object handling
  janitor
)

source("code/functions/fix_address.R")

source("code/functions/negate.R")

# Parse addresses 

# install.packages("remotes")
# remotes::install_github("chris-prener/postmastr")

library(postmastr)

## input files: 
# 1. partially cleaned sf of tdcj coordinates: tdcj_units.rds

## output files: 
# 1. parsed TDCJ addresses
# 2. parsed HIFLD addresses
# 3. combined parsed addresses: tdcj_hifld_comb.rds

## Clean house 

rm(list = ls()[sapply(mget(ls()), is.data.frame)])

## Load data: 

tdcj_units <- readRDS("data/intermediate/facilities/tdcj_units.rds") %>%
  dplyr::select(unit_name, address, city, state) %>%
  mutate(across(1:4, toupper)) %>%
  mutate(city = gsub("TENNESSEE COLONY", "THE COLONY", city)) %>%
  mutate(address_full = paste(address, city, state, sep = " "))

## Step 1: prep

pm_add <- pm_identify(tdcj_units, var = "address_full")

pm_add_min <- pm_prep(pm_add, var = "address_full", type = "street")

## Step 2: postal code

pm_postal_any(pm_add_min)

# FALSE

## Step 3: state

# Create dictionary

txDict <- pm_dictionary(locale = "us", type = "state", filter = "TX", case = "upper")

# Parse state

pm_add_min <- pm_state_parse(pm_add_min, dictionary = txDict, locale ="us")

## Step 4: city

# Create dictionary

cityDict <- pm_dictionary(locale = "us", type = "city", filter = "TX", case = "upper")

# check cities, parse

pm_city_all(pm_add_min, dictionary = cityDict)

pm_add_min <- pm_city_parse(pm_add_min, dictionary = cityDict)

## Step 5: unit

# Function not enabled yet

## Step 6: house number

pm_house_all(pm_add_min)

pm_no_house <- pm_house_none(pm_add_min)

pm_add_min <- pm_house_parse(pm_add_min)

## Step 7: street directionals

dirs <- pm_dictionary(type = "directional", case = "upper", locale = "us")

pm_add_min <- pm_streetDir_parse(pm_add_min, dictionary = dirs)

## Step 8: street suffix

pm_add_min <- pm_streetSuf_parse(pm_add_min)

## Step 9: street name

pm_add_min <- pm_street_parse(pm_add_min)

## Step 10: reconstruct

pm_parsed <- pm_replace(pm_add_min, source = pm_add)

pm_add_parsed <- pm_rebuild(pm_parsed, output = "short", keep_parsed = "no") %>%
  mutate(pm.address = toupper(pm.address))

## Have all the parsed addresses, save for future use

saveRDS(pm_add_parsed, "data/intermediate/facilities/tdcj_units_parsed.rds")

################################################################################
# Parse through HIFLD data
################################################################################

hifld_units <- st_read("data/raw/hifld/Prison_Boundaries/Prison_Boundaries.shp") %>%
  rename_all(tolower)

hifld_units <- hifld_units %>%
  # st_as_sf() %>%
  filter(state == "TX") %>%
  mutate(city = gsub("TENNESSEE COLONY", "THE COLONY", city)) %>%
  mutate(population = na_if(population, -999)) %>% #per the codebook, -999 denotes NA value
  st_transform(crs = 3081) %>%
  dplyr::select(-c(16:21), -(25:32)) %>%
  mutate(custody = case_when(
    str_detect(name, "JAIL") & !str_detect(name, "DETENTION") ~ "JAIL",
    str_detect(name, "JUVENILE") ~ "JUVENILE",
    TRUE ~ "PRISON" # NOTE: Puts detention centers (multi-use) as default prisons
  ), .after = state) %>%
  mutate(address = fix_address(address))

saveRDS(hifld_units, "data/intermediate/facilities/hifld_tx_parsed.rds")


################################################################################
# compare with HIFLD data
################################################################################

tx_units <- readRDS("data/intermediate/facilities/tdcj_units_parsed.rds")

# read in HIFLD data

hifld_units <- readRDS("data/intermediate/facilities/hifld_tx_parsed.rds") %>%
  dplyr::select(name, address, city, state, custody, type, status, county) %>%
  rename(unit_name = name) %>%
  mutate(address_full = paste(address, city, state, sep = " "),
         pm.address = address)

library(stringdist)

match <- amatch(tx_units$address, hifld_units$address, maxDist = 10)

units_match <- hifld_units[amatch(tx_units$address, hifld_units$address, maxDist = 10),]

units_match_comb <- data.frame(tdcj = tx_units$address,
                               hifld = units_match$address) %>%
  filter(!is.na(hifld))

tdcj_only <- tx_units %>%
  st_as_sf() %>%
  filter(address %!in% units_match_comb$tdcj) %>%
  mutate(source = "tdcj") %>%
  st_transform(crs = st_crs(hifld_units))

# now prepare dataset

hifld <- hifld_units  %>%
  mutate(source = "hifld")

comb <- bind_rows(hifld, tdcj_only)  %>%
  filter(custody != "JUVENILE") %>%
  filter(type != "FEDERAL") %>%
  mutate(unit_id = row_number())

saveRDS(comb, "data/intermediate/facilities/facilities_tdcj_hifld.rds") # condensed data with HIFLD and TDCJ data
