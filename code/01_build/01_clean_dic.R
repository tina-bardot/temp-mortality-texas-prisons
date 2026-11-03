# ==============================================================================
# 01_build/01_clean_dic.R
# (formerly texas/01a_clean_tx_DIC.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
################################################################################
# Data cleaning for TX custodial deaths
# National Center for Environmental Economics
# Last edited: 12/18/2024
################################################################################

rm(list = ls())


# Load Packages
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  readr,
  tidyverse,
  dplyr, # data wrangling
  lubridate, # Date object handling
  readxl
)

## input files:
# 1: Raw DIC data from TJI website

## output files:
# 1: partially cleaned DIC data.frame: tx_dic_for_parse.rds

################################################################################
# Step 1
# Source functions

# Function to remove duplicate strings

source("code/functions/rem_dup.R")

# Function to normalize address components

source("code/functions/fix_address.R")

source("code/functions/negate.R")


################################################################################
## Load Data
################################################################################

## Raw DIC data from TJI website, downloaded 12/28/2024
# https://texasjusticeinitiative.org/datasets/custodial-deaths

tx_dic_raw <- read.csv("data/raw/tji/tji_custodial-deaths_12182024.csv")

# summary(tx_dic_raw)
tx_dic_raw$death_date <- as.Date(tx_dic_raw$death_date, format = '%Y-%m-%d')

tx_dic_raw$date_of_birth <- as.Date(
  tx_dic_raw$date_of_birth,
  format = '%Y-%m-%d'
)

tx_dic <- tx_dic_raw %>%
  dplyr::select(
    record_id,
    date_of_birth,
    age_at_time_of_death,
    sex,
    race,
    death_date,
    death_location_city,
    death_location_street_address,
    death_location_county,
    death_location_type,
    death_from_pre_existing_medical_condition,
    manner_of_death,
    medical_cause_of_death,
    type_of_custody,
    days_from_custody_to_death,
    -contains("agency_")
  ) %>%
  mutate(
    age = as.numeric(difftime(death_date, date_of_birth, units = "weeks")) /
      52.25
  ) %>% # get approx age in years
  mutate(
    age = if_else(is.na(age) | age < 0, age_at_time_of_death, floor(age))
  ) %>%
  rename(death_ymd = death_date) %>%
  mutate(age = ifelse(age <= 0, NA, age))

count(tx_dic_raw, type_of_custody)

# summary(tx_dic$age)
#
# filter(tx_dic, is.na(age)) %>% view()
# filter(tx_dic, age<12) %>% view()

# mutate(tx_dic, age_diff = age - age_at_time_of_death) %>% filter(age_diff != 0) %>% view()

###############################################################################
## Clean Data : city and county names
################################################################################

################################################################################
# The first set of cleaning is fixing typos in city names

tx_dic_new <- tx_dic %>%
  rename(city = death_location_city) %>%
  rename(county = death_location_county) %>%
  rename(address = death_location_street_address) %>%
  rename(custody = type_of_custody) %>%
  mutate(custody = ifelse(str_detect(custody, "JAIL"), "JAIL", custody)) %>%
  mutate(address = fix_address(address)) %>%
  mutate(city = gsub("COLONY", "C", city)) %>% # Fixing all of the misspellings in the dataset
  mutate(city = gsub(", TEXAS", "", city)) %>%
  mutate(city = gsub(", TX.", "", city)) %>%
  mutate(city = gsub(" TX", "", city)) %>%
  mutate(city = gsub(", TX", "", city)) %>%
  mutate(city = gsub(" TX", "", city)) %>%
  mutate(city = gsub(", LA", "", city)) %>%
  mutate(city = gsub("\\.", "", city)) %>%
  mutate(city = gsub("\\,", "", city)) %>%
  mutate(
    city = gsub(
      "[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]",
      "",
      city
    )
  ) %>%
  mutate(
    city = case_when(
      city %in%
        c(
          "TENNESSEE",
          "TENNESSE C",
          "TENNESSEE CQ",
          "TENESSEE C",
          "TENENSSEE C",
          "TENNEESEE C",
          "TENNESSEE C OLONY",
          "TENNESSEE COLON",
          "TENNESSEE COLONG",
          "TENNESSEE CPLONY",
          "TENNESSES C",
          "THE C",
          "TENESSEE C",
          "TENNESSEE C",
          "TENNESSESS C"
        ) ~ "THE COLONY",
      city == "AIICE" ~ "ALICE",
      city == "HUMBER" ~ "HUMBLE",
      city %in% c("dicKENSON", "DICKENSON", "DICKISON") ~ "DICKINSON",
      city %in%
        c(
          "HUNSTVILLE",
          "HUSTVILLE",
          "HUTNSVILLE",
          "HUTSVILLE",
          "HUNTSVILE"
        ) ~ "HUNTSVILLE",
      city %in% c("GLAVESTON", "GAVLESTON", "GALVES") ~ "GALVESTON",
      city == "DALLS" ~ "DALLAS",
      city %in% c("FT. WORTH", "FT WORTH") ~ "FORT WORTH",
      city %in% c("FT. STOCKTON", "FT STOCKTON") ~ "FORT STOCKTON",
      city %in% c("GATESVILLED", "GATEVILLE") ~ "GATESVILLE",
      city %in% c("AMARLLO", "AMARILLA", "AMARILO") ~ "AMARILLO",
      city %in% c("BEAMONT", "BEAUMONTH", "BEAUMON") ~ "BEAUMONT",
      city %in%
        c(
          "BEEVILE",
          "BEEVILLE-8982",
          "BEEVILLE TEXAS",
          "BEEWILLE"
        ) ~ "BEEVILLE",
      city == "COLORADO" ~ "COLORADO CITY",
      city %in% c("CUERRO", "CURERO") ~ "CUERO",
      city %in% c("EDINBERG", "EDINBURGH") ~ "EDINBURG",
      city == "GROESBUCK" ~ "GROESBECK",
      city == "KENNEDY" ~ "KENEDY",
      city == "LAGRANGE" ~ "LA GRANGE",
      city %in% c("LUBBCOK", "LUBBOC") ~ "LUBBOCK",
      city == "MT. PLEASANT" ~ "MOUNT PLEASANT",
      city == "ODESS" ~ "ODESSA",
      city %in% c("PALASTINE", "PA;ESTINE") ~ "PALESTINE",
      city == "RAMONDVILLE" ~ "RAYMONDVILLE",
      city %in% c("RICHMON", "RICHMONED") ~ "RICHMOND",
      city == "RIO GRNADE CITY" ~ "RIO GRANDE CITY",
      city == "ROSESHARON" ~ "ROSHARON",
      city == "SUGARLAND" ~ "SUGAR LAND",
      city == "PASDENA" ~ "PASADENA",
      city == "SAN ANRTONIO" ~ "SAN ANTONIO",
      city == "TEXAS" ~ "TEXAS CITY",
      city == "WAXAHAXHIE" ~ "WAXAHACHIE",
      city == "WICHITA" ~ "WICHITA FALLS",
      city == "ARKANSAS PASS" ~ "ARANSAS PASS",
      city == "SPENDORA" ~ "SPLENDORA",
      city == "TRININTY" ~ "TRINITY",
      city %in% c("ABILIENE", "ABILEN") ~ "ABILENE",
      city == "ARLINGTION" ~ "ARLINGTON",
      city == "BRACKETVILLE" ~ "BRACKETTVILLE",
      city == "CORPUS CHRISIT" ~ "CORPUS CHRISTI",
      city == "ELEMENDORF" ~ "ELMENDORF",
      city == "BIRKBURNETT" ~ "BURKBURNETT",
      city == "DELEON" ~ "DE LEON",
      city == "EDDY" ~ "BRUCEVILLE-EDDY",
      city == "FREDERICKSBUG" ~ "FREDERICKSBURG",
      city == "HARLETON (741, -791)" ~ "HARLETON",
      city %in% c("HOUSTON (HUFFMAN)", "KINGWOOD") ~ "HOUSTON",
      city == "HORIZON" ~ "HORIZON CITY",
      city %in% c("IOWA", "IOWA C", "IOWA CITY") ~ "IOWA COLONY",
      city == "NAVASOTO" ~ "NAVASOTA",
      city == "JACINTO" ~ "JACINTO CITY",
      city == "LEAGEU CITY" ~ "LEAGUE CITY",
      city == "MT PLEASANT" ~ "MOUNT PLEASANT",
      city == "NEAR MARSHALL" ~ "MARSHALL",
      city == "NEAREST CITY- SIERRA BLANCA" ~ "SIERRA BLANCA",
      city == "NEW BOSTON TEXAS" ~ "NEW BOSTON",
      city %in% c("UNICORPORATED", "UNINCORPORATED", "UNINCORPORATED") ~ NA,
      city %in% c("NONE", "UNKNOWN") ~ NA,
      city == "SUGARLAND" ~ "SUGAR LAND",
      city == "SAN LEON (UNINCORPORATED)" ~ "SAN LEON",
      city == "ATASCOCITA RD" ~ "ATASCOCITA",
      city == "LEANA" ~ "LEANNA",
      str_detect(city, "LINDON") ~ "LINDEN",
      city == "SARDIS COMMUNITY (CR2218)" ~ "SARDIS",
      city %in% c("OLHITO", "OLMITO TEXAS") ~ "OLMITO",
      city == "LAGRANGE" ~ "LA GRANGE",
      city == "MT. VERNON" ~ "MOUNT VERNON",
      city == "HEBRONVILLE" ~ "HEBBRONVILLE",
      city == "ORANGE TEXAS" ~ "ORANGE",
      city == "SYNDER" ~ "SNYDER",
      city == "SKYVIEW" ~ "RUSK",
      TRUE ~ city
    )
  ) %>%
  group_by(city) %>%
  mutate(county = na_if(county, "")) %>%
  tidyr::fill(county) %>%
  mutate(
    county = case_when(
      county == "VAN_ZANDT" ~ "VAN ZANDT",
      county == "TOM_GREEN" ~ "TOM GREEN",
      county == "LIVE_OAK" ~ "LIVE OAK",
      county == "DE WITT" ~ "DEWITT",
      county == "DALLAM" ~ "DALLAS",
      city == "BROWNFIELD" ~ "TERRY",
      city == "CUERO" ~ "DEWITT",
      city == "DALHART" ~ "HARTLEY",
      city == "BRIDGEPORT" ~ "WISE",
      city == "BONHAM" ~ "FANNIN",
      city == "OVERTON" ~ "RUSK",
      city == "BARTLETT" ~ "WILLIAMSON",
      city == "SAN DIEGO" ~ "DUVAL",
      city == "MINERAL WELLS" ~ "PALO PINTO",
      TRUE ~ county
    )
  ) %>%
  mutate(city = gsub("25 MILES EAST OF GREENVILLE", "GREENVILLE", city)) %>%
  mutate(city = gsub("5 MILES EAST OF MAGNOLIA", "MAGNOLIA", city)) %>%
  mutate(city = gsub("5 MILES SOUTH OF CALDWELL", "CALDWELL", city)) %>%
  mutate(city = gsub("51 MILES EAST OF GLEN FLORA", "GLEN FLORA", city)) %>%
  mutate(city = gsub("58 MILES S GEORGE WEST", "GEORGE WEST", city)) %>%
  mutate(city = gsub(" (NEARBY)", "", city)) %>%
  filter(city != "EPPS, LA.", city != "OLLA, LA") %>%
  ungroup() %>%
  mutate_all(trimws)


# filter(tx_dic_new, age < 17) %>% view
# filter(tx_dic_new, is.na(age)) %>% count(death_location_type) %>% view()
# filter(tx_dic_new, is.na(age)) %>% view
# filter(tx_dic_new, death_ymd == date_of_birth) %>% view

tx_dic_for_parse <- tx_dic_new %>%
  mutate(address_full = gsub("[[:punct:]]", "", address)) %>%
  # mutate(address = rem_dup(address)) %>%
  # mutate(address = gsub(" TEXAS|TX", "", address)) %>%
  mutate(address_full = paste0(address, ", ", city, ", TX")) %>%
  group_by(city) %>%
  fill(county, .direction = "downup") %>%
  ungroup %>%
  filter(custody != "POLICE CUSTODY (PRE-BOOKING)") %>% # select only deaths in jail and prison facilities
  filter(!str_detect(address, "BOX")) %>%
  dplyr::select(-age_at_time_of_death) %>%
  filter(age >= 17 | !is.na(age)) # remove juveniles

sum(is.na(tx_dic_for_parse$county))

# 11662 observations left

# saveRDS(tx_dic_new, "Data/Intermediate/TX/geocoding/tx_dic_clean.rds")

saveRDS(tx_dic_for_parse, "data/intermediate/dic/dic_clean.rds")

# Note: sensitivity analysis excluding observation where the death date is after the report date

## note add age bins
