# ==============================================================================
# 01_build/03_get_tdcj_facilities.R
# (formerly texas/01a_get_tdcj_prison_coord.R). Run from the repo root; see README.md for the data map.
# ==============================================================================

################################################################################
# Extracting prison locations from TDCJ prison roster
# National Center for Environmental Economics
# Last edited: 12/18/2024
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf, # vector data operations
  dplyr, # data wrangling
  readxl,
  janitor
)

# input files: 
# 1. raw facility location file (TDCJ)

# output files: 
# 1. partially cleaned sf of tdcj coordinates: tdcj_units.rds

# Read KML file 
# https://www.tdcj.texas.gov/ks_facility.html

kml_file <- "data/raw/tdcj/TDCJ Find a Facility.kml"

layers <- st_layers(kml_file)

layers_comb = data.frame( 
  Name = character(),
  Description = character(),
  geometry = st_sfc()) %>%
  st_as_sf(crs = 3857) %>%
  st_transform(crs = st_crs(layers))

for (layer in layers$name) {
  layer_data <- st_read(kml_file, layer = layer) %>% st_zm(drop=T)
  # assign(layer, layer_data)
  
  if (layer == layers$name[1]) {
    layers_comb <- layer_data
  }
  else {
    layers_comb <- bind_rows(layers_comb, layer_data)
  }
}

# Facilities (units)

tx_unit_coords <- layers_comb %>%
  filter(str_detect(Description, "FACILIT|UNIT|PRISONS"))  %>%
  separate_wider_delim(Description, delim = "</div><div>", names = c('type', 'region', 'location', 'telephone', 'link'), too_many = 'debug')


# Clean data
tx_units_clean <- tx_unit_coords %>%
  mutate(link = ifelse(str_detect(type, "JAIL"), Description_remainder, link)) %>%
  dplyr::select(-starts_with("Descri")) %>%
  mutate(across(is.character, ~gsub('<div>:', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('\\</div>:', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('<b>', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('</div>', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('<div>', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('</b>:', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('REGION', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('LOCATION', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('TELEPHONE', '', .,  ignore.case = TRUE))) %>%
  mutate(across(is.character, ~gsub('LINK', '', .,  ignore.case = TRUE))) %>%
  rename(unit_name = Name) %>%
  distinct(location, .keep_all = TRUE) %>%
  dplyr::select(unit_name, location, geometry)

# Get city location from address
tx_units <- tx_units_clean %>%
  separate_wider_delim(location, delim = "; ", names = c("address", "city")) %>%
  mutate(city = gsub(" TX", "", city),
         city = gsub(",", "", city)) %>%
  mutate(state = 'TX', .after = city)


# Convert to spatial object and set CRS 

meter_to_mile <- function(x) {
  x*1609.244
}

tx_units_sf <- tx_units %>%
  st_as_sf(crs =  4326)  %>%
  st_transform(crs = 3081) %>% # transform to meter crs
  st_buffer(dist = meter_to_mile(1)) # Create 1 mile buffer 

saveRDS(tx_units_sf, file = "data/intermediate/facilities/tdcj_units.rds")

st_crs(tx_units_sf) # check

## Plot

# Add in the Texas CRS

tx_border <- tigris::states(cb= TRUE) %>%
  st_transform(crs = st_crs(tx_units_sf)) %>%
  filter(STUSPS == "TX")

tx_counties <- tigris::counties("TX") %>%
  st_transform(crs = st_crs(tx_units_sf))

# Plot it 

ggplot() +
  geom_sf(data = tx_counties, fill = NA, color = scales::alpha("black", alpha = 0.3)) +
  geom_sf(data = tx_units_sf, colour = "#e73d34") +
  coord_sf() +
  theme_void() 

