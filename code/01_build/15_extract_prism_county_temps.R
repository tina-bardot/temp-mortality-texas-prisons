################################################################################
# 15_extract_prism_county_temps.R
# County-wide daily PRISM temperatures for all Texas counties, used to match the
# general-population (DSHS) mortality data in 16_build_county_genpop_panel.R.
#
# NEW IN REPO CLEANUP: the working repo read
# Data/Intermediate/Climate data/temps_tx_county.rds, but no script created it.
# This script rebuilds it the same way 09_extract_prism_cluster_temps.R builds
# the cluster temperatures (area-weighted mean of the PRISM 4km daily grid over
# each polygon, via exactextractr), using TxDOT county polygons.
# VERIFY against the original temps_tx_county.rds if it is still available.
#
# Inputs:
#   data/raw/txdot/Texas_County_Boundaries.gdb
#   PRISM daily rasters (downloaded on the fly with the prism package)
# Outputs:
#   data/intermediate/temperature/county_daily/<var>/<year>/<var>_<date>.feather
#   data/intermediate/temperature/county_daily_temps.rds
#     columns: county, date, tmax, tmin, tmean, tdmean
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  lubridate,
  arrow,
  prism,
  sf,
  raster,
  exactextractr
)

options(timeout = max(10000, getOption("timeout")))

start_date <- ymd("2005-01-01")
end_date   <- ymd("2023-12-31") # DSHS data end in 2023
var_type   <- c("tmax", "tmin", "tmean", "tdmean")

tmp_dir <- "data/raw/prism_tmp_county"
out_dir <- "data/intermediate/temperature/county_daily"
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1. County polygons in the PRISM CRS
################################################################################

counties <- st_read("data/raw/txdot/Texas_County_Boundaries.gdb") %>%
  st_as_sf() %>%
  rename(county = CNTY_NM) %>%
  dplyr::select(county) %>%
  st_make_valid()

## download one raster to get the PRISM CRS
prism_set_dl_dir(tmp_dir)
get_prism_dailys(type = "tmean", minDate = "2005-01-01", maxDate = "2005-01-01", keepZip = FALSE)
crs_file <- list.files(tmp_dir, pattern = "\\.bil$", recursive = TRUE, full.names = TRUE)[1]
polys <- st_transform(counties, st_crs(raster(crs_file)))
unlink(tmp_dir, recursive = TRUE)

################################################################################
# 2. Daily extraction
################################################################################

for (i in var_type) {
  for (DATE in as.character(seq(start_date, end_date, "days"))) {

    out_file <- file.path(out_dir, i, year(DATE), paste0(i, "_", DATE, ".feather"))
    if (file.exists(out_file)) next # allows restarting an interrupted run

    dir <- file.path(tmp_dir, i, DATE)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    prism_set_dl_dir(dir)
    get_prism_dailys(type = i, minDate = DATE, maxDate = DATE, keepZip = FALSE)

    file <- list.files(dir, pattern = "\\.bil$", recursive = TRUE, full.names = TRUE)[1]

    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    polys %>%
      mutate(mean = exact_extract(raster(file), polys, "mean")) %>%
      st_drop_geometry() %>%
      write_feather(out_file)

    unlink(dir, recursive = TRUE)
    gc()
  }
}

################################################################################
# 3. Combine into one county x day file
################################################################################

read_var <- function(i) {
  list.files(file.path(out_dir, i), pattern = "\\.feather$", recursive = TRUE, full.names = TRUE) %>%
    map_dfr(~ read_feather(.x) %>%
              mutate(date = str_extract(basename(.x), "\\d{4}-\\d{2}-\\d{2}"))) %>%
    distinct(county, date, .keep_all = TRUE) %>%
    rename(!!i := mean)
}

county_temps <- var_type %>%
  map(read_var) %>%
  reduce(full_join, by = c("county", "date")) %>%
  relocate(county, date)

saveRDS(county_temps, "data/intermediate/temperature/county_daily_temps.rds")
