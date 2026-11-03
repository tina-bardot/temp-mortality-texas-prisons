################################################################################
# run_all.R
# Reproduces the data build, descriptive tables, and regression tables/figures.
# Run from the repository root (open tx-prison-temp-mortality.Rproj, or setwd()
# to this folder). Replaces texas/07_run_qp_models_and_tables.R.
#
# Each step runs in its own environment so scripts that call rm(list = ls())
# cannot clear this runner. Model + table scripts that share in-memory model
# objects run together in one environment.
################################################################################

## ---- switches ---------------------------------------------------------------
RUN_BUILD           <- TRUE   # 01_build: raw -> final data (PRISM steps take days)
RUN_PRISM_DOWNLOADS <- FALSE  # 09 and 15 download ~30k PRISM rasters each
RUN_DESCRIPTIVES    <- TRUE
RUN_QP_MODELS       <- TRUE
RUN_BINNED_MODELS   <- TRUE
RUN_GENPOP_CENSORED <- FALSE  # 23 is slow (custom censored QP; was run on a server)

## ---- helpers -----------------------------------------------------------------
source("code/00_setup_dirs.R")

run_group <- function(...) {
  files <- c(...)
  env <- new.env(parent = globalenv())
  for (f in files) {
    message("\n==== ", f, " ====")
    source(f, local = env, echo = FALSE)
  }
  invisible(env)
}

b <- function(x) file.path("code/01_build", x)
d <- function(x) file.path("code/02_descriptives", x)
a <- function(x) file.path("code/03_analysis", x)

## ---- 1. data build -------------------------------------------------------------
if (RUN_BUILD) {
  # deaths in custody
  run_group(b("01_clean_dic.R"))
  run_group(b("02_parse_dic_addresses.R"))
  # facilities
  run_group(b("03_get_tdcj_facilities.R"))
  run_group(b("04_parse_facilities_tdcj_hifld.R"))
  # geocode, match, cluster
  run_group(b("05_geocode_dic_locations.R"))
  run_group(b("06_combine_facility_polygons.R"))
  run_group(b("07_match_and_cluster_locations.R"))
  run_group(b("08_assign_city_county_to_clusters.R"))
  # cluster temperatures
  if (RUN_PRISM_DOWNLOADS) run_group(b("09_extract_prism_cluster_temps.R"))
  run_group(b("10_combine_cluster_temps.R"))
  run_group(b("11_create_temp_thresholds.R"))
  # analysis panels
  run_group(b("12_join_dic_cluster_temps.R"))
  run_group(b("13_aggregate_prison_city_county.R"))
  # general-population comparison inputs
  run_group(b("14_clean_county_population.R"))
  if (RUN_PRISM_DOWNLOADS) run_group(b("15_extract_prism_county_temps.R"))
  run_group(b("16_build_county_genpop_panel.R"))
}

## ---- 2. descriptives -----------------------------------------------------------
if (RUN_DESCRIPTIVES) {
  run_group(d("01_mortality_summary_tables.R"))
  run_group(d("02_state_threshold_tables.R"))
  run_group(d("03_temp_summary_tables.R"))
  run_group(d("04_prison_dic_summary_stats.R"))
  run_group(d("05_prison_location_summary_stats.R"))
}

## ---- 3. quasi-Poisson threshold and lag models ------------------------------------
if (RUN_QP_MODELS) {
  # hot, state thresholds
  run_group(a("01_model_hot_threshold_state.R"), a("02_tab_hot_threshold_state.R"))
  run_group(a("03_model_hot_lag_state.R"), a("04_tab_hot_lag_state.R"), a("05_tab_hot_cumlag_state.R"))
  # hot, city thresholds
  run_group(a("06_model_hot_threshold_city.R"), a("07_tab_hot_threshold_city.R"))
  run_group(a("08_model_hot_lag_city.R"), a("09_tab_hot_lag_city.R"), a("10_tab_hot_cumlag_city.R"))
  # cold, state thresholds
  run_group(a("11_model_cold_threshold_state.R"), a("12_tab_cold_threshold_state.R"))
  run_group(a("13_model_cold_lag_state.R"), a("14_tab_cold_lag_state.R"), a("15_tab_cold_cumlag_state.R"))
  # cold, city thresholds
  run_group(a("16_model_cold_threshold_city.R"), a("17_tab_cold_threshold_city.R"))
  run_group(a("18_model_cold_lag_city.R"), a("19_tab_cold_lag_city.R"), a("20_tab_cold_cumlag_city.R"))
}

## ---- 4. binned temperature models (prison vs general population) ----------------
if (RUN_BINNED_MODELS) {
  if (RUN_GENPOP_CENSORED) {
    run_group(a("23_model_binned_genpop_censored.R"))   # writes output/regression_results/*.Rdata
  }
  run_group(a("24_tab_binned_genpop.R"))                # reads output/regression_results/*.Rdata
  run_group(a("21_model_binned_dic.R"),
            a("22_tab_binned_dic.R"),
            a("25_test_equality_dic_genpop.R"))         # needs 21 objects + 23 results
}

message("\nDone.")
