# Extreme temperatures and mortality in Texas prisons

Replication code for [paper title, authors, journal/DOI — to fill in].

This repository builds a daily panel linking deaths in custody in Texas state prisons (2005–2024) to PRISM temperatures at the matched facility, then estimates quasi-Poisson models of mortality on hot- and cold-day thresholds, lags, and temperature bins. It also compares prison deaths with general-population deaths from county data.

## Repository layout

```
tx-prison-temp-mortality/
├── run_all.R                  # runs everything, in order
├── tx-prison-temp-mortality.Rproj
├── code/
│   ├── 00_setup_dirs.R        # creates data/ and output/ folders
│   ├── 01_build/              # raw -> final data (scripts 01–16)
│   ├── 02_descriptives/       # summary-statistics tables
│   ├── 03_analysis/           # models (model_*) and their tables/figures (tab_*)
│   ├── functions/             # helper functions sourced by the scripts
│   └── archive/               # scripts not used for the paper (kept for reference)
├── data/                      # not tracked; see data/README.md
│   ├── raw/  intermediate/  final/
└── output/
    ├── tables/                # .tex tables; coefs_csv/ holds coefficient CSVs
    │   └── archive/           # tables not produced by the current code
    ├── figures/
    └── regression_results/    # saved censored-model objects (.Rdata)
```

## How to run

1. Put the raw inputs in `data/raw/` as described in `data/README.md`.
2. Open `tx-prison-temp-mortality.Rproj` so the working directory is the repo root. All paths are relative to the root.
3. Run `source("run_all.R")`. The switches at the top control the slow steps: `RUN_PRISM_DOWNLOADS` covers scripts 09 and 15, which download about 29,000 daily rasters per variable, and `RUN_GENPOP_CENSORED` covers script 23.

Packages: tidyverse, data.table, sf, sfheaders, units, tigris, spatialEco, fuzzyjoin, stringdist, postmastr (GitHub: slu-openGIS/postmastr), tidygeocoder, prism, raster, exactextractr, arrow, fixest, lfe, sandwich, lmtest, optimParallel, modelsummary, kableExtra, huxtable, gt, summarytools, broom, rlist, readxl, janitor, weathermetrics, reshape2, mapview. Most build scripts load their packages through `pacman::p_load()`. Consider recording exact versions with `renv::snapshot()`.

## Pipeline

| Step | Script (`code/01_build/`) | Main output |
|---|---|---|
| Clean deaths in custody | `01_clean_dic.R` | `data/intermediate/dic/dic_clean.rds` |
| Parse death addresses | `02_parse_dic_addresses.R` | `dic/dic_with_pm_uid.rds`, `geocoding/dic_locs_{parsed_address,no_address,city_only}.rds` |
| TDCJ facility points | `03_get_tdcj_facilities.R` | `facilities/tdcj_units.rds` |
| Combine TDCJ and HIFLD facilities | `04_parse_facilities_tdcj_hifld.R` | `facilities/facilities_tdcj_hifld.rds` |
| Geocode and match death locations | `05_geocode_dic_locations.R` | `geocoding/dic_locs_geocoded.rds` |
| Merge nearby facility polygons | `06_combine_facility_polygons.R` | `facilities/facilities_combined.rds` |
| Distance-match and assign `cluster_id` | `07_match_and_cluster_locations.R` | `clusters/dic_locs_cluster_id.rds` |
| Assign city and county to clusters | `08_assign_city_county_to_clusters.R` | **`final/cluster_locations.rds`**, **`final/dic_cluster_crosswalk.rds`** |
| Extract PRISM to clusters | `09_extract_prism_cluster_temps.R` | `temperature/cluster_daily/<var>/<year>/*.feather` |
| Combine cluster temperatures | `10_combine_cluster_temps.R` | **`final/cluster_daily_temps.rds`** |
| Percentile thresholds | `11_create_temp_thresholds.R` | **`final/temp_thresholds_{state,city,county}.rds`** |
| Join deaths to temperatures; keep prison clusters | `12_join_dic_cluster_temps.R` | `final/panel_cluster_day_dic_temp.rds`, `final/panel_cluster_day_prison.rds` |
| Aggregate to city and county | `13_aggregate_prison_city_county.R` | **`final/panel_city_day_prison.rds`**, **`final/panel_county_day_prison.rds`** |
| County population | `14_clean_county_population.R` | `population/tx_county_pop_2000_2024.rds` |
| Extract PRISM to counties | `15_extract_prism_county_temps.R` | `temperature/county_daily_temps.rds` |
| Stack prison and general-population data | `16_build_county_genpop_panel.R` | **`final/panel_county_day_prison_genpop.rds`** |

Analysis (`code/03_analysis/`). `run_all.R` runs each model script in the same environment as the table scripts that follow it.

| Scripts | Analysis | Outputs |
|---|---|---|
| 01–05 | Hot days, state thresholds: threshold, lag/wave, and cumulative-lag models | `qp_hot_city_state_*.tex`, `coefs_csv/cs_*hot*.csv`, `figures/hot_state_*.png` |
| 06–10 | Hot days, city thresholds | `qp_hot_city_city_*.tex`, `coefs_csv/cc_*hot*.csv` |
| 11–15 | Cold days, state thresholds | `qp_cold_city_state_*.tex` |
| 16–20 | Cold days, city thresholds | `qp_cold_city_city_*.tex` |
| 21–22 | Binned temperature models, prison deaths (city and county) | `binned_reg_dic_*.tex` |
| 23–24 | Censored binned models, general population (DSHS) | `regression_results/*.Rdata`, `Table3e.tex` |
| 25 | Test of equal prison vs general-population coefficients | `Table3f_*.tex`, `Table3g_*.tex` |

## File-name crosswalk (old → new)

| Old path | New path |
|---|---|
| `Data/Raw/State dic/tji_custodial-deaths_12182024.csv` | `data/raw/tji/…` |
| `Data/Raw/TDCJ Find a Facility.kml` | `data/raw/tdcj/…` |
| `Data/Facility locations/Prison_Boundaries/` | `data/raw/hifld/Prison_Boundaries/` |
| `Data/Raw/TxDOT_City_Boundaries.gdb`, `data/raw/Texas_County_Boundaries.gdb` | `data/raw/txdot/…` |
| `data/raw/TX county population/` | `data/raw/tx_county_population/` |
| `Data/Raw/T2_TB_DM-24-0447_daily_deaths_2000-2023.xlsx` | `data/raw/dshs/…` |
| `Data/Intermediate/TX/geocoding/tx_dic_for_parse.rds` | `data/intermediate/dic/dic_clean.rds` |
| `…/geocoding/dic_with_pm_uid.rds` | `data/intermediate/dic/dic_with_pm_uid.rds` |
| `…/geocoding/intermediate_pm_add.rds` | `data/intermediate/geocoding/dic_locs_parsed_address.rds` |
| `…/geocoding/pm_no_address.rds` | `data/intermediate/geocoding/dic_locs_no_address.rds` |
| `…/geocoding/pm_only_city_pm_uid.rds` | `data/intermediate/geocoding/dic_locs_city_only.rds` |
| `…/geocoding/locs_geocoded_all_w_pm.rds` | `data/intermediate/geocoding/dic_locs_geocoded.rds` |
| `Data/Intermediate/TX/tdcj_units.rds` | `data/intermediate/facilities/tdcj_units.rds` |
| `…/geocoding/tdjc_units_parsed.rds` | `data/intermediate/facilities/tdcj_units_parsed.rds` |
| `…/geocoding/hifld_tx_parsed.rds` | `data/intermediate/facilities/hifld_tx_parsed.rds` |
| `…/geocoding/tdcj_hifld_comb.rds` | `data/intermediate/facilities/facilities_tdcj_hifld.rds` |
| `…/geocoding/combined_units.rds` | `data/intermediate/facilities/facilities_combined.rds` |
| `…/geocoding/dic_locs_cluster_id.rds` | `data/intermediate/clusters/dic_locs_cluster_id.rds` |
| `Data/Intermediate/TX/TxDOT_City_Boundaries.rds` | `data/intermediate/boundaries/txdot_city_boundaries.rds` |
| `…/geocoding/locs_w_pmuid_cluster_id_final.rds` (and the stale `locs_w_pmuid_cluster_id.rds`) | `data/final/cluster_locations.rds` |
| `Data/Intermediate/TX/dic_cluster_id_locs_final.rds` | `data/final/dic_cluster_crosswalk.rds` |
| `Data/cluster_id_temps/unit_locs_w_prism.rds` | `data/intermediate/temperature/cluster_polys_prism_crs.rds` |
| `Data/cluster_id_temps/prism/` | `data/intermediate/temperature/cluster_daily/` |
| `Data/cluster_id_temps/comb/` | `data/intermediate/temperature/cluster_combined/` |
| `Data/cluster_id_temps/prism_temporary/` | `data/raw/prism_tmp/` (temporary) |
| `Data/Raw/Climate data/temps_tx_031125.rds` | `data/final/cluster_daily_temps.rds` |
| `Data/Intermediate/Climate data/temps_tx_county.rds` | `data/intermediate/temperature/county_daily_temps.rds` |
| `Data/Final/{state,city,county}_temp_threshold.rds` | `data/final/temp_thresholds_{state,city,county}.rds` |
| `Data/Final/cluster_id_DIC_temp.rds` | `data/final/panel_cluster_day_dic_temp.rds` |
| `Data/Final/cluster_id_DIC_prison_temp.rds` | `data/final/panel_cluster_day_prison.rds` |
| `Data/Final/dic_temps_city_prison.rds` | `data/final/panel_city_day_prison.rds` |
| `Data/Final/dic_temps_county_prison.rds` (and `prison_dic_temps_by_county.rds`) | `data/final/panel_county_day_prison.rds` |
| `Data/Intermediate/TX/daily_county_dic_genpop_temp_combined.rds` | `data/final/panel_county_day_prison_genpop.rds` |
| `data/intermediate/TX/tx_county_pop_2000_2024.rds` | `data/intermediate/population/tx_county_pop_2000_2024.rds` |
| `output/tables/coefs csv/` | `output/tables/coefs_csv/` |
| `Output/Regression Results/` | `output/regression_results/` |

To reuse existing data files without rerunning the build, copy them to the new paths above.

## Changes made in the cleanup (check before release)

1. **Prison filter added (`12_join_dic_cluster_temps.R`).** No script wrote `cluster_id_DIC_prison_temp.rds`. The new step keeps clusters where any matched death has `custody == "PRISON"` and counts non-jail deaths as `num_death`, following the retired `combine_dic_temps_old.R`. Check the counts against the paper (9,387 prison deaths in `02_descriptives/04`, which also drops ages under 16; 337,763 city-day observations in the lagged binned model `lreg6` in `21`). If they differ, the old script also required `!is.na(unit_geo)`.
2. **County temperatures rebuilt (`15_extract_prism_county_temps.R`, new).** The original `temps_tx_county.rds` had no script. This one rebuilds it with TxDOT county polygons, so county names may need to be harmonized with DSHS names. Script 16 prints a message if any don't match.
3. **Gen-pop panel input (`16`).** It now reads `panel_county_day_prison.rds` instead of `prison_dic_temps_by_county.rds`, which came from an old script.
4. **TxDOT city boundaries (`08`).** The conversion from the raw .gdb is now run instead of commented out, so the .rds is reproducible.
5. **Tables 3e/3f/3g (`24`, `25`)** are now saved with `save_kable()`. Before, they were copied into Overleaf by hand.
6. **Possible bug to check (`23`).** The supplemental top-20-county model uses bins 1–7, while the full-sample supplemental model uses bins 0–8.
7. **Other cleanup:** paths are lowercase and relative to the root; `View()`/`getwd()` calls were removed; the function-test block in 23 was removed; a one-day PRISM patch loop in 09 is behind `RUN_PATCH`; and `FUN_*.R` became `code/functions/*.R` (the old `.r` vs `.R` mismatch broke on Linux).

## Archived (not used for the paper)

`code/archive/` contains: mild-climate sensitivity (05a/05b/06a/06b); county-level QP and older prison models (03e_*, 03zz, 04ee, 04ccc); AC fixed effects and sensitivity tests (05_*); COCF/COJ/CDC/population summaries (02_sum_*); maps and time-series plots (02b, 02d, 05a_map); duplicate extraction scripts (01f_extract, 01g_combine); debug/review scripts (zz*, check_model_binned); `combine_dic_temps_old.R`; and the old runner `07_run_qp_models_and_tables.R`. Several of these read files that no current script writes.

## Data availability and license

[Fill in. DSHS mortality data cannot be redistributed. Choose a code license, e.g. MIT.]
