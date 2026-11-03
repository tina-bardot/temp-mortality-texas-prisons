# Data

Data files are not tracked in git. Put the raw inputs below in `data/raw/`, then run `run_all.R` from the repo root. Every intermediate and final file is rebuilt by the scripts in `code/01_build/`.

## Raw inputs (`data/raw/`)

| Path | Source | Access |
|---|---|---|
| `tji/tji_custodial-deaths_12182024.csv` | Texas Justice Initiative, custodial deaths (downloaded 2024-12-18) | Public |
| `tdcj/TDCJ Find a Facility.kml` | TDCJ "Find a Facility" map export | Public |
| `hifld/Prison_Boundaries/Prison_Boundaries.shp` | HIFLD Prison Boundaries | Public |
| `txdot/TxDOT_City_Boundaries.gdb` | TxDOT City Boundaries (layer `Cities`) | Public |
| `txdot/Texas_County_Boundaries.gdb` | TxDOT Texas County Boundaries | Public |
| `tx_county_population/` | `2021_txpopest_county.csv`, `2022_txpopest_county.csv`, `2023_txpopest_county.csv` (TX Demographic Center); `IntercensalCounty.csv`; `co-est00int-01-48.xls` (Census intercensal 2000–2010) | Public |
| `dshs/T2_TB_DM-24-0447_daily_deaths_2000-2023.xlsx` | TX DSHS daily county deaths, data request DM-24-0447 | Restricted. Not redistributable; request from DSHS |
| PRISM daily 4 km grids (`tmax`, `tmin`, `tmean`, `tdmean`) | PRISM Climate Group, downloaded with the `prism` package by scripts 09 and 15 | Public; cached temporarily in `data/raw/prism_tmp*/` and deleted |

## Final analysis files (`data/final/`)

| File | Unit | Built by | Used by |
|---|---|---|---|
| `cluster_locations.rds` | death location (sf), with `cluster_id`, matched facility, city, county | 08 | 09, 11, descriptives |
| `dic_cluster_crosswalk.rds` | death record → `cluster_id`, city, county, custody | 08 | 12 |
| `cluster_daily_temps.rds` | cluster × day: `tmax`, `tmin`, `tmean`, `tdmean` (°C) | 10 | 11, 12 |
| `temp_thresholds_{state,city,county}.rds` | 1st/5th/95th/99th percentile thresholds | 11 | QP models, descriptives |
| `panel_cluster_day_dic_temp.rds` | cluster × day, all deaths (unaggregated) | 12 | reference |
| `panel_cluster_day_prison.rds` | cluster × day, prison clusters, `num_death` | 12 | 13 |
| `panel_city_day_prison.rds` | city × day prison deaths + mean temps | 13 | all QP and binned city models |
| `panel_county_day_prison.rds` | county × day prison deaths + mean temps | 13 | 16, binned county models |
| `panel_county_day_prison_genpop.rds` | county × day, prison and general population stacked (`location`) | 16 | 23 |

The intermediate files (`data/intermediate/`) are listed in the main README.
