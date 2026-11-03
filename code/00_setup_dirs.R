# code/00_setup_dirs.R
# Creates the data/ and output/ folder structure the scripts write to.
# Safe to re-run. Run from the repo root.

dirs <- c(
  "data/raw/tji",
  "data/raw/tdcj",
  "data/raw/hifld",
  "data/raw/txdot",
  "data/raw/tx_county_population",
  "data/raw/dshs",
  "data/intermediate/dic",
  "data/intermediate/geocoding",
  "data/intermediate/facilities",
  "data/intermediate/clusters",
  "data/intermediate/boundaries",
  "data/intermediate/population",
  "data/intermediate/temperature/cluster_daily",
  "data/intermediate/temperature/cluster_combined",
  "data/intermediate/temperature/county_daily",
  "data/final",
  "output/tables/coefs_csv",
  "output/figures",
  "output/regression_results"
)

invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
