# ==============================================================================
# 02_descriptives/02_state_threshold_tables.R
# (formerly texas/02_gen_state_threshold_tables.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
pacman::p_load(
  kableExtra,
  tidyverse,
  dplyr
)

state_threshold <- readRDS("data/final/temp_thresholds_state.rds") %>%
  distinct(mean95, .keep_all = T) %>%
  select(-city, -county)

temp_cols <- rep(c("minimum", "mean", "max", "dew point"), 2)

# Hot tables

state_hot <- state_threshold %>%
  select(min95, mean95, max95, dp95, min99, mean99, max99, dp99) %>%
  # select(matches("9")) %>%
  kbl(
    format = "latex",
    caption = "Hot Percentile Threshold Temperatures (State)",
    booktabs = T,
    col.names = temp_cols,
    label = "state_temp_threshold_hot"
  ) %>%
  kable_styling(full_width = F) %>%
  kableExtra::add_header_above(c("95th Percentile" = 4, "99th Percentile" = 4))

save_kable(state_hot, "output/tables/hot_state_threshold.tex")

# state_threshold %>%
#   select(min95, mean95, max95, dp95, min99, mean99, max99, dp99) %>%
#   # select(matches("9")) %>%
#   kbl(
#     caption = "Hot Percentile Threshold Temperatures (State)",
#     booktabs = T,
#     col.names = temp_cols,
#     label = "state_temp_threshold_hot"
#   ) %>%
#   kable_styling(full_width = F) %>%
#   kableExtra::add_header_above(c("95th Percentile" = 4, "99th Percentile" = 4))

## Cold tables

state_cold <- state_threshold %>%
  select(min01, mean01, max01, dp01, min05, mean05, max05, dp05) %>%
  # select(matches("9")) %>%
  kbl(
    format = "latex",
    caption = "Cold Percentile Threshold Temperatures (State)",
    booktabs = T,
    col.names = temp_cols,
    label = "state_temp_threshold_cold"
  ) %>%
  kable_styling(full_width = F) %>%
  kableExtra::add_header_above(c("1st Percentile" = 4, "5th Percentile" = 4))

save_kable(state_cold, "output/tables/cold_state_threshold.tex")

# state_threshold %>%
#   select(min01, mean01, max01, dp01, min05, mean05, max05, dp05) %>%
#   # select(matches("9")) %>%
#   kbl(
#     caption = "Cold Percentile Threshold Temperatures (State)",
#     booktabs = T,
#     col.names = temp_cols,
#     label = "state_temp_threshold_cold"
#   ) %>%
#   kable_styling(full_width = F) %>%
#   kableExtra::add_header_above(c("1st Percentile" = 4, "5th Percentile" = 4))
