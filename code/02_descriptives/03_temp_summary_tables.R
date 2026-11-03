# ==============================================================================
# 02_descriptives/03_temp_summary_tables.R
# (formerly texas/02e_temp_sum_stat_tables.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  readr,
  tidyverse,
  modelsummary,
  lubridate, # Date object handling
  janitor,
  kableExtra
)

rm(list = ls())

plot_path <- "output/tables/"

# Figures for Poster presentation

temps <- c('tmax', 'tmin', 'tmean', 'tdmean')

############################
# Plot ts of county by day data
############################

dic_city_temp <- readRDS("data/final/panel_city_day_prison.rds") %>%
  mutate(across(everything(), toupper)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(across(all_of(temps), as.numeric)) # convert temperatures to numeric

city_threshold <- readRDS("data/final/temp_thresholds_city.rds")

state_threshold <- readRDS("data/final/temp_thresholds_state.rds")

# temps_summary <- summarize(city_temp, across(all_of(temps), .fns = list(
#   min = min,
#   mean = mean,
#   max = max,
#   sd = sd
# )))

p99 <- function(x) quantile(x, probs == .99, na.rm = T)
p01 <- function(x) quantile(x, probs == .01, na.rm = T)


datasummary(
  All(select(city_temp, all_of(temps))) ~ p01 + Min + Mean + Max + p99,
  data = city_temp,
  output = 'output/tables/city_temp_summary.tex'
)

##
# summarize number the percentiles

state_threshold %>%
  select(-c(1:2)) %>%
  filter(row_number() == 1) %>%
  pivot_longer(
    cols = starts_with(c("min", "mean", "max", "dp")),
    names_to = c(".value", "pctile"),
    names_sep = "(?<=[a-z])(?=[0-9])"
  ) %>%
  arrange(pctile) %>%
  rename(
    "Percentile" = 1,
    "Minimum" = 2,
    "Mean" = 3,
    "Maximum" = 4,
    "Dew Point" = 5
  ) %>%
  kbl(
    caption = "Temperature Thresholds (State)",
    format = "latex",
    align = 'c',
    booktabs = T
  ) %>%
  kable_styling(latex_options = "striped") %>%
  # add_header_above(c("Percentile" = 1,"Minimum" = 2, "Mean" = 3, "Maximum" = 4, "Dew Point" = 5 )) %>%
  save_kable("output/tables/state_temp_threshold.tex")

##
# Summarize hot days by city

dic_summer <- dic_city_temp %>%
  left_join(
    state_threshold %>% select(-county) %>% distinct(city, .keep_all = T)
  ) %>%
  # filter(city %in% multi_death$city) %>%
  # filter(custody == "PRISON") %>% # not a huge difference between the restricted and less restricted versions
  filter(
    month == "03" |
      month == "04" |
      month == "05" |
      month == "06" |
      month == "07" |
      month == "08" |
      month == "09" |
      month == "10"
  ) %>% #Limit to months with hot days
  mutate(
    across(
      starts_with("min9"),
      .fns = ~ ifelse(tmin >= ., T, F),
      .names = "hot_day_{.col}"
    ),
    across(
      starts_with("mean9"),
      .fns = ~ ifelse(tmean >= ., T, F),
      .names = "hot_day_{.col}"
    ),
    across(
      starts_with("max9"),
      .fns = ~ ifelse(tmax >= ., T, F),
      .names = "hot_day_{.col}"
    ),
    across(
      starts_with("dp9"),
      .fns = ~ ifelse(tdmean >= ., T, F),
      .names = "hot_day_{.col}"
    )
  )
