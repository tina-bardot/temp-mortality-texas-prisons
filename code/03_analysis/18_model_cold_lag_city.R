# ==============================================================================
# 03_analysis/18_model_cold_lag_city.R
# (formerly texas/03dd_model_qp_city_city_lag_cold.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
## measuring the impact of cold days on mortality

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  readr,
  foreign,
  tidyverse,
  readxl,
  janitor,
  fixest,
  weathermetrics
)

##
# Part 1 : estimate the impact of extreme cold days on mortality
#######
rm(list = ls())

source("code/functions/calculate_lags.R")

###
# Load data
###

dic_prison <- readRDS("data/final/panel_city_day_prison.rds") %>%
  # dplyr::select(-county) %>%
  rename(death_prison = total_death_city) %>%
  mutate(
    date = ymd(date),
    month = month(date) %>% factor(),
    year = year(date) %>% factor(),
    dow = wday(date) %>% factor(),
    county = str_to_title(county),
    city_old = as.factor(city)
  ) %>%
  #create this new city x county variable to account for some cities that
  #are in two counties. want to keep the city x county combo separate.
  mutate(city = paste(city_old, county, sep = '_') %>% as.factor())

city_threshold <- readRDS("data/final/temp_thresholds_city.rds") %>%
  distinct(city, .keep_all = T)

##
# Close match data

# summary statistics

city_deaths <- dic_prison %>%
  group_by(city) %>%
  count(death_prison)

# Limit to cities where we observe multiple deaths over time

multi_death = city_deaths %>%
  filter(death_prison == T & n > 0)

## 156 cities where there are more than 4 observations

# Limit to winter months to target the effects of cold days

###
# Part 1 : Quasi-poisson specification for city threshold
###

dic_winter <- dic_prison %>%
  left_join(city_threshold) %>%
  # filter(city %in% multi_death$city & custody == "PRISON") %>%
  filter(city %in% multi_death$city) %>%
  # filter(year != 2020) %>%
  # filter(custody == "PRISON") %>% # not a huge difference between the restricted and less restricted versions
  filter(month(date) %in% c(10:12, 1:3)) %>% #Limit to months with cold days
  mutate(
    across(
      starts_with("min0"),
      .fns = ~ ifelse(tmin <= ., T, F),
      .names = "cold_day_{.col}"
    ),
    across(
      starts_with("mean0"),
      .fns = ~ ifelse(tmean <= ., T, F),
      .names = "cold_day_{.col}"
    ),
    across(
      starts_with("max0"),
      .fns = ~ ifelse(tmax <= ., T, F),
      .names = "cold_day_{.col}"
    ),
    across(
      starts_with("dp0"),
      .fns = ~ ifelse(tdmean <= ., T, F),
      .names = "cold_day_{.col}"
    )
  )

n <- 14 #max number of lags
dic_sum_cdl <- dic_winter %>%
  group_by(city, year) %>%
  arrange(date, .by_group = T) %>% # ensure that all dates are ordered for lagging
  calculate_lags(starts_with('cold'), 1:n) %>% # select all "cold day" specifications
  # calculate_lags(starts_with('t'), 1:n) %>% # select all temperature variables
  ungroup() %>%
  rowwise %>%
  mutate(
    cold_wave_min01 = ifelse(
      # construct a cold wave variable
      cold_day_min01 == T & cold_day_min01_lag1 == T & cold_day_min01_lag2 == T,
      T,
      F
    ),
    cold_wave_max01 = ifelse(
      cold_day_max01 == T & cold_day_max01_lag1 == T & cold_day_max01_lag2 == T,
      T,
      F
    ),
    cold_wave_dp01 = ifelse(
      cold_day_dp01 == T & cold_day_dp01_lag1 == T & cold_day_dp01_lag2 == T,
      T,
      F
    ),
    cold_wave_mean01 = ifelse(
      cold_day_mean01 == T &
        cold_day_mean01_lag1 == T &
        cold_day_mean01_lag2 == T,
      T,
      F
    ),
  ) %>%
  ungroup

####
min01lags <- paste0('cold_day_min01_lag', 1:14)

lag_fmla <- as.formula(paste(
  "death_prison ~ cold_day_min01 +",
  paste(min01lags, collapse = "+"),
  "| city^year + month + dow"
))

# reg1_lag  <- feglm(death_prison ~
#                      cold_day_min01 +
#                      cold_day_min01_lag1 +
#                      cold_day_min01_lag2 +
#                      cold_day_min01_lag3 +
#                      cold_day_min01_lag4 |
#                      city^year + month + dow,
#                    data = dic_sum_cdl,
#                    cluster = "city",
#                    family = "poisson")

reg1_lag <- feglm(
  lag_fmla,
  data = dic_sum_cdl,
  cluster = "city",
  family = "quasipoisson"
)

# summary(reg1_lag)

reg1_wave <- feglm(
  death_prison ~
    cold_wave_min01 |
    city^year + month + dow,
  data = dic_sum_cdl,
  cluster = "city",
  family = "poisson"
)

# summary(reg1_wave)

## Mean temperature

mean01lags <- paste0('cold_day_mean01_lag', 1:14)

lag_fmla <- as.formula(paste(
  "death_prison ~ cold_day_mean01 +",
  paste(mean01lags, collapse = "+"),
  "| city^year + month + dow"
))

# reg2_lag  <- feglm(death_prison ~
#                      cold_day_mean01 +
#                      cold_day_mean01_lag1 +
#                      cold_day_mean01_lag2 +
#                      cold_day_mean01_lag3 +
#                      cold_day_mean01_lag4 |
#                      city^year + month + dow,
#                    data = dic_sum_cdl,
#                    cluster = "city",
#                    family = "poisson")

reg2_lag <- feglm(
  lag_fmla,
  data = dic_sum_cdl,
  cluster = "city",
  family = "quasipoisson"
)

# summary(reg2_lag)

## cold wave

reg2_wave <- feglm(
  death_prison ~
    cold_wave_mean01 |
    city^year + month + dow,
  data = dic_sum_cdl,
  cluster = "city",
  family = "poisson"
)

# summary(reg2_wave)

## Tmax

max01lags <- paste0('cold_day_max01_lag', 1:14)

lag_fmla <- as.formula(paste(
  "death_prison ~ cold_day_max01 +",
  paste(max01lags, collapse = "+"),
  "| city^year + month + dow"
))

# reg3_lag  <- feglm(death_prison ~
#                      cold_day_max01 +
#                      cold_day_max01_lag1 +
#                      cold_day_max01_lag2 +
#                      cold_day_max01_lag3 +
#                      cold_day_max01_lag4 |
#                      city^year + month + dow,
#                    data = dic_sum_cdl,
#                    cluster = "city",
#                    family = "poisson")

reg3_lag <- feglm(
  lag_fmla,
  data = dic_sum_cdl,
  cluster = "city",
  family = "quasipoisson"
)
# summary(reg3_lag)

# Day of has larger effect when considering days following

reg3_wave <- feglm(
  death_prison ~
    cold_wave_max01 |
    city^year + month + dow,
  data = dic_sum_cdl,
  cluster = "city",
  family = "poisson"
)

# summary(reg3_wave)

# Dew point

dp01lags <- paste0('cold_day_dp01_lag', 1:14)

lag_fmla <- as.formula(paste(
  "death_prison ~ cold_day_dp01 +",
  paste(dp01lags, collapse = "+"),
  "| city^year + month + dow"
))

# reg4_lag  <- feglm(death_prison ~
#                      cold_day_dp01 +
#                      cold_day_dp01_lag1 +
#                      cold_day_dp01_lag2 +
#                      cold_day_dp01_lag3 +
#                      cold_day_dp01_lag4 |
#                      city^year + month + dow,
#                    data = dic_sum_cdl,
#                    cluster = "city",
#                    family = "poisson")

reg4_lag <- feglm(
  lag_fmla,
  data = dic_sum_cdl,
  cluster = "city",
  family = "quasipoisson"
)
# summary(reg4_lag)

reg4_wave <- feglm(
  death_prison ~
    cold_wave_dp01 |
    city^year + month + dow,
  data = dic_sum_cdl,
  cluster = "city",
  family = "poisson"
)

# summary(reg4_wave)

## Create list of summary models

lag_models <- list(
  "minimum" = reg1_lag,
  "mean" = reg2_lag,
  "maximum" = reg3_lag,
  "dew point" = reg4_lag
)


coefficients_list <- list()
for (i in 1:length(lag_models)) {
  coefficients_list[[i]] <- data.frame(coeftable(lag_models[[i]]))
}

lag_coefs <- do.call(rbind, coefficients_list) %>%
  mutate(Estimate = exp(Estimate)) %>%
  mutate(sig = ifelse(Pr...z.. < .1, T, F))

conf_int <- list()
for (i in 1:length(lag_models)) {
  conf_int[[i]] <- data.frame(exp(confint(lag_models[[i]])))
}

conf_int_lag <- do.call(rbind, conf_int) %>%
  bind_cols(lag_coefs) %>%
  mutate(across(1:3, ~ . - 1)) %>%
  mutate(reg = "lag")


## wave models

wave_models <- list(
  "minimum" = reg1_wave,
  "mean" = reg2_wave,
  "maximum" = reg3_wave,
  "dew point" = reg4_wave
)

coefficients_list <- list()
for (i in 1:length(wave_models)) {
  coefficients_list[[i]] <- data.frame(coeftable(wave_models[[i]]))
}

wave_coefs <- do.call(rbind, coefficients_list) %>%
  mutate(Estimate = exp(Estimate)) %>%
  mutate(sig = ifelse(Pr...z.. < .1, T, F))

conf_int <- list()
for (i in 1:length(wave_models)) {
  conf_int[[i]] <- data.frame(exp(confint(wave_models[[i]])))
}

conf_int_full <- do.call(rbind, conf_int) %>%
  bind_cols(wave_coefs) %>%
  mutate(across(1:3, ~ . - 1)) %>%
  mutate(reg = "wave") %>%
  bind_rows(conf_int_lag)

write.csv(conf_int_full, "output/tables/coefs_csv/cc_cold_wave_lag_coefs.csv")

dic_sum_cdl <- dic_sum_cdl %>%
  calculate_lags(starts_with('cold_wave'), 1:3) # select all "cold day" specifications

# reg1_wave  <- feglm(death_prison ~
#                       cold_wave_min01_lag3 |
#                       city^year + month + dow,
#                     data = dic_sum_cdl,
#                     cluster = "city",
#                     family = "poisson")
#
# summary(reg1_wave)
#
#
# reg1_wave  <- feglm(death_prison ~
#                       cold_wave_mean01_lag3 |
#                       city^year + month + dow,
#                     data = dic_sum_cdl,
#                     cluster = "city",
#                     family = "poisson")
#
# summary(reg1_wave)

rm(list = ls()[grep("reg|conf|coef|dic", ls())])
