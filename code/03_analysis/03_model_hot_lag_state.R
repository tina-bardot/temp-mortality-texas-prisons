# ==============================================================================
# 03_analysis/03_model_hot_lag_state.R
# (formerly texas/03aa_model_qp_city_state_lag_hot.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
##
#################
# Part 3: Lagged temperature
# How does heat impact mortality in the days following?

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

# rm(list = ls()[sapply(mget(ls()), is.data.frame)])

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

state_threshold <- readRDS("data/final/temp_thresholds_state.rds") %>%
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

## 119 cities where there are more than 4 observations

# Limit to summer months to target the effects of hot days

###
# Part 1 : Quasi-poisson specification for state threshold
###

dic_summer <- dic_prison %>%
  left_join(state_threshold) %>%
  # filter(city %in% multi_death$city & custody == "PRISON") %>%
  filter(city %in% multi_death$city) %>%
  # filter(year != 2020) %>%
  # filter(custody == "PRISON") %>% # not a huge difference between the restricted and less restricted versions
  filter(month(date) %in% c(3:10)) %>% #Limit to months with hot days
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

#function to quickly create many lagged vars
#from: https://typethepipe.com/vizs-and-tips/how-to-create-multiple-lags-in-r/

#construct lagged vars.
n <- 14 #max number of lags
dic_sum_hdl <- dic_summer %>%
  group_by(city, year) %>%
  arrange(date, .by_group = T) %>% # ensure that all dates are ordered for lagging
  calculate_lags(starts_with('hot'), 1:n) %>% # select all "hot day" specifications
  # calculate_lags(starts_with('t'), 1:n) %>% # select all temperature variables
  ungroup() %>%
  rowwise %>%
  mutate(
    heat_wave_min99 = ifelse(
      # construct a heat wave variable
      hot_day_min99 == T & hot_day_min99_lag1 == T & hot_day_min99_lag2 == T,
      T,
      F
    ),
    heat_wave_max99 = ifelse(
      hot_day_max99 == T & hot_day_max99_lag1 == T & hot_day_max99_lag2 == T,
      T,
      F
    ),
    heat_wave_dp99 = ifelse(
      hot_day_dp99 == T & hot_day_dp99_lag1 == T & hot_day_dp99_lag2 == T,
      T,
      F
    ),
    heat_wave_mean99 = ifelse(
      hot_day_mean99 == T & hot_day_mean99_lag1 == T & hot_day_mean99_lag2 == T,
      T,
      F
    ),
  ) %>%
  ungroup

####
min99lags <- paste0('hot_day_min99_lag', 1:14)

lag_fmla <- as.formula(paste(
  "death_prison ~ hot_day_min99 +",
  paste(min99lags, collapse = "+"),
  "| city^year + month + dow"
))

# reg1_lag  <- feglm(death_prison ~
#                      hot_day_min99 +
#                      hot_day_min99_lag1 +
#                      hot_day_min99_lag2 +
#                      hot_day_min99_lag3 +
#                      hot_day_min99_lag4 |
#                      city^year + month + dow,
#                    data = dic_sum_hdl,
#                    cluster = "city",
#                    family = "poisson")

reg1_lag <- feglm(
  lag_fmla,
  data = dic_sum_hdl,
  cluster = "city",
  family = "quasipoisson"
)

# summary(reg1_lag)

reg1_wave <- feglm(
  death_prison ~
    heat_wave_min99 |
    city^year + month + dow,
  data = dic_sum_hdl,
  cluster = "city",
  family = "poisson"
)

# summary(reg1_wave)

## Mean temperature

mean99lags <- paste0('hot_day_mean99_lag', 1:14)

lag_fmla <- as.formula(paste(
  "death_prison ~ hot_day_mean99 +",
  paste(mean99lags, collapse = "+"),
  "| city^year + month + dow"
))

# reg2_lag  <- feglm(death_prison ~
#                      hot_day_mean99 +
#                      hot_day_mean99_lag1 +
#                      hot_day_mean99_lag2 +
#                      hot_day_mean99_lag3 +
#                      hot_day_mean99_lag4 |
#                      city^year + month + dow,
#                    data = dic_sum_hdl,
#                    cluster = "city",
#                    family = "poisson")

reg2_lag <- feglm(
  lag_fmla,
  data = dic_sum_hdl,
  cluster = "city",
  family = "quasipoisson"
)

# summary(reg2_lag)

## Heat wave

reg2_wave <- feglm(
  death_prison ~
    heat_wave_mean99 |
    city^year + month + dow,
  data = dic_sum_hdl,
  cluster = "city",
  family = "poisson"
)

# summary(reg2_wave)

## Tmax

max99lags <- paste0('hot_day_max99_lag', 1:14)

lag_fmla <- as.formula(paste(
  "death_prison ~ hot_day_max99 +",
  paste(max99lags, collapse = "+"),
  "| city^year + month + dow"
))

# reg3_lag  <- feglm(death_prison ~
#                      hot_day_max99 +
#                      hot_day_max99_lag1 +
#                      hot_day_max99_lag2 +
#                      hot_day_max99_lag3 +
#                      hot_day_max99_lag4 |
#                      city^year + month + dow,
#                    data = dic_sum_hdl,
#                    cluster = "city",
#                    family = "poisson")

reg3_lag <- feglm(
  lag_fmla,
  data = dic_sum_hdl,
  cluster = "city",
  family = "quasipoisson"
)
# summary(reg3_lag)

# Day of has larger effect when considering days following

reg3_wave <- feglm(
  death_prison ~
    heat_wave_max99 |
    city^year + month + dow,
  data = dic_sum_hdl,
  cluster = "city",
  family = "poisson"
)

# summary(reg3_wave)

# Dew point

dp99lags <- paste0('hot_day_dp99_lag', 1:14)

lag_fmla <- as.formula(paste(
  "death_prison ~ hot_day_dp99 +",
  paste(dp99lags, collapse = "+"),
  "| city^year + month + dow"
))

# reg4_lag  <- feglm(death_prison ~
#                      hot_day_dp99 +
#                      hot_day_dp99_lag1 +
#                      hot_day_dp99_lag2 +
#                      hot_day_dp99_lag3 +
#                      hot_day_dp99_lag4 |
#                      city^year + month + dow,
#                    data = dic_sum_hdl,
#                    cluster = "city",
#                    family = "poisson")

reg4_lag <- feglm(
  lag_fmla,
  data = dic_sum_hdl,
  cluster = "city",
  family = "quasipoisson"
)
# summary(reg4_lag)

reg4_wave <- feglm(
  death_prison ~
    heat_wave_dp99 |
    city^year + month + dow,
  data = dic_sum_hdl,
  cluster = "city",
  family = "poisson"
)

# summary(reg4_wave)

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

write.csv(conf_int_full, "output/tables/coefs_csv/cs_hot_wave_lag_coefs.csv")

rm(list = ls()[grep("reg|conf|coef|dic", ls())])
