# ==============================================================================
# 03_analysis/06_model_hot_threshold_city.R
# (formerly texas/03b_model_qp_city_city_threshold_hot.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
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

city_deaths <- dic_prison %>%
  group_by(city) %>%
  count(death_prison)
# Limit to cities where we observe multiple deaths over time

multi_death = city_deaths %>%
  filter(death_prison == T & n > 0)

city_threshold <- readRDS("data/final/temp_thresholds_city.rds") %>%
  distinct(city, .keep_all = T)

dic_summer <- dic_prison %>%
  left_join(city_threshold) %>%
  filter(city %in% multi_death$city) %>%
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

# Main fixed effects include city-by-year, month, and day of week fixed effects

reg1 <- feglm(
  death_prison ~ hot_day_min99 +
    tmin |
    city^year +
      month +
      dow,
  data = dic_summer,
  cluster = "city",
  family = "quasipoisson"
)

# estimate mean temperature effects

reg2 <- feglm(
  death_prison ~ hot_day_mean99 +
    tmean |
    city^year +
      month +
      dow,
  data = dic_summer,
  cluster = "city",
  family = "quasipoisson"
)


# When hot day is defined by max temperature, we don't see an effect

reg3 <- feglm(
  death_prison ~ hot_day_max99 +
    tmax |
    city^year +
      dow +
      month,
  data = dic_summer,
  cluster = "city",
  family = "quasipoisson"
)

## Not significant

reg4 <- feglm(
  death_prison ~ hot_day_dp99 +
    tdmean |
    city^year +
      month +
      dow,
  data = dic_summer,
  cluster = "city",
  family = "quasipoisson"
)

city_models <- list(
  reg1,
  reg2,
  reg3,
  reg4
)

coefficients_list <- list()
for (i in 1:length(city_models)) {
  coefficients_list[[i]] <- data.frame(coeftable(city_models[[i]]))
}
p99_coefs <- do.call(rbind, coefficients_list) %>%
  mutate(Estimate = exp(Estimate)) %>%
  mutate(sig = ifelse(Pr...t.. < .1, T, F))

conf_int <- list()
for (i in 1:length(city_models)) {
  conf_int[[i]] <- data.frame(exp(confint(city_models[[i]])))
}

conf_int99 <- do.call(rbind, conf_int) %>%
  bind_cols(p99_coefs) %>%
  mutate(across(1:3, ~ .x - 1)) %>%
  mutate(percentile = '99th')

# write.csv(conf_int99, "output/tables/cc_p99_reg_est.csv")

## 95th percentile

reg1 <- feglm(
  death_prison ~ hot_day_min95 +
    tmin |
    city^year + month + dow,
  data = dic_summer,
  cluster = "city",
  family = "quasipoisson"
)

# estimate mean temperature effects

reg2 <- feglm(
  death_prison ~ hot_day_mean95 +
    tmean |
    city^year + month + dow,
  data = dic_summer,
  cluster = "city",
  family = "quasipoisson"
)


# When hot day is defined by max temperature, we don't see an effect

reg3 <- feglm(
  death_prison ~ hot_day_max95 +
    tmax |
    city^year + dow + month,
  data = dic_summer,
  cluster = "city",
  family = "quasipoisson"
)

## Not significant

reg4 <- feglm(
  death_prison ~ hot_day_dp95 +
    tdmean |
    city^year + month + dow,
  data = dic_summer,
  cluster = "city",
  family = "quasipoisson"
)


city_models2 <- list(reg1, reg2, reg3, reg4)

coefficients_list <- list()
for (i in 1:length(city_models2)) {
  coefficients_list[[i]] <- data.frame(coeftable(city_models2[[i]]))
}

p95_coefs <- do.call(rbind, coefficients_list) %>%
  mutate(Estimate = exp(Estimate)) %>%
  mutate(sig = ifelse(Pr...t.. < .1, T, F))

conf_int <- list()
for (i in 1:length(city_models2)) {
  conf_int[[i]] <- data.frame(exp(confint(city_models2[[i]])))
}

conf_int95 <- do.call(rbind, conf_int) %>%
  bind_cols(p95_coefs) %>%
  mutate(across(1:3, ~ . - 1)) %>%
  mutate(percentile = '95th') %>%
  bind_rows(conf_int99)

write.csv(conf_int95, "output/tables/coefs_csv/cc_threshold_hot_est.csv")

rm(list = ls()[grep("reg|p9|conf|dic", ls())])
