# ==============================================================================
# 03_analysis/11_model_cold_threshold_state.R
# (formerly texas/03c_model_qp_city_state_threshold_cold.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
# Estimating prison deaths by city and county

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

state_threshold <- readRDS("data/final/temp_thresholds_state.rds") %>%
  distinct(city, .keep_all = T)

##
# Close match data

# summary statistics
city_deaths <- dic_prison %>%
  group_by(city) %>%
  count(death_prison)

# # Limit to cities where we observe multiple deaths over time

multi_death = city_deaths %>%
  filter(death_prison == T & n > 0)

## 156 cities where there are more than 4 observations

# Limit to winter months to target the effects of cold days

###
# Part 1 : Quasi-poisson specification for state threshold
###

dic_winter <- dic_prison %>%
  left_join(state_threshold) %>%
  # filter(city %in% multi_death$city & custody == "PRISON") %>%
  # filter(city %in% multi_death$city) %>%
  # filter(year != 2020) %>%
  # filter(custody == "PRISON") %>% # not a huge difference between the restricted and less restricted versions
  # filter(month(date) %in% c(10:12, 1:3)) %>% #Limit to months with cold days
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

# dic_winter %>% select(starts_with("cold"), death_prison) %>% cor()

# Main fixed effects include city-by-year, month, and day of week fixed effects

# Spec 1, cold day defined by minimum temperature

reg1 <- feglm(
  death_prison ~ cold_day_min01 +
    tmin |
    city^year + month + dow,
  data = dic_winter,
  cluster = "city",
  family = "quasipoisson"
)

# summary(reg1)
###

reg2 <- feglm(
  death_prison ~ cold_day_mean01 +
    tmean |
    city^year + month + dow,
  data = dic_winter,
  cluster = "city",
  family = "quasipoisson"
)

# summary(reg2)

###
# No effect for max temperature, we don't see an effect

reg3 <- feglm(
  death_prison ~ cold_day_max01 +
    tmax |
    city^year + dow + month,
  data = dic_winter,
  cluster = "city",
  family = "quasipoisson"
)

# summary(reg3)

## Not significant

reg4 <- feglm(
  death_prison ~ cold_day_dp01 +
    tdmean |
    city^year + month + dow,
  data = dic_winter,
  cluster = "city",
  family = "quasipoisson"
)

# summary(reg4)

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
p01_coefs <- do.call(rbind, coefficients_list) %>%
  mutate(Estimate = exp(Estimate)) %>%
  mutate(sig = ifelse(Pr...t.. < .1, T, F))

conf_int <- list()
for (i in 1:length(city_models)) {
  conf_int[[i]] <- data.frame(exp(confint(city_models[[i]])))
}

conf_int01 <- do.call(rbind, conf_int) %>%
  bind_cols(p01_coefs) %>%
  mutate(across(1:3, ~ .x - 1)) %>%
  mutate(percentile = "1st")

# write.csv(conf_int01, "output/tables/cs_p01_reg_est.csv")

# exp(confint(city_models[[1]], 'cold_day_min01TRUE', level = .05))

# Part 2: 05th percentile

reg1 <- feglm(
  death_prison ~ cold_day_min05 +
    tmin |
    city^year + month + dow,
  data = dic_winter,
  cluster = "city",
  family = "quasipoisson"
)

# estimate mean temperature effects

reg2 <- feglm(
  death_prison ~ cold_day_mean05 +
    tmean |
    city^year + month + dow,
  data = dic_winter,
  cluster = "city",
  family = "quasipoisson"
)


# When cold day is defined by max temperature, we don't see an effect

reg3 <- feglm(
  death_prison ~ cold_day_max05 +
    tmax |
    city^year + dow + month,
  data = dic_winter,
  cluster = "city",
  family = "quasipoisson"
)

## Not significant

reg4 <- feglm(
  death_prison ~ cold_day_dp05 +
    tdmean |
    city^year + month + dow,
  data = dic_winter,
  cluster = "city",
  family = "quasipoisson"
)


city_models2 <- list(reg1, reg2, reg3, reg4)

coefficients_list <- list()
for (i in 1:length(city_models2)) {
  coefficients_list[[i]] <- data.frame(coeftable(city_models2[[i]]))
}

p05_coefs <- do.call(rbind, coefficients_list) %>%
  mutate(Estimate = exp(Estimate)) %>%
  mutate(sig = ifelse(Pr...t.. < .1, T, F))

conf_int <- list()
for (i in 1:length(city_models2)) {
  conf_int[[i]] <- data.frame(exp(confint(city_models2[[i]])))
}

conf_int05 <- do.call(rbind, conf_int) %>%
  bind_cols(p05_coefs) %>%
  mutate(across(1:3, ~ . - 1)) %>%
  mutate(percentile = "5th") %>%
  bind_rows(conf_int01)

write.csv(conf_int05, "output/tables/coefs_csv/cs_threshold_cold_est.csv")

rm(list = ls()[grep("reg|p0|conf|coef", ls())])
