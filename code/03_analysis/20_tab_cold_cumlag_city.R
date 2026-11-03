# ==============================================================================
# 03_analysis/20_tab_cold_cumlag_city.R
# (formerly texas/04dd2_gen_cumulative_lag_cold_city.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
## 04dd2_gen_cumulative_lag_cold_city.R
##
## Cumulative effect of lagged cold exposure on mortality - city thresholds.
##
## Must be sourced AFTER 03dd_model_qp_city_city_lag_cold.R (needs the
## `lag_models` list still in the environment) and after
## FUN_calculate_cumulative_lag.R has been sourced.

source("code/functions/calculate_cumulative_lag.R")

library(tidyverse)
library(kableExtra)

# Un-lagged (day 0) exposure variable names, matching the formulas built in
# 03dd_model_qp_city_city_lag_cold.R
cold_city_base_vars <- c(
  "minimum"   = "cold_day_min01",
  "mean"      = "cold_day_mean01",
  "maximum"   = "cold_day_max01",
  "dew point" = "cold_day_dp01"
)

cum_cold_city <- purrr::imap_dfr(lag_models, function(mod, nm) {
  calculate_cumulative_lag(mod, base_var = cold_city_base_vars[[nm]], max_lag = 14) %>%
    mutate(measure = nm)
})

## Figure: cumulative RR curve, faceted by temperature measure

ggplot(cum_cold_city, aes(lag, RR)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  facet_wrap(~ measure) +
  labs(
    x = "Days since threshold exceedance",
    y = "Cumulative rate ratio",
    title = "Cumulative effect of cold exposure on mortality (city thresholds)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))

ggsave(
  "output/figures/cold_city_cum_estimates.png",
  width = 10, height = 6, units = "in", bg = "white"
)

## Table: cumulative RR at selected horizons (day 0, day 3, day 7, day 14)

cum_cold_city_summary <- cum_cold_city %>%
  filter(lag %in% c(0, 3, 7, 14)) %>%
  mutate(across(c(RR, lwr, upr), ~ round(., 3))) %>%
  select(Measure = measure, Lag_Days = lag, Cumulative_RR = RR, CI_Lower = lwr, CI_Upper = upr)

write.csv(
  cum_cold_city_summary,
  "output/tables/coefs_csv/cc_cold_cum_lag_coefs.csv",
  row.names = FALSE
)

cum_cold_city_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Cumulative rate ratio of cold exposure by day since threshold exceedance (city thresholds)",
    label = "qp_cold_cum_city",
    col.names = c("Measure", "Lag (days)", "Cumulative RR", "95\\% CI (lower)", "95\\% CI (upper)"),
    escape = FALSE
  ) %>%
  kable_styling(full_width = FALSE, latex_options = "scale_down") %>%
  footnote(general = c(
    "Cumulative RR reflects the modeled effect of a city-threshold cold day",
    "sustained from day 0 through the indicated lag.",
    "Derived from the quasi-Poisson distributed-lag models in 03dd/04dd.",
    "95% CIs via delta method on the summed lag coefficients."
  )) %>%
  save_kable("output/tables/qp_cold_city_city_cum_reg.tex")

rm(list = c("cum_cold_city", "cum_cold_city_summary", "cold_city_base_vars"))
