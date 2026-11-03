# ==============================================================================
# 03_analysis/10_tab_hot_cumlag_city.R
# (formerly texas/04bb2_gen_cumulative_lag_hot_city.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
## 04bb2_gen_cumulative_lag_hot_city.R
##
## Cumulative effect of lagged heat exposure on mortality - city thresholds.
##
## Must be sourced AFTER 03bb_model_qp_city_city_lag_hot.R (needs the
## `lag_models` list still in the environment) and after
## FUN_calculate_cumulative_lag.R has been sourced.

source("code/functions/calculate_cumulative_lag.R")

library(tidyverse)
library(kableExtra)

# Un-lagged (day 0) exposure variable names, matching the formulas built in
# 03bb_model_qp_city_city_lag_hot.R
hot_city_base_vars <- c(
  "minimum"   = "hot_day_min99",
  "mean"      = "hot_day_mean99",
  "maximum"   = "hot_day_max99",
  "dew point" = "hot_day_dp99"
)

cum_hot_city <- purrr::imap_dfr(lag_models, function(mod, nm) {
  calculate_cumulative_lag(mod, base_var = hot_city_base_vars[[nm]], max_lag = 14) %>%
    mutate(measure = nm)
})

## Figure: cumulative RR curve, faceted by temperature measure

ggplot(cum_hot_city, aes(lag, RR)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  facet_wrap(~ measure) +
  labs(
    x = "Days since threshold exceedance",
    y = "Cumulative rate ratio",
    title = "Cumulative effect of heat exposure on mortality (city thresholds)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))

ggsave(
  "output/figures/hot_city_cum_estimates.png",
  width = 10, height = 6, units = "in", bg = "white"
)

## Table: cumulative RR at selected horizons (day 0, day 3, day 7, day 14)

cum_hot_city_summary <- cum_hot_city %>%
  filter(lag %in% c(0, 3, 7, 14)) %>%
  mutate(across(c(RR, lwr, upr), ~ round(., 3))) %>%
  select(Measure = measure, Lag_Days = lag, Cumulative_RR = RR, CI_Lower = lwr, CI_Upper = upr)

write.csv(
  cum_hot_city_summary,
  "output/tables/coefs_csv/cc_hot_cum_lag_coefs.csv",
  row.names = FALSE
)

cum_hot_city_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Cumulative rate ratio of heat exposure by day since threshold exceedance (city thresholds)",
    label = "qp_hot_cum_city",
    col.names = c("Measure", "Lag (days)", "Cumulative RR", "95\\% CI (lower)", "95\\% CI (upper)"),
    escape = FALSE
  ) %>%
  kable_styling(full_width = FALSE, latex_options = "scale_down") %>%
  footnote(general = c(
    "Cumulative RR reflects the modeled effect of a city-threshold hot day",
    "sustained from day 0 through the indicated lag.",
    "Derived from the quasi-Poisson distributed-lag models in 03bb/04bb.",
    "95% CIs via delta method on the summed lag coefficients."
  )) %>%
  save_kable("output/tables/qp_hot_city_city_cum_reg.tex")

rm(list = c("cum_hot_city", "cum_hot_city_summary", "hot_city_base_vars"))
