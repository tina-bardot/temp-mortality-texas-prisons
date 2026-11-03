# ==============================================================================
# 03_analysis/05_tab_hot_cumlag_state.R
# (formerly texas/04aa2_gen_cumulative_lag_hot_state.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
## 04aa2_gen_cumulative_lag_hot_state.R
##
## Cumulative effect of lagged heat exposure on mortality - state thresholds.
##
## Must be sourced AFTER 03aa_model_qp_city_state_lag_hot.R (needs the
## `lag_models` list still in the environment) and after
## FUN_calculate_cumulative_lag.R has been sourced.
##
## Generalizes the single-model prototype in 04ee_qp_cold_lag_cum.R to all
## four temperature measures, using calculate_cumulative_lag().

source("code/functions/calculate_cumulative_lag.R")

library(tidyverse)
library(kableExtra)

# Un-lagged (day 0) exposure variable names, matching the formulas built in
# 03aa_model_qp_city_state_lag_hot.R
hot_state_base_vars <- c(
  "minimum"   = "hot_day_min99",
  "mean"      = "hot_day_mean99",
  "maximum"   = "hot_day_max99",
  "dew point" = "hot_day_dp99"
)

cum_hot_state <- purrr::imap_dfr(lag_models, function(mod, nm) {
  calculate_cumulative_lag(mod, base_var = hot_state_base_vars[[nm]], max_lag = 14) %>%
    mutate(measure = nm)
})

## Figure: cumulative RR curve, faceted by temperature measure

ggplot(cum_hot_state, aes(lag, RR)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  facet_wrap(~ measure) +
  labs(
    x = "Days since threshold exceedance",
    y = "Cumulative rate ratio",
    title = "Cumulative effect of heat exposure on mortality (state thresholds)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))

ggsave(
  "output/figures/hot_state_cum_estimates.png",
  width = 10, height = 6, units = "in", bg = "white"
)

## Table: cumulative RR at selected horizons (day 0, day 3, day 7, day 14)

cum_hot_state_summary <- cum_hot_state %>%
  filter(lag %in% c(0, 3, 7, 14)) %>%
  mutate(across(c(RR, lwr, upr), ~ round(., 3))) %>%
  select(Measure = measure, Lag_Days = lag, Cumulative_RR = RR, CI_Lower = lwr, CI_Upper = upr)

write.csv(
  cum_hot_state_summary,
  "output/tables/coefs_csv/cs_hot_cum_lag_coefs.csv",
  row.names = FALSE
)

cum_hot_state_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Cumulative rate ratio of heat exposure by day since threshold exceedance (state thresholds)",
    label = "qp_hot_cum_state",
    col.names = c("Measure", "Lag (days)", "Cumulative RR", "95\\% CI (lower)", "95\\% CI (upper)"),
    escape = FALSE
  ) %>%
  kable_styling(full_width = FALSE, latex_options = "scale_down") %>%
  footnote(general = c(
    "Cumulative RR reflects the modeled effect of a state-threshold hot day",
    "sustained from day 0 through the indicated lag.",
    "Derived from the quasi-Poisson distributed-lag models in 03aa/04aa.",
    "95% CIs via delta method on the summed lag coefficients."
  )) %>%
  save_kable("output/tables/qp_hot_city_state_cum_reg.tex")

rm(list = c("cum_hot_state", "cum_hot_state_summary", "hot_state_base_vars"))
