# ==============================================================================
# 03_analysis/15_tab_cold_cumlag_state.R
# (formerly texas/04cc2_gen_cumulative_lag_cold_state.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
## 04cc2_gen_cumulative_lag_cold_state.R
##
## Cumulative effect of lagged cold exposure on mortality - state thresholds.
##
## Must be sourced AFTER 03cc_model_qp_city_state_lag_cold.R (needs the
## `lag_models` list still in the environment) and after
## FUN_calculate_cumulative_lag.R has been sourced.
##
## This generalizes and corrects the single-model prototype in
## 04ee_qp_cold_lag_cum.R: that script re-fit its own one-off models with a
## 0:14 lag range (folding the day-0 term into the lag sequence) instead of
## reusing 03cc's fitted objects (which follow the same day-0 + lag1:14
## convention as the hot models). This script reuses 03cc's `lag_models`
## directly, so the cold cumulative effects are now computed the same way as
## heat's, across all four temperature measures. See README for the
## recommendation to archive 04ee.

source("code/functions/calculate_cumulative_lag.R")

library(tidyverse)
library(kableExtra)

# Un-lagged (day 0) exposure variable names, matching the formulas built in
# 03cc_model_qp_city_state_lag_cold.R
cold_state_base_vars <- c(
  "minimum" = "cold_day_min01",
  "mean" = "cold_day_mean01",
  "maximum" = "cold_day_max01",
  "dew point" = "cold_day_dp01"
)

cum_cold_state <- purrr::imap_dfr(lag_models, function(mod, nm) {
  calculate_cumulative_lag(
    mod,
    base_var = cold_state_base_vars[[nm]],
    max_lag = 14
  ) %>%
    mutate(measure = nm)
})

## Figure: cumulative RR curve, faceted by temperature measure

ggplot(cum_cold_state, aes(lag, RR)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  facet_wrap(~measure) +
  labs(
    x = "Days since threshold exceedance",
    y = "Cumulative rate ratio",
    title = "Cumulative effect of cold exposure on mortality (state thresholds)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))

ggsave(
  "output/figures/cold_state_cum_estimates.png",
  width = 10,
  height = 6,
  units = "in",
  bg = "white"
)

## Table: cumulative RR at selected horizons (day 0, day 3, day 7, day 14)

cum_cold_state_summary <- cum_cold_state %>%
  filter(lag %in% c(0, 3, 7, 14)) %>%
  mutate(across(c(RR, lwr, upr), ~ round(., 3))) %>%
  select(
    Measure = measure,
    Lag_Days = lag,
    Cumulative_RR = RR,
    CI_Lower = lwr,
    CI_Upper = upr
  )

write.csv(
  cum_cold_state_summary,
  "output/tables/coefs_csv/cs_cold_cum_lag_coefs.csv",
  row.names = FALSE
)

cum_cold_state_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Cumulative rate ratio of cold exposure by day since threshold exceedance (state thresholds)",
    label = "qp_cold_cum_state",
    col.names = c(
      "Measure",
      "Lag (days)",
      "Cumulative RR",
      "95\\% CI (lower)",
      "95\\% CI (upper)"
    ),
    escape = FALSE
  ) %>%
  kable_styling(full_width = FALSE, latex_options = "scale_down") %>%
  footnote(
    general = c(
      "Cumulative RR reflects the modeled effect of a state-threshold cold day",
      "sustained from day 0 through the indicated lag.",
      "Derived from the quasi-Poisson distributed-lag models in 03cc/04cc.",
      "95% CIs via delta method on the summed lag coefficients."
    )
  ) %>%
  save_kable("output/tables/qp_cold_city_state_cum_reg.tex")

rm(list = c("cum_cold_state", "cum_cold_state_summary", "cold_state_base_vars"))
