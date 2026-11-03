# ==============================================================================
# 03_analysis/14_tab_cold_lag_state.R
# (formerly texas/04cc_gen_qpcold_lag_state_tables.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
library(modelsummary)
library(kableExtra)
library(stringr)
library(tidyverse)

## Lagged models

## New function to make a more concise table

rename_cold_day <- function(old_names) {
  # old_names <- names(coefficients(lag_models[[1]]))

  new_names <- gsub("TRUE", "", old_names)
  # new_names <- gsub("cold_day_", "", new_names)
  new_names <- gsub("_min01|_mean01|_max01|_dp01", "", new_names)
  new_names <- gsub("_", " ", new_names)
  new_names <- gsub(" (lag\\d{1,2})", "_{\\1}", new_names)

  setNames(new_names, old_names)
}

modelsummary(
  lag_models,
  # shape = 'cbind',
  estimate = "{estimate}{stars}",
  statistic = "std.error",
  coef_omit = "Intercept",
  coef_rename = rename_cold_day,
  gof_omit = 'DF|Deviance|R2|AIC|BIC'
)

latex_table <- modelsummary(
  lag_models,
  estimate = "{estimate}{stars}",
  statistic = "std.error",
  coef_omit = "Intercept",
  coef_rename = rename_cold_day,
  gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE',
  output = "data.frame"
)

latex_table <-
  latex_table %>%
  filter(!str_detect(term, "FE|Std")) %>%
  select(-c(1, 3)) %>%
  rename(" " = term) %>%
  kable(
    format = "latex",
    caption = "Lagged model (cold)",
    label = "qp_cold_lag_state",
    booktabs = T,
    escape = F
  ) %>%
  kable_styling(full_width = F, latex_options = "scale_down") %>%
  collapse_rows(
    columns = 1:2,
    latex_hline = "none",
    row_group_label_position = "first"
  ) %>%
  # add_header_above(c(" " = 1, "1st Percentile" = 4, "5th Percentile" = 4)) %>%
  footnote(
    general = c(
      "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
      "All models include city-by-year, day-of-week",
      "and month-of-year fixed effects.",
      "Standard errors are clustered at the city level.",
      "Temperature thresholds reflect the state-level",
      "1\\textsuperscript{st} percentile of the distribution."
    ),
    escape = F
  )


save_kable(latex_table, "output/tables/qp_cold_city_state_lag_reg.tex")

# bins <- data.frame(
#   xmin = seq(-0.5, 13.5, by = 1),
#   xmax = seq(1.5, 14.5, by = 1),
#   ymin = min(lag_models[[1]]$coefficients),
#   ymax = max(lag_models[[1]]$coefficeints)
# )

modelplot(
  lag_models,
  conf_level = .95,
  coef_omit = '^t',
  coef_rename = rename_cold_day,
  exponentiate = T,
  draw = F
) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~ .x - 1)) %>%
  separate(term, into = c("term", "lag"), sep = "_") %>%
  mutate(
    lag = gsub("\\{|\\}|lag", "", lag),
    lag = ifelse(is.na(lag), 0, lag),
    lag = as.numeric(lag)
  ) %>%
  ggplot(aes(
    x = lag,
    color = model,
    shape = model,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  ggdist::geom_pointinterval(
    size = 4,
    linewidth = 1,
    position = "dodge"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 15), # X-axis label text size
    axis.title.y = element_text(size = 15), # Y-axis label text size
    axis.text = element_text(size = 12), # Axis tick text size
    legend.text = element_text(size = 12), # Legend text size
    legend.title = element_text(size = 14) # Legend title text size
  ) +
  # theme(panel.grid.minor = element_line(colour = "gray", size = 1)) +
  labs(x = "Lag") +
  geom_hline(yintercept = 0) +
  facet_wrap(~model) +
  scale_shape_manual(name = "Temperature measure", values = c(15, 16, 17, 18)) +
  scale_x_reverse(limits = c(14, 0), breaks = seq(0, 14, by = 1)) +
  scale_color_discrete(name = "Temperature measure")

ggsave(
  "output/figures/cold_state_lag_estimates.png",
  width = 12,
  height = 5.5,
  units = "in",
  bg = "white"
)

# cold wave models
# modelsummary(wave_models,
#              # shape = 'cbind',
#              estimate  = "{estimate}{stars}",
#              statistic = c("conf.int",
#                            "p = {p.value}"),
#              coef_omit = "Intercept",
#              # coef_rename = rename_cold_day,
#              gof_omit = 'DF|Deviance|R2|AIC|BIC')

latex_table <- modelsummary(
  wave_models,
  # title = "cold wave model",
  estimate = "{estimate}{stars}",
  statistic = "std.error",
  coef_omit = "Intercept",
  coef_rename = rename_cold_day,
  gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE',
  output = "data.frame"
)

latex_table <-
  latex_table %>%
  filter(!str_detect(term, "FE|Std")) %>%
  select(-c(1, 3)) %>%
  rename(" " = term) %>%
  kable(
    format = "latex",
    caption = "cold wave model",
    label = "qp_cold_wave_state",
    booktabs = T,
    escape = F
  ) %>%
  kable_styling(full_width = T, latex_options = "scale_down") %>%
  collapse_rows(
    columns = 1:2,
    latex_hline = "none",
    row_group_label_position = "first"
  ) %>%
  # add_header_above(c(" " = 1, "1st Percentile" = 4, "5th Percentile" = 4)) %>%
  footnote(
    general = c(
      "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
      "All models include city-by-year, day-of-week",
      "and month-of-year fixed effects.",
      "Standard errors are clustered at the city level.",
      "Temperature thresholds reflect the state-level",
      "1\\textsuperscript{st} percentile of the distribution."
    ),
    escape = F
  )

save_kable(latex_table, "output/tables/qp_cold_city_state_wave_reg.tex")

modelplot(
  wave_models,
  conf_level = .95,
  # coef_omit = '^t',
  coef_rename = rename_cold_day,
  exponentiate = T,
  draw = F
) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~ .x - 1)) %>%
  ggplot(aes(
    x = model,
    color = model,
    shape = model,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  ggdist::geom_pointinterval(
    size = 4,
    linewidth = 1,
    position = "dodge"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 15), # X-axis label text size
    axis.title.y = element_text(size = 15), # Y-axis label text size
    axis.text = element_text(size = 12), # Axis tick text size
    legend.text = element_text(size = 12), # Legend text size
    legend.title = element_text(size = 14) # Legend title text size
  ) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(name = "Temperature measure", values = c(15, 16, 17, 18)) +
  scale_color_discrete(name = "Temperature measure")

ggsave(
  "output/figures/cold_state_wave_estimates.png",
  width = 15,
  height = 5.5,
  units = "in",
  bg = "white"
)
