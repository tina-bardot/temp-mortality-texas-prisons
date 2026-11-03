# ==============================================================================
# 03_analysis/09_tab_hot_lag_city.R
# (formerly texas/04bb_gen_qphot_lag_city_tables.R). Run from the repo root; see README.md for the data map.
# ==============================================================================

library(modelsummary)
library(kableExtra)
library(stringr)
library(tidyverse)

## Lagged models

## New function to make a more concise table

rename_hot_day <- function(old_names) {
  
  
  # old_names <- names(coefficients(lag_models[[1]]))
  
  new_names <- gsub("TRUE", "", old_names)
  # new_names <- gsub("hot_day_", "", new_names)
  new_names <- gsub("_min99|_mean99|_max99|_dp99", "", new_names)
  new_names <- gsub("_", " ", new_names)
  new_names <- gsub(" (lag\\d{1,2})", "_{\\1}", new_names)
  
  
  setNames(new_names, old_names)
}

modelsummary(lag_models, 
             # shape = 'cbind',
             estimate  = "{estimate}{stars}",
             statistic = "std.error",
             coef_omit = "Intercept",
             coef_rename = rename_hot_day,
             gof_omit = 'DF|Deviance|R2|AIC|BIC')

latex_table <- modelsummary(lag_models, 
                            estimate  = "{estimate}{stars}",
                            statistic = "std.error",
                            coef_omit = "Intercept",
                            coef_rename = rename_hot_day,
                            gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE',
                            output = "data.frame")

latex_table<- 
  latex_table %>%
  filter(!str_detect(term, "FE|Std")) %>%
  select(-c(1, 3)) %>%
  rename(" "= term) %>% 
  kable(format = "latex",
        caption = "Lagged model (heat)",
        booktabs = T, 
        label = "qp_hot_lag_city",
        escape =F) %>%
  kable_styling(full_width = F,
                latex_options = "scale_down") %>%
  collapse_rows(columns = 1:2, latex_hline = "none", row_group_label_position = "first")  %>%
  # add_header_above(c(" " = 1, "99th Percentile" = 4, "95th Percentile" = 4)) %>%
  footnote(general = c("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
                       "All models include city-by-year, day-of-week", 
                       "and month-of-year fixed effects.", 
                       "Standard errors are clustered at the city level.",
                       "Temperature thresholds reflect the city-level",
                       "99\\textsuperscript{th} percentile of the distribution."),
           escape=F)

save_kable(latex_table, "output/tables/qp_hot_city_city_lag_reg.tex")

modelplot(lag_models, 
          conf_level = .95,
          coef_omit = '^t',
          coef_rename = rename_hot_day,
          exponentiate = T,
          draw=F) %>%
  mutate(across(c(estimate, conf.low, conf.high),~.x-1)) %>%
  separate(term, into = c("term", "lag"), sep = "_")%>%
  mutate(lag = gsub("\\{|\\}|lag", "", lag), 
         lag = ifelse(is.na(lag), 0, lag),
         lag = as.numeric(lag)) %>%
  ggplot(aes(x = lag, 
             color= model, 
             shape=model, 
             y = estimate,
             ymin = conf.low, ymax = conf.high)) +
  ggdist::geom_pointinterval(
    size=4,
    linewidth=1,
    position = "dodge") +
  theme_minimal() +
# theme(panel.grid.minor = element_line(colour = "gray", size = 1)) +
labs(x = "Lag") +
  geom_hline(yintercept = 0) +
  facet_wrap(~ model) +
  scale_shape_manual(name = "Temperature measure",
                     values = c(15,16,17,18)) +
  scale_x_reverse(limits = c(14,0), 
                  breaks = seq(0, 14, by = 1)
  ) +
  scale_color_discrete(name = "Temperature measure") 

ggsave("output/figures/hot_city_lag_estimates.png", 
       width = 15, height = 5.5, units = "in",
       bg="white")

# Heat wave models
# modelsummary(wave_models, 
#              # shape = 'cbind',
#              estimate  = "{estimate}{stars}",
#              statistic = c("conf.int",
#                            "p = {p.value}"),
#              coef_omit = "Intercept",
#              # coef_rename = rename_hot_day,
#              gof_omit = 'DF|Deviance|R2|AIC|BIC')

latex_table <- modelsummary(wave_models, 
                            title = "Heat and cold wave model",
                            estimate  = "{estimate}{stars}",
                            statistic = "std.error",
                            coef_omit = "Intercept",
                            coef_rename = rename_hot_day,
                            gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE',
                            output = "data.frame")

latex_table<- 
  latex_table %>%
  filter(!str_detect(term, "FE|Std")) %>%
  select(-c(1, 3)) %>%
  rename(" "= term) %>% 
  kable(format = "latex",
        caption = "Heat wave model",
        booktabs = T, 
        label = "qp_hot_wave_city",
        escape =F) %>%
  kable_styling(full_width = T,
                latex_options = "scale_down") %>%
  collapse_rows(columns = 1:2, latex_hline = "none", row_group_label_position = "first")  %>%
  # add_header_above(c(" " = 1, "99th Percentile" = 4, "95th Percentile" = 4)) %>%
  footnote(general = c("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
                       "All models include city-by-year, day-of-week", 
                       "and month-of-year fixed effects.", 
                       "Standard errors are clustered at the city level.",
                       "Temperature thresholds reflect the city-level",
                       "99\\textsuperscript{th} percentile of the distribution."),
           escape=F)

save_kable(latex_table, "output/tables/qp_hot_city_city_wave_reg.tex")

modelplot(wave_models, 
          conf_level = .95,
          # coef_omit = '^t',
          coef_rename = rename_hot_day,
          exponentiate = T,
          draw=F) %>%
  mutate(across(c(estimate, conf.low, conf.high),~.x-1))%>%
  ggplot(aes(x = model, 
             color= model, 
             shape=model, 
             y = estimate,
             ymin = conf.low, ymax = conf.high)) +
  ggdist::geom_pointinterval(
    size=4,
    linewidth=1,
    position = "dodge") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 15),    # X-axis label text size
    axis.title.y = element_text(size = 15),    # Y-axis label text size
    axis.text = element_text(size = 12),       # Axis tick text size
    legend.text = element_text(size = 12),     # Legend text size
    legend.title = element_text(size = 14)     # Legend title text size
  ) + 
  geom_hline(yintercept = 0) +
  scale_shape_manual(name = "Temperature measure",
                     values = c(15,16,17,18)) +
  scale_color_discrete(name = "Temperature measure") 

ggsave("output/figures/hot_city_wave_estimates.png",
       width = 15, height = 5.5, units = "in",
       bg="white")
