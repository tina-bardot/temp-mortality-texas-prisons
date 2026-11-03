# ==============================================================================
# 03_analysis/02_tab_hot_threshold_state.R
# (formerly texas/04a_gen_qphot_threshold_state_tables.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
# Generate temperature mortality figures for threshold analysis

library(modelsummary)
library(kableExtra)
library(stringr)
library(tidyverse)

options(knitr.table.format = "latex")
## 

# Generate functions:

rename_hot_day <- function(old_names) {
  #TEST
  # old_names <- names(coefficients(city_models[[1]]))
  
  new_names <- gsub("dp|dmean", "dew point", old_names)
  # new_names <- gsub("hot_day_", "", new_names)
  new_names <- gsub("99|TRUE|95|TRUE", "", new_names)
  new_names <- gsub("_", " ", new_names)
  new_names <- gsub(" (min|max|mean|dew point)", "_{\\1}", new_names)
  new_names <- gsub("t(min|max|mean|dew point)", "temp_{\\1}", new_names)
  ### NEED TO FIND A WAY TO ADD } AT THE END OF THE COEFFICIENT 
  setNames(new_names, old_names)
}

# names(coefficients(city_models[[1]]))

#####################
# Part 1: Generate regression output for hot days
## Generate mortality tables for city - state threshold model

# test<-city_models

panels <- list(
  "99th Percentile" = city_models,
  "95th Percentile" = city_models2
)

# omit the statistics 
# gm <- modelsummary::gof_map
# gm$omit <- FALSE

# modelsummary(panels, 
#              shape = 'cbind',
#              estimate  = "{estimate}{stars}",
#              statistic = "std.error",
#              coef_omit = "Intercept",
#              coef_rename = rename_hot_day,
#              gof_map = 'DF|Deviance|R2|AIC|BIC|RMSE')

# latex_table <- modelsummary(panels, 
#              shape = 'cbind',
#              title = "City-level regression estimates",
#              estimate  = "{estimate}{stars}",
#              statistic = "std.error",
#              coef_omit = "Intercept",
#              coef_rename = rename_hot_day,
#              gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE',
#              output = "data.frame",
#              escape =F)
# 
# save_tt(latex_table, "output/tables/qp_hot_city_state_threshold_reg.tex", 
#          overwrite = T)

latex_table <- modelsummary(panels, 
                            shape = 'cbind',
                            # title = "City-level percentile threshold model",
                            estimate  = "{estimate}{stars}",
                            statistic = "std.error",
                            coef_omit = "Intercept",
                            coef_rename = rename_hot_day,
                            gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE',
                            stars = TRUE,
                            output = "data.frame",
                            escape =F) 

latex_table[2,2] <- "" # Hard code because there is something funky going on with collapse rows

latex_table<- 
  latex_table %>%
  filter(!str_detect(term, "FE|Std")) %>%
  dplyr::select(-c(1, 3)) %>%
  # rename_with(~gsub("99th|95th|Percentile", "", .)) %>% 
  kable(format = "latex",
        caption = "State threshold model (heat)",
        booktabs = T, 
        col.names = NULL,
        label = "qp_hot_threshold_state",
        escape =F) %>%
  kable_styling(full_width = F,
                latex_options = "scale_down") %>%
  collapse_rows(columns = 1, latex_hline = "none") %>%
  add_header_above(c(" " = 1, "99th Percentile" = 4, "95th Percentile" = 4))  %>%
  footnote(general = c("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
                       "All models include city-by-year, day-of-week and month-of-year fixed effects.", 
                       "Standard errors are clustered at the city level.")) %>%
  landscape() 

save_kable(latex_table, "output/tables/qp_hot_city_state_threshold_reg.tex")


# modelplot(c("99th Percentile"=city_models, 
#             "95th Percentile"=city_models2), conf_level = .95,
#           coef_omit = '^t',
#           coef_rename = rename_hot_day,
#           exponentiate = T
#           ) +
#   aes(color = ifelse(p.value < 0.001, "Significant", "Not significant")) +
#   scale_color_manual(values = c("grey", "black"))

rename_hot_day2 <- function(old_names) {
  #TEST
  # old_names <- names(coefficients(city_models[[1]]))
  
  new_names <- gsub("dp|dmean", "dew point", old_names)
  # new_names <- gsub("hot_day_", "", new_names)
  new_names <- gsub("hot_day_|99|95|TRUE", "", new_names)
  ### NEED TO FIND A WAY TO ADD } AT THE END OF THE COEFFICIENT 
  setNames(new_names, old_names)
}

modelplot(c("99th Percentile"=city_models, 
            "95th Percentile"=city_models2), 
          conf_level = .95,
          coef_omit = '^t',
          coef_rename = rename_hot_day2,
          exponentiate = T,
          draw=F
) %>%
  mutate(model = gsub('[1-4]',"", model)) %>%
  mutate(across(starts_with(c("est", "conf")), ~.x-1)) %>%
  ggplot(aes(x = term, y = estimate,
             ymin = conf.low, ymax = conf.high,
             shape = model,
             color = model)) +
  ggdist::geom_pointinterval(
    size=4,
    linewidth=1,
    position = "dodge") +
  theme_minimal() + 
  labs(x = "Temperature Measure")  +
  theme(
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 15),    # X-axis label text size
    axis.title.y = element_text(size = 15),    # Y-axis label text size
    axis.text = element_text(size = 12),       # Axis tick text size
    legend.text = element_text(size = 12),     # Legend text size
    legend.title = element_text(size = 14)     # Legend title text size
  ) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_color_discrete(name = "Threshold Type") + 
  scale_shape_discrete(name = "Threshold Type")

ggsave("output/figures/hot_state_reg_coef.png", 
       width = 9, height = 5.5, units = "in",
       bg="white")


