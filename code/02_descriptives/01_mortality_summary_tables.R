# ==============================================================================
# 02_descriptives/01_mortality_summary_tables.R
# (formerly texas/02a_mortality_sum_tables.R). Run from the repo root; see README.md for the data map.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  lubridate, # Date object handling
  janitor,
  kableExtra,
  modelsummary
)


## Read DIC information
dic_df <- readRDS("data/intermediate/dic/dic_clean.rds")  %>%
  mutate(death_ymd = ymd(death_ymd),
         year = year(death_ymd),
         age = as.numeric(age),
         days_from_custody_to_death = as.numeric(days_from_custody_to_death)) %>%
  mutate(across(c(sex, custody), str_to_title))

# Rename variables
dems <- dic_df %>%
  mutate(age = as.numeric(age)) %>%
  # mutate(race = ifelse(race=="", "Other", race)) %>%
  select(sex, age, days_from_custody_to_death) %>% 
  group_by(sex) %>%
  summarize(mean_age = round(mean(age, na.rm=T)),
            avg_length_stay = round(mean(days_from_custody_to_death/365, na.rm = T), 1),
            obs = n()) 

names(dems) <- c("Sex", "Av. Age", "Years in Custody", "Num. Obs")

dems %>% ungroup %>% 
  select(-1) %>%
  kbl(format = 'latex',
      caption = "Death in Custody statistics by sex",
      align = 'c',
      booktabs = T) %>%
  kable_styling(latex_options = 'striped') %>%
  save_kable("output/tables/sex_age_sum.tex") 


## Average age at death

years <- dic_df %>%
  group_by(year) %>%
  summarize(num_deaths = n()) %>%
  mutate(year = ifelse(year == 2024, "2024*", year)) %>% ungroup

names(years) <- c("Year", "# of Deaths")

years  %>% 
  kbl(format = 'latex',
      caption = "Death count by year",
      align = 'c',
      booktabs = T)  %>%
  footnote(symbol = "Jan to Aug 2024",
           footnote_as_chunk= T) %>%
  kable_styling(latex_options = 'striped',
                full_width = FALSE) %>%
  save_kable("output/tables/deaths_by_year.tex") 
