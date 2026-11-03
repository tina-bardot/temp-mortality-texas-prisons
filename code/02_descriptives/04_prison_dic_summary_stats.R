# ==============================================================================
# 02_descriptives/04_prison_dic_summary_stats.R
# (formerly texas/04b_create_prison_dic_summary_stats_for_paper.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
####
# Title: 04b_create_prison_dic_summary_stats_for_paper.R
# created: 2/25/2025 | updated: 5/8/2025
# Peiley Lau, lau.peiley@epa.gov
# Memo: Construct descriptive statistics of the prison mortality observations in our paper
# 
####

####
#0. SET UP
library(tidyverse)
library(kableExtra) # create LaTeX table from df
library(modelsummary)
library(sf)
library(summarytools)
library(gt)

#load data
#intermediate data path
int_data_path <- "data/intermediate/dic"

# #unique HIFLD facilities
# units_df <- read_rds(file.path(int_data_path, 'tdcj_hifld_comb.rds')) 
# 
# #clustered HIFLD facilities + geocoded addresses
# combo_units_df <- read_rds(file.path(int_data_path, 'combined_units.rds'))

#unique DIC locations matched to HIFLD locations/clustered HIFLD locations
loc_df <- read_rds("data/final/cluster_locations.rds") %>%
  mutate(pm.uid = as.integer(pm.uid))

#DIC data w/ pm.uid
dic_pmuid_df <- read_rds("data/intermediate/dic/dic_with_pm_uid.rds")

####
#1. SUBSET AND CLEAN DATA
#PRISON ONLY LOCATIONS AND DEATHS. 
prison_pmuid <- loc_df %>%
  filter(custody == 'PRISON') %>%
  pull(pm.uid) %>%
  unique()

prison_dic_df <- dic_pmuid_df %>%
  filter(pm.uid %in% prison_pmuid) %>%
  filter(custody != 'JAIL',
         age >= 16) %>%
  mutate(death_ymd = ymd(death_ymd),
         year = year(death_ymd),
         age = as.numeric(age),
         age_bin = floor(age/10) * 10,
         date_of_birth = ymd(date_of_birth),
         days_from_custody_to_death = as.numeric(days_from_custody_to_death),
         county = str_to_title(county),
         sex = str_to_title(sex)) %>%
  left_join(loc_df %>%
              dplyr::select(pm.uid, cluster_id), 
            by = 'pm.uid') %>%
  mutate(`age_bin group` = case_when(age_bin == 10 ~ '17 - 19',
                                 age_bin == 20 ~ '20 - 29',
                                 age_bin == 30 ~ '30 - 39',
                                 age_bin == 40 ~ '40 - 49',
                                 age_bin == 50 ~ '50 - 59',
                                 age_bin == 60 ~ '60 - 69',
                                 age_bin == 70 ~ '70 - 79',
                                 age_bin == 80 ~ '80 - 89',
                                 age_bin == 90 ~ '90+',))

##################
#1. PRISON MORTALITY SUMMARY STATISTICS
#number of total deaths
dim(prison_dic_df)[1]
#9387 deaths  

#number of deaths by year
prison_dic_df %>%
  group_by(year) %>%
  summarise(num_death = n_distinct(record_id)) %>%
  ungroup()

#number of clusters by year
prison_dic_df %>%
  group_by(year) %>%
  summarise(num_cluster = n_distinct(cluster_id)) %>%
  ungroup()

#number of deaths by clusters by year
prison_dic_df %>%
  group_by(year, cluster_id) %>%
  summarise(num_death = n_distinct(record_id)) %>%
  ungroup() %>%
  left_join(loc_df %>% 
              dplyr::select(cluster_id, unit_name, city) %>%
              distinct(),
            by = 'cluster_id') %>%
  st_drop_geometry() %>%
  arrange(num_death %>% desc()) %>%
  print()

#cluster_id: 82 --> Galveston 809 HARBORSIDE DR
#cluster id: 303 --> MICHAEL STATE PRISON the colony
#cluster id: 270 --> city of the colony


##################
#2. BALANCE TABLES
# SAVED IN OVERLEAF AS TABLE1E...

#TABLE 1: overall 
datasummary(age + (`years in prison \n prior to death` = days_from_custody_to_death/365) ~
              (mean + sd)*Arguments(na.rm=TRUE),
            data = prison_dic_df,
            title = 'Statistics across the entire sample',
            output = 'latex')

#TABLE 2: by county
  #county x Statistics age + years in prison
  datasummary(county*((mean + sd)*Arguments(na.rm=TRUE)) ~ 
                  age + (`years in prison` = days_from_custody_to_death/365),
              data = prison_dic_df,
              title = 'Statistics by county',
              longtable = T,
              # booktabs = T,
              latex_options = c('striped', 'repeat_header'),
              output = 'latex') 
  #county x sex
  datasummary_crosstab(county ~ sex,
                       statistic = 1 ~ Percent('col'),
                       data = prison_dic_df)
  #county freq table
  datasummary(county + 1 ~ Percent(),
              data = prison_dic_df,
              title = 'Frequency table by county',
              output = 'latex')
  
  #county freq table for top counties
  freq(prison_dic_df$county,
       order = 'freq',
       rows = 1:10)  
# #TABLE 3: by R/E
# datasummary(race*((mean + sd)*Arguments(na.rm=TRUE)) ~ 
#               age + (`years in prison` = days_from_custody_to_death/365),
#             data = prison_dic_df)


#TABLE 3: by sex
datasummary(sex*((mean + sd)*Arguments(na.rm=TRUE)) ~
              age + (`years in prison` = days_from_custody_to_death/365),
            data = prison_dic_df,
            title = 'Statistics by sex',
            output = 'latex')
  #frequency table
  datasummary((`age group` = as.factor(`age_bin group`)) * sex + 1 ~ 
                N + Percent(),
              data = prison_dic_df,
              title = 'Frequency table by age group and sex',
              output = 'latex')

#TABLE 4: by year
datasummary((`year of death` = as.factor(year))*((mean + sd)*Arguments(na.rm=TRUE)) ~
              age + (`years in prison` = days_from_custody_to_death/365),
            data = prison_dic_df,
            title = 'Statistics by year',
            output = 'latex')


datasummary((`year of death` = as.factor(year)) + 1 ~
                N + Percent(),
                          data = prison_dic_df,
            title = 'Frequency by year',
            output = 'latex')

#TABLE 5: by cluster_id
#county freq table for top clusters
freq(prison_dic_df$cluster_id,
     order = 'freq',
     rows = 1:10)  


##################
#3. BALANCE TABLE - CAUSE OF DEATH

#cause of death
freq(prison_dic_df$medical_cause_of_death,
     order = 'freq',
     rows = 1:50)

#manner of death
datasummary((`manner of death ` = manner_of_death) ~ 1 + Percent(),
            data = prison_dic_df %>%
              mutate(manner_of_death = str_to_title(manner_of_death)))

temp_df <- prison_dic_df %>% dplyr::select(medical_cause_of_death) %>% distinct()
write_csv(temp_df, "output/tables/unique_medical_cause_of_death_dic.csv")








##################
#3. BALANCE TABLE - MONTH



















