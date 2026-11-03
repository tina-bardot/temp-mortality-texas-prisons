# ==============================================================================
# 01_build/14_clean_county_population.R
# (formerly texas/01a_clean_tx_county_population_2000_2024.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
#clean TX county pop data
#Peiley Lau, lau.peiley@epa.gov (EPA/NCEE)
#created: 10/24/2024 | updated: 03/26/2025

#0. SET UP
library(tidyverse)
library(reshape2)
library(readxl)

  #read in data
    #tx pop data path
    tx_pop_path <- 'data/raw/tx_county_population'
    tx2023 <- read.csv(file.path(tx_pop_path, '2023_txpopest_county.csv'))
    tx2022 <- read.csv(file.path(tx_pop_path, '2022_txpopest_county.csv'))
    tx2021 <- read.csv(file.path(tx_pop_path, '2021_txpopest_county.csv'))
    tx2010_2020 <- read.csv(file.path(tx_pop_path, 'IntercensalCounty.csv'))        
    tx2000_2009 <- read_xls(file.path(tx_pop_path, 'co-est00int-01-48.xls'),
                            sheet = 2)
#1. CLEAN DATASETS
    #2023 + 2024
    tx2023 <- tx2023 %>%
      dplyr::select(FIPS, county, 
                    pop_2023 = july1_2023_pop_est, 
                    pop_2024 = jan1_2024_pop_est) %>%
      pivot_longer(cols = starts_with('pop'),
                   names_sep = '_',
                   names_to = c(NA, 'year'),
                   values_to = 'pop')
    glimpse(tx2023)    
    
    #2022
    tx2022 <- tx2022 %>%
      dplyr::select(FIPS, county, 
                    pop = july1_2022_pop_est) %>%
      mutate(year = '2022')
    glimpse(tx2022)   
    
    #2021
    tx2021 <- tx2021 %>%
      dplyr::select(FIPS, county, 
                    pop = july1_2021_pop_est) %>%
      mutate(year = '2021')
    
    county_xwalk <- tx2021 %>%
      dplyr::select(FIPS, county)

    #2010-2020    
    glimpse(tx2010_2020)
    tx2010_2020 <- tx2010_2020 %>%
      dplyr::select(FIPS, 
                    pop_2010 = Census201004, 
                    pop_2011 = Estimates201107, 
                    pop_2012 = Estimates201207 , 
                    pop_2013 = Estimates201307 , 
                    pop_2014 = Estimates201407 , 
                    pop_2015 = Estimates201507 , 
                    pop_2016 = Estimates201607 , 
                    pop_2017 = Estimates201707 , 
                    pop_2018 = Estimates201807 , 
                    pop_2019 = Estimates201807 , 
                    pop_2020 = Census202004) %>%
      pivot_longer(cols = starts_with('pop'),
                   names_sep = '_',
                   names_to = c(NA, 'year'),
                   values_to = 'pop') %>%
      left_join(county_xwalk, by = 'FIPS')


#2000-2009
    glimpse(tx2000_2009)
    tx2000_2009 <- tx2000_2009 %>%
      pivot_longer(cols = starts_with('20'),
                   names_to = 'year',
                   values_to = 'pop') %>%
      left_join(county_xwalk, by = 'county')
      
    
#2. BIND YEARS TOGETHER  
    #bind 2000 - 2024 together
    tx_pop <- tx2000_2009 %>% 
      bind_rows(tx2010_2020, tx2021, tx2022, tx2023)
    
    #plot them
    tx_pop %>%
      filter(FIPS == 15) %>%
      ggplot(aes(x = year, y = pop, group = FIPS)) +
      geom_line() +
      theme_bw()

#3. SAVE DATA
    write_rds(tx_pop, 'data/intermediate/population/tx_county_pop_2000_2024.rds')
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    