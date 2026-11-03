# ==============================================================================
# 03_analysis/22_tab_binned_dic.R
# (formerly texas/04e_create_dic_prison_binned_reg_table.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
####
# Title: 04b_create_dic_prison_binned_reg_table.R
# created: 11/20/2024 | updated: 02/12/2026
# Peiley Lau, lau.peiley@epa.gov
# memo: first, run 03b_estimate_model_binned_dic.R for the reg outputs (city and county)
# this script creates the LaTeX tables from these outputs for the paper
####
#0. SET UP
library(modelsummary)
library(tidyverse)
library(huxtable)

#regression objects (reg*_p, reg*_dic, lreg*) are created in 21_model_binned_dic.R
if (!exists("reg1_p")) source('code/03_analysis/21_model_binned_dic.R', local = TRUE)

####################################
#1. REG OUTPUTS FROM PRISON DIC DATA - CITY LEVEL
    models1 <- list(
      '(1)' = reg1_p,
      '(2)' = reg2_p,
      '(3)' = reg3_p,
      # '(4)' = reg4_p,
      '(4)' = reg5_p,
      '$[$5)' = reg6_p
      # '(7)' = reg7_p
    )
    
    
    #models w/ city x year FE in sep table
    models2 <- list(
      '(1)' = reg4_p,
      '(2)' = reg7_p
    )
    
# REG OUTPUTS FROM PRISON DIC DATA - COUNTY LEVEL
    models3 <- list(
      '(1)' = reg1_dic,
      '(2)' = reg2_dic,
      '(3)' = reg3_1_dic,
      '(4)' = reg4_1_dic
    )
    
    models4 <- list(
      '(1)' = reg5_dic,
      '(2)' = reg6_dic,
      '(3)' = reg7_dic
    )
    
  # REG OUTPUTS FROM PRISON DIC DATA - CITY LEVEL WITH LAGS
    models5 <- list(
      '(1)' = lreg1,
      '(2)' = lreg2,
      '(3)' = lreg3,
      '(4)' = lreg6
    )

####################################
#2. CREATE MODEL SUMMARY TABLES - CITY DIC REGRESSIONS
#TABLE 1
msummary(models1)

#TABLE 2
msummary(models2)

#2A. FORMATTING TABLES
  #change name of coefficients
  cm = c('bin1' = '\\leq 5\\celsius',
         'bin2' = ' $[$5\\celsius, 10\\celsius)',
         'bin3' = '$[$10\\celsius, 15\\celsius)',
         'bin4' = '$[$15\\celsius, 20\\celsius)',
         # 'bin5' = 'between 20 and 25\\celsius',
         'bin6' = '$[$25\\celsius, 30\\celsius)',
         'bin7' = '\\geq 30\\celsius'
         )
  
  #change name of statistics
    #get_gof(reg1_p) to see "raw" names of these statistics.
    gm <- list(
      list('raw' = 'nobs', 'clean' = 'Sample Size', 'fmt' = 0),
      list('raw' = 'FE: as.factor(month)', 'clean' = 'Month FE', 'fmt' = 0),
      list('raw' = 'FE: city^as.factor(year)', 'clean' = 'City x year FE', 'fmt' = 0),
      list('raw' = 'FE: as.factor(weekday)', 'clean' = 'Day of week FE', 'fmt' = 0),
      list('raw' = 'FE: city', 'clean' = 'City FE', 'fmt' = 0),
      list('raw' = 'FE: as.factor(year)', 'clean' = 'Year FE', 'fmt' = 0),
      list('raw' = 'rmse', 'clean' = 'RMSE', 'fmt' = 2)
    )
    
  #add note to table
    note_table1 <- c(
      'Note: std. errors are clustered at the city level for (2)-(5).'
    )
    note_table2 <- c(
      'Note: std. errors are clustered at the city level.'
    )

#2B. CREATE TABLES
  ###
  #Table 1  
  tab1 <- msummary(models1,
           coef_map = cm,
           gof_map = gm,
           gof_omit = 'Std.Errors',
           stars = T,
           notes = note_table1,
           output = 'huxtable',
           escape = F) %>%
    set_caption('Dep. variable: mortality in Texas prisons') %>%
    set_italic(final(1), 1) %>%
    set_align(1, everywhere, 'center') %>%
    quick_latex(file = 'output/tables/binned_reg_dic_city_table1.tex')
 
  #for interpreting statistical significance/stars
    #https://cran.r-project.org/web/packages/modelsummary/modelsummary.pdf
    #• TRUE: c("+" = .1, "*" = .05, "**" = .01, "***" = 0.001)
  
  ###
  #Table 2
  #add in fixed effects with latex code
  rows2 <- tribble(~term, ~'(1)', ~'(2)',
                   'Month FE', '\\XSolidBrush', '\\Checkmark',
                   'City x year FE', '\\Checkmark','\\Checkmark',
                   'Day of week FE', '\\XSolidBrush','\\Checkmark')
  
  #use model summary to create a huxtable/gt table
  tab2 <- msummary(models2,
                   coef_map = cm,
                   gof_map = gm,
                   gof_omit = 'Std.Errors',
                   stars = T,
                   notes = note_table2,
                   output="huxtable",
                   # add_rows = rows2,
                   escape = F
  ) %>%
    set_caption('Dep. variable: mortality in Texas prisons') %>%
    set_italic(final(1), 1) %>%
    set_align(1, everywhere, 'center')

  #output a latex table file  
  tab2 %>%
    print_latex()
  
  tab2 %>%
    quick_latex(., file = 'output/tables/binned_reg_dic_city_table2.tex')

  
####################################
#3. CREATE MODEL SUMMARY TABLES - COUNTY DIC REGRESSIONS  

  #3A. FORMATTING TABLES
  #change name of statistics
  #get_gof(reg1_p) to see "raw" names of these statistics.
  gm_cty <- list(
    list('raw' = 'nobs', 'clean' = 'Sample Size', 'fmt' = 0),
    list('raw' = 'FE: as.factor(month)', 'clean' = 'Month FE', 'fmt' = 0),
    list('raw' = 'FE: county^as.factor(year)', 'clean' = 'County x year FE', 'fmt' = 0),
    list('raw' = 'FE: as.factor(weekday)', 'clean' = 'Day of week FE', 'fmt' = 0),
    list('raw' = 'FE: county', 'clean' = 'County FE', 'fmt' = 0),
    list('raw' = 'FE: as.factor(year)', 'clean' = 'Year FE', 'fmt' = 0),
    list('raw' = 'rmse', 'clean' = 'RMSE', 'fmt' = 2),
    list('raw' = 'FE: as.factor(dow)', 'clean' = 'Day of week FE', 'fmt' = 2)
  )

  #add note to table
  note_table3 <- c(
    'Note: std. errors are clustered at the county level for (3)-(4).'
  )
  note_table4 <- c(
    'Note: std. errors are clustered at the county level.'
  )
    
    
  #3B. CREATE TABLES
  ###
  #Table 3  
  tab3 <- msummary(models3,
                   coef_map = cm,
                   gof_map = gm_cty,
                   gof_omit = 'Std.Errors',
                   stars = T,
                   notes = note_table3,
                   output = 'huxtable',
                   escape = F) %>%
    set_caption('Dep. variable: mortality in Texas prisons') %>%
    set_italic(final(1), 1) %>%
    set_align(1, everywhere, 'center')
    #output a latex table file  
    tab3 %>%
      print_latex()
    
    tab3 %>%
      quick_latex(file = 'output/tables/binned_reg_dic_county_table3.tex')
    
  ###
  #Table 4
  tab4 <- msummary(models4,
                   coef_map = cm,
                   gof_map = gm_cty,
                   gof_omit = 'Std.Errors',
                   stars = T,
                   notes = note_table4,
                   output="huxtable",
                   # add_rows = rows2,
                   escape = F
  ) %>%
    set_caption('Dep. variable: mortality') %>%
    set_italic(final(1), 1) %>%
    set_align(1, everywhere, 'center')
  
  #output a latex table file  
  tab4 %>%
    print_latex()
  
  tab4 %>%
    quick_latex(., file = 'output/tables/binned_reg_dic_county_table4.tex')
  
  ###
  #Table 5
  note_table5 <- c(
    'Note: std. errors for (4) are clustered at the county level.'
  )
  
  tab5 <- msummary(models5,
                   coef_map = cm,
                   gof_map = gm_cty,
                   gof_omit = 'Std.Errors',
                   stars = T,
                   notes = note_table4,
                   output="huxtable",
                   # add_rows = rows2,
                   escape = F
  ) %>%
    set_caption('Dep. variable: mortality in Texas prisons') %>%
    set_italic(final(1), 1) %>%
    set_align(1, everywhere, 'center')
  
  #output a latex table file  
  tab5 %>%
    print_latex()
  
  tab5 %>%
    quick_latex(., file = 'output/tables/binned_reg_dic_city_withlags_table5.tex')
  
    
####################################
#5. CREATE MODEL SUMMARY TABLES - SUPPLEMENTAL REGRESSIONS W/ EXTRA BINS
  models6 <- list(
    '(1)' = reg7_p2, #CITY
    '(2)' = reg7_dic2 #COUNTY
  )
  
  #change name of coefficients
  cm_supp = c( 'bin0' = '<0\\celsius',
         'bin1' = '$[$0\\celsius,5\\celsius)',
         'bin2' = ' $[$5\\celsius, 10\\celsius)',
         'bin3' = '$[$10\\celsius, 15\\celsius)',
         'bin4' = '$[$15\\celsius, 20\\celsius)',
         # 'bin5' = 'between 20 and 25\\celsius',
         'bin6' = '$[$25\\celsius, 30\\celsius)',
         'bin7' = '$[$30\\celsius, 32\\celsius)',
         'bin8' = '\\geq 32\\celsius'
  )
    
  gm_supp <- list(
    list('raw' = 'nobs', 'clean' = 'Sample Size', 'fmt' = 0),
    list('raw' = 'FE: as.factor(month)', 'clean' = 'Month FE', 'fmt' = 0),
    list('raw' = 'FE: city^as.factor(year)', 'clean' = 'City x year FE', 'fmt' = 0),
    list('raw' = 'FE: county^as.factor(year)', 'clean' = 'County x year FE', 'fmt' = 0),
    list('raw' = 'FE: as.factor(weekday)', 'clean' = 'Day of week FE', 'fmt' = 0),
    list('raw' = 'rmse', 'clean' = 'RMSE', 'fmt' = 2),
    list('raw' = 'FE: as.factor(dow)', 'clean' = 'Day of week FE', 'fmt' = 2)
  )
  

  #add note to table
  note_table6 <- c(
    'Note: std. errors are clustered at the city level for (1) and at the county level for (2).'
  )  
    
  ###
  #Table 6
  tab6 <- msummary(models6,
                   coef_map = cm_supp,
                   gof_map = gm_supp,
                   gof_omit = 'Std.Errors',
                   stars = T,
                   notes = note_table6,
                   output="huxtable",
                   # add_rows = rows2,
                   escape = F
  ) %>%
    set_caption('Dep. variable: mortality in Texas prisons') %>%
    set_italic(final(1), 1) %>%
    set_align(1, everywhere, 'center')
  
  #output a latex table file  
  tab6 %>%
    print_latex()
  
  tab6 %>%
    quick_latex(., file = 'output/tables/binned_reg_dic_supplemental_table6.tex')  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  