# ==============================================================================
# 03_analysis/25_test_equality_dic_genpop.R
# (formerly texas/04c_test_equality_coeff_binned_reg_dic_and_dshs.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
#04c_test_equality_coeff_binned_reg_dic_and_dshs.R
# Memo: this script compares the binned regression coefficients
# between the DIC and DSHS 
# Peiley Lau, lau.peiley@epa.gov
# Created: 12/18/2024 | updated: 2/12/2026
####################################
#0. SET UP 
library(tidyverse)
library(rlist)
library(broom)
library(kableExtra)
library(huxtable) #for quick_latex()

#load regression results
#MAIN REGRESSIONS
#REG4_1: COUNTY + YEAR + MONTH FE + COUNTY CLUSTER
  # DSHS
  reg4_1_dshs <- list.load(file.path('output/regression_results/reg4_1_dshs.Rdata'))
  reg4_1_top20_dshs <- list.load(file.path('output/regression_results/reg4_1_top20_dshs.Rdata'))
  # DIC: model objects come from 21_model_binned_dic.R
  if (!exists("reg4_1_dic")) source('code/03_analysis/21_model_binned_dic.R', local = TRUE)
  reg4_1_dic
  reg4_1_top20_dic
  
#SUPPLEMENTAL REGRESSIONS - EXTRA TEMPERATURE BINS AT THE TAILS
  #REG4_1: COUNTY + YEAR + MONTH FE + COUNTY CLUSTER
  # DSHS
  reg4_1_dshs2 <- list.load(file.path('output/regression_results/reg4_1_dshs2.Rdata'))
  reg4_1_top20_dshs2 <- list.load(file.path('output/regression_results/reg4_1_top20_dshs2.Rdata'))
  # DIC
  reg4_1_dic2 #from 21_model_binned_dic.R
  reg4_1_top20_dic2  
####################################
# A. THE FULL SAMPLE - ALL THE COUNTIES
  #descriptive stats
  #in Full sample, there are 8609 deaths and 89 counties
###
# A1. CLEAN AND REVIEW OUTPUTS
  #coefficient names
  #change name of coefficients
  # coefficients = c('intercept',
  #                  'bin1',
  #                  'bin2',
  #                  'bin3',
  #                  'bin4',
  #                  'bin6',
  #                  'bin7'
  # )

  coefficients = c('intercept',
                   '\\leq 5C',
                   ' (5C, 10C]',
                   '(10C, 15C]',
                   '(15C, 20C]',
                   # 'bin5' = 'between 20 and 25C',
                   '(25C, 30C]',
                   '>30C'
  )
  
  
  #create results dataframe
  table <- data.frame(term = coefficients) %>%
    bind_cols(est_dshs = reg4_1_dshs$coefficients[1:7],
              se_dshs = reg4_1_dshs$standard_errors[1:7]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    filter(term != 'intercept') %>%
    mutate(est_dic = coefficients(reg4_1_dic), #add in DIC estimates
           se_dic = reg4_1_dic$se) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    mutate(est_dshs = glue::glue("{est_dshs}\n({se_dshs})"),
           est_dic = glue::glue("{est_dic}\n({se_dic})")
    ) %>%
    select(starts_with("term")| starts_with("e")) %>%
    mutate(across(everything(), ~ linebreak(.x))) 
  
  

###
#A2. TEST FOR EQUALITY OF COEFFICIENTS
  #EQUATION: (COEFF_A - COEFF_B)\SQRT(SE_A^2 + SE_B^2) = Z-VALUE
  #COMPARE Z-VALUE TO TWO SIDE Z TABLE
  table2 <- data.frame(term = coefficients) %>%
    bind_cols(est_dshs = reg4_1_dshs$coefficients[1:7],
              se_dshs = reg4_1_dshs$standard_errors[1:7]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    filter(term != 'intercept') %>%
    mutate(est_dic = coefficients(reg4_1_dic), #add in DIC estimates
           se_dic = reg4_1_dic$se) %>%
    mutate(z = (
      (est_dic - est_dshs)/sqrt(se_dshs^2 + se_dic^2)),
      p_val = 2*pnorm(-abs(z))
      ) %>%
    dplyr::select(z, p_val) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5)))
  
  table_final <- table %>%
    bind_cols(table2) 
  
  table_final
###
#A3. LATEX
  
  table_final %>%
    kable(booktabs = TRUE,
          col.names = c("", "Gen. Pop.", "DIC", "Z", "p-value"),
          align = c("l", "c", "c", "c", "c"),
          caption = "Test of equality, all counties",
          escape = FALSE,
          format = 'latex',
          linesep = "\\addlinespace",
          label = 'table3f_1') %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    row_spec(6, hline_after = TRUE) %>%
    save_kable("output/tables/Table3f_1.tex")
  
  ##this is the 1st table in "Table3f_test_of_equality_binned_regs.tex"
####################################   
#B. SUBSET TO TOP 20 COUNTIES  
  #in subset, we have 3743 deaths in 20 counties, or about 43.5% of deaths
  # in 22.5% of the lcoations
  #create results dataframe
  table_top20 <- data.frame(term = coefficients) %>%
    bind_cols(est_dshs = reg4_1_top20_dshs$coefficients[1:7],
              se_dshs = reg4_1_top20_dshs$standard_errors[1:7]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    filter(term != 'intercept') %>%
    mutate(est_dic = coefficients(reg4_1_top20_dic), #add in DIC estimates
           se_dic = reg4_1_top20_dic$se) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    mutate(est_dshs = glue::glue("{est_dshs}\n({se_dshs})"),
           est_dic = glue::glue("{est_dic}\n({se_dic})")
    ) %>%
    select(starts_with("term")| starts_with("e")) %>%
    mutate(across(everything(), ~ linebreak(.x))) 
  
  
  ###
  #B2. TEST FOR EQUALITY OF COEFFICIENTS
  #EQUATION: (COEFF_A - COEFF_B)\SQRT(SE_A^2 + SE_B^2) = Z-VALUE
  #COMPARE Z-VALUE TO TWO SIDE Z TABLE
  table2_top20 <- data.frame(term = coefficients) %>%
    bind_cols(est_dshs = reg4_1_top20_dshs$coefficients[1:7],
              se_dshs = reg4_1_top20_dshs$standard_errors[1:7]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    filter(term != 'intercept') %>%
    mutate(est_dic = coefficients(reg4_1_top20_dic), #add in DIC estimates
           se_dic = reg4_1_top20_dic$se) %>%
    mutate(z = (
      (est_dic - est_dshs)/sqrt(se_dshs^2 + se_dic^2)),
      p_val = 2*pnorm(-abs(z))
    ) %>%
    dplyr::select(z, p_val) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5)))
  
  table_final_top20 <- table_top20 %>%
    bind_cols(table2_top20) 

###
#B3. LATEX
  
  table_final_top20 %>%
    kable(booktabs = TRUE,
          col.names = c("", "Gen. Pop.", "DIC", "Z", "p-value"),
          align = c("l", "c", "c", "c", "c"),
          caption = "Test of equality, top 20 most populous counties",
          escape = FALSE,
          format = 'latex',
          linesep = "\\addlinespace",
          label = 'table3f_2') %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    row_spec(6, hline_after = TRUE) %>%
    save_kable("output/tables/Table3f_2.tex")
  
  ##this is the 2nd table in "Table3f_test_of_equality_binned_regs.tex"
    
####################################   
#C. SUPPLEMENTAL REGRESSIONS  
  coefficients2 = c('intercept',
                   '\\leq 0C',
                   ' (0C, 5C]',
                   ' (5C, 10C]',
                   '(10C, 15C]',
                   '(15C, 20C]',
                   # 'bin5' = 'between 20 and 25C',
                   '(25C, 30C]',
                   '(30C, 32C]',
                   '>32C'
  )
  
  #create results dataframe
  table2 <- data.frame(term = coefficients2) %>%
    bind_cols(est_dshs = reg4_1_dshs2$coefficients[1:9],
              se_dshs = reg4_1_dshs2$standard_errors[1:9]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    filter(term != 'intercept') %>%
    mutate(est_dic = coefficients(reg4_1_dic2), #add in DIC estimates
           se_dic = reg4_1_dic2$se) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    mutate(est_dshs = glue::glue("{est_dshs}\n({se_dshs})"),
           est_dic = glue::glue("{est_dic}\n({se_dic})")
    ) %>%
    select(starts_with("term")| starts_with("e")) %>%
    mutate(across(everything(), ~ linebreak(.x))) 
  
  
  
  ###
  #C2. TEST FOR EQUALITY OF COEFFICIENTS
  #EQUATION: (COEFF_A - COEFF_B)\SQRT(SE_A^2 + SE_B^2) = Z-VALUE
  #COMPARE Z-VALUE TO TWO SIDE Z TABLE
  table2_2 <- data.frame(term = coefficients2) %>%
    bind_cols(est_dshs = reg4_1_dshs2$coefficients[1:9],
              se_dshs = reg4_1_dshs2$standard_errors[1:9]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    filter(term != 'intercept') %>%
    mutate(est_dic = coefficients(reg4_1_dic2), #add in DIC estimates
           se_dic = reg4_1_dic2$se) %>%
    mutate(z = (
      (est_dic - est_dshs)/sqrt(se_dshs^2 + se_dic^2)),
      p_val = 2*pnorm(-abs(z))
    ) %>%
    dplyr::select(z, p_val) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5)))
  
  table_final2 <- table2 %>%
    bind_cols(table2_2) 
  
  table_final2
  ###
  #C3. LATEX
  
  table_final2 %>%
    kable(booktabs = TRUE,
          col.names = c("", "Gen. Pop.", "DIC", "Z", "p-value"),
          align = c("l", "c", "c", "c", "c"),
          caption = "Test of equality, all counties",
          escape = FALSE,
          format = 'latex',
          linesep = "\\addlinespace",
          label = 'table3g_1') %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    row_spec(8, hline_after = TRUE) %>%
    save_kable("output/tables/Table3g_1.tex")
  
  # %>%
  #   quick_latex(file = 'output/tables/Table3g_1.tex') #PL - I don't like the huxtable latex format. So just ran the code and manually c/p into Overleaf
  # 
  ####################################   
  # SUPPLEMENTAL REGRESSIONS
  #D. SUBSET TO TOP 20 COUNTIES  
  #in subset, we have 3743 deaths in 20 counties, or about 43.5% of deaths
  # in 22.5% of the lcoations
  #create results dataframe
  table_top20_2 <- data.frame(term = coefficients2) %>%
    bind_cols(est_dshs = reg4_1_top20_dshs2$coefficients[1:9],
              se_dshs = reg4_1_top20_dshs2$standard_errors[1:9]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    filter(term != 'intercept') %>%
    mutate(est_dic = coefficients(reg4_1_top20_dic2), #add in DIC estimates
           se_dic = reg4_1_top20_dic2$se) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    mutate(est_dshs = glue::glue("{est_dshs}\n({se_dshs})"),
           est_dic = glue::glue("{est_dic}\n({se_dic})")
    ) %>%
    select(starts_with("term")| starts_with("e")) %>%
    mutate(across(everything(), ~ linebreak(.x))) 
  
  
  ###
  #D2. TEST FOR EQUALITY OF COEFFICIENTS
  #EQUATION: (COEFF_A - COEFF_B)\SQRT(SE_A^2 + SE_B^2) = Z-VALUE
  #COMPARE Z-VALUE TO TWO SIDE Z TABLE
  table2_top20_2 <- data.frame(term = coefficients2) %>%
    bind_cols(est_dshs = reg4_1_top20_dshs2$coefficients[1:9],
              se_dshs = reg4_1_top20_dshs2$standard_errors[1:9]) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
    filter(term != 'intercept') %>%
    mutate(est_dic = coefficients(reg4_1_top20_dic2), #add in DIC estimates
           se_dic = reg4_1_top20_dic2$se) %>%
    mutate(z = (
      (est_dic - est_dshs)/sqrt(se_dshs^2 + se_dic^2)),
      p_val = 2*pnorm(-abs(z))
    ) %>%
    dplyr::select(z, p_val) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 5)))
  
  table_final_top20_2 <- table_top20_2 %>%
    bind_cols(table2_top20_2) 
  
  ###
  #D3. LATEX
  
  table_final_top20_2 %>%
    kable(booktabs = TRUE,
          col.names = c("", "Gen. Pop.", "DIC", "Z", "p-value"),
          align = c("l", "c", "c", "c", "c"),
          caption = "Test of equality, top 20 most populous counties",
          escape = FALSE,
          format = 'latex',
          linesep = "\\addlinespace",
          label = 'table3g_2') %>%
    kable_styling(latex_options = c("HOLD_position")) %>%
    row_spec(8, hline_after = TRUE) %>%
    save_kable("output/tables/Table3g_2.tex")
  
  # %>%
  #   quick_latex(file = 'output/tables/Table3g_2.tex') #PL - I don't like the huxtable latex format. So just ran the code and manually c/p into Overleaf
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  