# ==============================================================================
# 03_analysis/24_tab_binned_genpop.R
# (formerly texas/04e_create_genpop_binned_reg_table.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
####
# Title: 04b_create_genpop_binned_reg_table.R
# created: 12/6/2024 | updated: 12/6/2024
# Peiley Lau, lau.peiley@epa.gov
# memo: this script creates the LaTeX results tables from regressions run in
# 03c_estimate_model_binned_leftcensored_genpop.R for the paper
####
#0. SET UP
library(tidyverse)
library(kableExtra)
library(rlist)
library(broom)

  #reg results path
  results_path <- 'output/regression_results'
  
  #function to create stars for p-value
  p_text <- function(x) {
    case_when(
      x >= 0.05 ~ "",
      x < 0.001 ~ "***",
      x < 0.01 ~ "**",
      x < 0.05 ~ "*"
    )
  }
  
  
#used code from: https://stackoverflow.com/questions/67144930/add-parentheses-and-significance-stars-to-a-table-of-estimates-and-standard-erro?
################  
#1. REG OUTPUTS - COUNTY LEVEL
    #load regression outputs
    reg1_dshs <- list.load(file.path(results_path, 'reg1_dshs.Rdata'))
    reg2_dshs <- list.load(file.path(results_path, 'reg2_dshs.Rdata'))
    reg3_1_dshs <- list.load(file.path(results_path, 'reg3_1_dshs.Rdata'))
    reg4_1_dshs <- list.load(file.path(results_path, 'reg4_1_dshs.Rdata'))
    reg4_1_top20_dshs <- list.load(file.path(results_path, 'reg4_1_top20_dshs.Rdata'))
    
################
#2. WRANGLE RESULTS INTO A DATAFRAME
  #REG1: NO FE + NO CLUSTER
  #REG2: COUNTY FE + NO CLUSTER
  #REG3_1: COUNTY + YEAR FE + COUNTY CLUSTER
  #REG4_1: COUNTY + YEAR + MONTH FE + COUNTY CLUSTER
    
    
    #coefficient names
    #change name of coefficients
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
      bind_cols(e1 = reg1_dshs$coefficients,
                sd1 = reg1_dshs$standard_errors,
                e2 = reg2_dshs$coefficients[1:7],
                sd2 = reg2_dshs$standard_errors[1:7],
                e3 = reg3_1_dshs$coefficients[1:7],
                sd3 = reg3_1_dshs$standard_errors[1:7],
                e4 = reg4_1_dshs$coefficients[1:7],
                sd4 = reg4_1_dshs$standard_errors[1:7]) %>%
     #compute p-values
       dplyr::mutate(z1 = e1/sd1,
                    p1 = round((2*(1-pnorm(abs(z1)))), 4),
                    z2 = e2/sd2,
                    p2 = round((2*(1-pnorm(abs(z2)))), 4),
                    z3 = e3/sd3,
                    p3 = round((2*(1-pnorm(abs(z3)))), 4),
                    z4 = e4/sd4,
                    p4 = round((2*(1-pnorm(abs(z4)))), 4)
      ) %>%
      mutate(across(where(is.numeric), ~ round(., digits = 5))) %>%
      filter(term != 'intercept')

    #format table
    table2 <- table %>%
      mutate(across(starts_with("p"), ~p_text(.x))) %>%
      mutate(e1 = glue::glue("{e1}{p1}\n({sd1})"),
             e2 = glue::glue("{e2}{p2}\n({sd2})"),
             e3 = glue::glue("{e3}{p3}\n({sd3})"),
             e4 = glue::glue("{e4}{p4}\n({sd4})")
      ) %>%
      select(starts_with("term")| starts_with("e")) %>%
      mutate(across(everything(), ~ linebreak(.x))) 
    
    #fixed effects df
    fe_df <- data.frame(
      'term' = c('Sample size', 'County FE', 'Year FE', 'Month FE', 'County cluster'),
      'e1' = c('380858', '\\xmark', '\\xmark', '\\xmark', '\\xmark'),
      'e2' = c('380858','\\checkmark', '\\xmark', '\\xmark', '\\xmark'),
      'e3' = c('380858','\\checkmark', '\\checkmark', '\\xmark', '\\checkmark'),
      'e4' = c('380858','\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark')
    )
    
    temp <- table2 %>%
      bind_rows(fe_df)

################  
# 3. FORMAT TABLE WITH KABLEEXTRA
    temp %>%
    kable(booktabs = TRUE,
        col.names = c("", "(1)", "(2)", "(3)", "(4)"),
        align = c("l", "c", "c", "c", "c"),
        caption = "Dep. variable: mortality in the general population",
        escape = FALSE,
        format = 'latex',
        linesep = "\\addlinespace",
        label = 'table3e') %>%
      kable_styling(latex_options = c("HOLD_position")) %>%
      row_spec(6, hline_after = TRUE) %>%
    save_kable("output/tables/Table3e.tex")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    