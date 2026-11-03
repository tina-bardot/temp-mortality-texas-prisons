# ==============================================================================
# 03_analysis/23_model_binned_genpop_censored.R
# (formerly texas/03c_estimate_model_binned_leftcensored_genpop.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
#03C_estimate_model_binned_leftcensored_genpop.R
#This script runs the censored quasipoisson regression on the TX 
#county mortality data 
#Peiley Lau, lau.peiley@epa.gov
#10/18/2024 | updated: 04/30/2024

##########################
#0 SET-UP
  options(scipen =  9999)
  
  #load libraries
  library(tidyverse)
  # library(censReg)  
  library(lfe)
  library(sandwich)
  library(lmtest)
  library(fixest)
  library(optimParallel)
  library(parallel)
  library(rlist) #for saving list objects

  
  #load data
  #mortality data cleaned
  main_df <- read_rds('data/final/panel_county_day_prison_genpop.rds')
  #vector of prison counties
  prison_cty_vec <- main_df %>%
    filter(location == 'PRISON') %>%
    pull(county) %>% unique()
  
  main_df <- main_df %>%
    #keep only counties that are in both prison + free living population
    filter(county %in% prison_cty_vec)%>%
    mutate(month = month(date)) %>%
    mutate(bin1 = ifelse(tmean <5, 1, 0), #31F - 40F
           bin2 = ifelse(tmean >= 5 & tmean <10, 1, 0), #40 - 49F
           bin3 = ifelse(tmean >= 10 & tmean <15, 1, 0), #49 - 58F
           bin4 = ifelse(tmean >= 15 & tmean <20, 1, 0), #58 - 67F
           bin5 = ifelse(tmean >= 20 & tmean <25, 1, 0), #67 - 76F
           bin6 = ifelse(tmean >= 25 & tmean <30, 1, 0), #>76F
           bin7= ifelse(tmean >= 30, 1, 0))
  
  #create second dataset w/ different bins at the tails
  #include additional bins at the tails
  main_df2 <- main_df %>%
    mutate(bin0 = ifelse(tmean <0, 1, 0), #31F - 40F
           bin1 = ifelse(tmean >= 0 & tmean <5, 1, 0), #31F - 40F
           bin2 = ifelse(tmean >= 5 & tmean <10, 1, 0), #40 - 49F
           bin3 = ifelse(tmean >= 10 & tmean <15, 1, 0), #49 - 58F
           bin4 = ifelse(tmean >= 15 & tmean <20, 1, 0), #58 - 67F
           bin5 = ifelse(tmean >= 20 & tmean <25, 1, 0), #67 - 76F
           bin6 = ifelse(tmean >= 25 & tmean <30, 1, 0), #>76F
           bin7 = ifelse(tmean >= 30 & tmean <32, 1, 0), #>76F
           bin8= ifelse(tmean >= 32, 1, 0)) 
  bin_vec2 <- paste0('bin', setdiff(0:8, 5)) #remove bin5 to be our reference bin
  
  #for the free living population, any counties w/ fewer than
  #10 deaths that day == NA.(censor limit = 9)
  
  #tx county pop data
  tx_pop <- read_rds('data/intermediate/population/tx_county_pop_2000_2024.rds') %>%
    filter(! FIPS == 509) %>%
    filter(county %in% prison_cty_vec)

################       
# 0. CENSORED QUASIPOISSON FUNCTION  
      #censored quasi-poisson function 
      source('code/functions/cpoisson_quasi.R')
################  
#1. CLEAN THE DATA
  # main_df <- main_df %>%
  #   left_join(tx_pop %>%
  #               mutate(year = as.numeric(year)), by = c('county', 'year'))
  
  #vec of top N counties
  n <- 20
  topN_vec <- tx_pop %>%
    filter(year == 2024) %>%
    filter(county %in% unique(main_df$county))%>% #only keep counties in our data
    arrange(pop %>% desc()) %>%
    slice_head(n = n) %>%
    pull(county)
  
  # #vec of bottom N counties
  # bottomN_vec <- tx_pop %>%
  #   filter(year == 2024) %>%
  #   filter(county %in% unique(main_df$county))%>% #only keep counties in our data
  #   arrange(pop %>% desc()) %>%
  #   slice_tail(n = n) %>%
  #   pull(county)

    
# (Function test / sanity-check block removed for the public repo; see
#  code/archive/ for the original exploratory version.)

################   
#3. RUN THE BINNED TEMPERATURE MODEL USING AVERAGE TEMPERATURE
        bin_vec <- paste0('bin', setdiff(1:7, 5)) #remove bin5 to be our reference bin
        
        #base formula = regress # deaths on temp bins
        f <- paste('total_death_county ~ ', paste(bin_vec, collapse = ' + '))  
    ##########      
    #DSHS DATA
          
        censor_limit <- 9 #any deaths that are between 0-9 are recorded as NA.
        
        freedf <- main_df %>%
          filter(location == 'free') %>%
          # filter(county %in% topN_vec) %>% #LOCAL DEBUG
          mutate(total_death_county_censor = ifelse(is.na(total_death_county),
                                                    censor_limit,
                                                    total_death_county),
                 county_year = paste(county, year, sep = '_'))
        
        #RUN ON DMAP
        # specification 1
        y <- freedf$total_death_county_censor
        X <- cbind(1, freedf$bin1, freedf$bin2, freedf$bin3, freedf$bin4, 
                   freedf$bin6, freedf$bin7)
        reg1_dshs <- cpoisson_quasi(y, X, censor_limit = 9)
        print(reg1_dshs)  
          #save regression result output
          list.save(reg1_dshs, 'output/regression_results/reg1_dshs.Rdata')
        
        #RUN ON DMAP
        # specification 2
        # add in county fixed effects 
        fixed_effects_cty <- as.factor(freedf$county)
        reg2_dshs <- cpoisson_quasi(y, X, censor_limit, 
                                    fixed_effects1 = fixed_effects_cty)
        print(reg2_dshs) 
          #save regression result output
          list.save(reg2_dshs, 'output/regression_results/reg2_dshs.Rdata')
        
        #RUN ON DMAP
        #specification 3.1
        #county FE
        #county cluster
        cluster <- freedf$county
        reg2_1_dshs <- cpoisson_quasi(y = y, X = X, 
                                      censor_limit = censor_limit, 
                                      fixed_effects1 = fixed_effects_cty,
                                      fixed_effects2 = NULL,
                                      cluster = cluster)
        print(reg2_1_dshs) 
        
        #RUN ON DMAP
        # specification 3
        # add in year fixed effects
        fixed_effects_yr <- as.factor(freedf$year)
        reg3_dshs <- cpoisson_quasi(y, X, censor_limit, 
                                    fixed_effects1 = fixed_effects_cty,
                                    fixed_effects2 = fixed_effects_yr)
        print(reg3_dshs)
        

        # specification 3.1
        # add in year fixed effects
        # county cluster
        reg3_1_dshs <- cpoisson_quasi(y, X, censor_limit, 
                                    fixed_effects1 = fixed_effects_cty,
                                    fixed_effects2 = fixed_effects_yr,
                                    cluster = cluster)
        print(reg3_1_dshs)
          #save regression result output
          list.save(reg3_1_dshs, 'output/regression_results/reg3_1_dshs.Rdata')
          
        # specification 4.1
        # add in month fixed effects
        # county cluster
          fixed_effects_month <- as.factor(freedf$month)
          reg4_1_dshs <- cpoisson_quasi(y, X, censor_limit, 
                                        fixed_effects1 = fixed_effects_cty,
                                        fixed_effects2 = fixed_effects_yr,
                                        fixed_effects3 = fixed_effects_month,
                                        cluster = cluster)
          print(reg4_1_dshs)
          #save regression result output
          list.save(reg4_1_dshs, 'output/regression_results/reg4_1_dshs.Rdata')
          
        
        

          # specification 4 - limit to top 20 most populous counties
            freedf_topN <- freedf %>%
              filter(county %in% topN_vec) 
            y <- freedf_topN$total_death_county_censor
            X <- cbind(1, freedf_topN$bin1, freedf_topN$bin2, freedf_topN$bin3, freedf_topN$bin4, 
                       freedf_topN$bin6, freedf_topN$bin7)
            cluster <- freedf_topN$county
            fixed_effects_month <- as.factor(freedf_topN$month)
            fixed_effects_cty <- as.factor(freedf_topN$county)
            fixed_effects_yr <- as.factor(freedf_topN$year)
            
            reg4_1_top20_dshs <- cpoisson_quasi(y, X, censor_limit, 
                                                fixed_effects1 = fixed_effects_cty,
                                                fixed_effects2 = fixed_effects_yr,
                                                fixed_effects3 = fixed_effects_month,
                                                cluster = cluster)
            
            #save regression result output
            list.save(reg4_1_top20_dshs, 'output/regression_results/reg4_1_top20_dshs.Rdata')
        # # specification 4
        # #interact year and county FEs 
        # fixed_effects_month <- as.factor(freedf$month)
        # fixed_effects_cty_yr <- as.factor(freedf$county_year)
        # reg4_1_dshs <- cpoisson_quasi(y, X, censor_limit, 
        #                               fixed_effects1 = fixed_effects_cty_yr,
        #                               cluster = cluster)
        # print(reg4_1_dshs)          
        # # specification 5
        # # county x year FE + month FE
        # reg5_1_dshs <- cpoisson_quasi(y, X, censor_limit, 
        #                               fixed_effects1 = fixed_effects_cty_yr,
        #                               fixed_effects2 = fixed_effects_month,
        #                               cluster = cluster)
        # print(reg5_1_dshs)

####################################
#4. RUN THE SUPPLEMENTAL REGRESSIONS WITH ADDITIONAL BINS AT THE TAILS
bin_vec2 <- paste0('bin', setdiff(0:8, 5)) 
            
#base formula = regress # deaths on temp bins
f2 <- paste('total_death_county ~ ', paste(bin_vec2, collapse = ' + '))  
         
##########      
#DSHS DATA

censor_limit <- 9 #any deaths that are between 0-9 are recorded as NA.

freedf2 <- main_df2 %>%
  filter(location == 'free') %>%
  # filter(county %in% topN_vec) %>% #LOCAL DEBUG
  mutate(total_death_county_censor = ifelse(is.na(total_death_county),
                                            censor_limit,
                                            total_death_county),
         county_year = paste(county, year, sep = '_'))

#RUN ON DMAP
# specification 1
y <- freedf2$total_death_county_censor
X <- cbind(1, freedf2$bin0, freedf2$bin1, freedf2$bin2, freedf2$bin3, freedf2$bin4, 
           freedf2$bin6, freedf2$bin7, freedf2$bin8)
reg1_dshs2 <- cpoisson_quasi(y, X, censor_limit = 9)
print(reg1_dshs2)  
#save regression result output
list.save(reg1_dshs2, 'output/regression_results/reg1_dshs2.Rdata')

#RUN ON DMAP
# specification 2
# add in county fixed effects 
fixed_effects_cty <- as.factor(freedf2$county)
reg2_dshs2 <- cpoisson_quasi(y, X, censor_limit, 
                            fixed_effects1 = fixed_effects_cty)
print(reg2_dshs2) 
#save regression result output
list.save(reg2_dshs2, 'output/regression_results/reg2_dshs2.Rdata')

# specification 3.1
# add in year fixed effects
fixed_effects_yr <- as.factor(freedf2$year)
# county cluster
cluster <- freedf2$county
reg3_1_dshs2 <- cpoisson_quasi(y, X, censor_limit, 
                              fixed_effects1 = fixed_effects_cty,
                              fixed_effects2 = fixed_effects_yr,
                              cluster = cluster)
print(reg3_1_dshs2)
#save regression result output
list.save(reg3_1_dshs2, 'output/regression_results/reg3_1_dshs2.Rdata')

# specification 4.1
# add in month fixed effects
# county cluster
fixed_effects_month <- as.factor(freedf2$month)
reg4_1_dshs2 <- cpoisson_quasi(y, X, censor_limit, 
                              fixed_effects1 = fixed_effects_cty,
                              fixed_effects2 = fixed_effects_yr,
                              fixed_effects3 = fixed_effects_month,
                              cluster = cluster)
print(reg4_1_dshs2)
#save regression result output
list.save(reg4_1_dshs2, 'output/regression_results/reg4_1_dshs2.Rdata')




# NOTE (review before release): the design matrix below uses bin1-bin7 only,
# while the full-sample supplemental model above uses bin0-bin8. Confirm
# whether bin0 and bin8 should be included here.
# specification 4 - limit to top 20 most populous counties
freedf2_topN <- freedf2 %>%
  filter(county %in% topN_vec) 
y <- freedf2_topN$total_death_county_censor
X <- cbind(1, freedf2_topN$bin1, freedf2_topN$bin2, freedf2_topN$bin3, freedf2_topN$bin4, 
           freedf2_topN$bin6, freedf2_topN$bin7)
cluster <- freedf2_topN$county
fixed_effects_month <- as.factor(freedf2_topN$month)
fixed_effects_cty <- as.factor(freedf2_topN$county)
fixed_effects_yr <- as.factor(freedf2_topN$year)

reg4_1_top20_dshs2 <- cpoisson_quasi(y, X, censor_limit, 
                                    fixed_effects1 = fixed_effects_cty,
                                    fixed_effects2 = fixed_effects_yr,
                                    fixed_effects3 = fixed_effects_month,
                                    cluster = cluster)

#save regression result output
list.save(reg4_1_top20_dshs2, 'output/regression_results/reg4_1_top20_dshs2.Rdata')            
            

            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
               
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          