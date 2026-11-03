# ==============================================================================
# 03_analysis/21_model_binned_dic.R
# (formerly texas/03b_estimate_model_binned_dic.R). Run from the repo root; see README.md for the data map.
# ==============================================================================
#03b_estimate_model_binned_dic.R
#Peiley Lau, lau.peiley@epa.gov
#Estimate a flexible temperature binned quasi-poisson model for DIC data
#at the city level and at the county level
#Created: Aug 2024 | Updated: Feb. 25, 2026

library(tidyverse)
library(fixest)
library(purrr)
library(sf)
library(magrittr)

############
## 1) CREATED BINNED VARIABLES
## 1A)
#CITY LEVEL
pdata <- read_rds('data/final/panel_city_day_prison.rds') %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date),
         weekday = wday(date) %>% as.factor(),
         county = str_to_title(county),
         city_old = as.factor(city)) %>%
  #create this new city x county variable to account for some cities that
  #are in two counties. want to keep the city x county combo separate. 
  mutate(city = paste(city_old, county, sep = '_') %>% as.factor()) %>%
  dplyr::select(date, month, year, weekday, city, county,
                tmin, tmax, tmean, 
                total_death_city) %>%
  mutate(bin1 = ifelse(tmean <5, 1, 0), #31F - 40F
         bin2 = ifelse(tmean >= 5 & tmean <10, 1, 0), #40 - 49F
         bin3 = ifelse(tmean >= 10 & tmean <15, 1, 0), #49 - 58F
         bin4 = ifelse(tmean >= 15 & tmean <20, 1, 0), #58 - 67F
         bin5 = ifelse(tmean >= 20 & tmean <25, 1, 0), #67 - 76F
         bin6 = ifelse(tmean >= 25 & tmean <30, 1, 0), #>76F
         bin7= ifelse(tmean >= 30, 1, 0)) 

bin_vec <- paste0('bin', setdiff(1:7, 5)) #remove bin5 to be our reference bin

#COUNTY LEVEL
#DIC data from prisons only
main_df <- read_rds('data/final/panel_county_day_prison.rds') %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date),
         weekday = wday(date) %>% as.factor(),
         county = str_to_title(county),
         county = as.factor(county)) %>%
  dplyr::select(date, month, year, weekday, county,
                tmin, tmax, tmean, 
                total_death_county) %>%
  #vector of prison counties
  mutate(bin1 = ifelse(tmean <5, 1, 0), #31F - 40F
         bin2 = ifelse(tmean >= 5 & tmean <10, 1, 0), #40 - 49F
         bin3 = ifelse(tmean >= 10 & tmean <15, 1, 0), #49 - 58F
         bin4 = ifelse(tmean >= 15 & tmean <20, 1, 0), #58 - 67F
         bin5 = ifelse(tmean >= 20 & tmean <25, 1, 0), #67 - 76F
         bin6 = ifelse(tmean >= 25 & tmean <30, 1, 0), #>76F
         bin7= ifelse(tmean >= 30, 1, 0))

  bin_vec <- paste0('bin', setdiff(1:7, 5)) #remove bin5 to be our reference bin

###
## 1B)
  ## CITY DATASET WITH MORE BINS
  #include additional bins at the tails
  pdata2 <- pdata %>%
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
  
  ## COUNTY DATASET WITH MORE BINS
  #main df w/ extra bins for the tails tails
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

########  
##1D) ADDITIONAL POPULATION DATA
#TX population
  #list of TX counties with prisons
  prison_cty_vec <- main_df %>%
    pull(county) %>%
    unique()
  
  #tx county pop data
  tx_pop <- read_rds('data/intermediate/population/tx_county_pop_2000_2024.rds') %>%
    filter(! FIPS == 509) %>%
    filter(county %in% prison_cty_vec)

############
# 2. MAIN REGRESSIONS - PRISON. CITY LEVEL
#base formula = regress # deaths on temp bins
f <- paste('total_death_city ~ ', paste(bin_vec, collapse = ' + '))

# #histogram of temp distribution in prisons data.
# pdata %>%
#   summarise_at(c('bin1',
#                  'bin2',
#                  'bin3',
#                  'bin4',
#                  'bin5',
#                  'bin6',
#                  'bin7'),
#                .funs = sum)

##
# specification 1
reg1_p <- feglm(as.formula(f), 
              data = pdata, 
              family = "quasipoisson")
summary(reg1_p)

## specification 2
#add in fixed effects
reg2_p <- feglm(as.formula(paste(f, '| city')), 
              data = pdata, 
              family = "quasipoisson")
summary(reg2_p)

## specification 3
#add in fixed effects
reg3_p <- feglm(as.formula(paste(f, '| city + as.factor(year)')), 
              data = pdata, 
              family = "quasipoisson")
summary(reg3_p)


## specification 4
#add in fixed effects: interact city and year
reg4_p <- feglm(as.formula(paste(f, '| city^as.factor(year)')), 
              data = pdata, 
              family = "quasipoisson",
              cluster = 'city')
summary(reg4_p)


## specification 5
#add in fixed effects: add in month-of-year FE (allowing for seasonal FEs)
reg5_p <- feglm(as.formula(paste(f, '| city + as.factor(month) + as.factor(year)')), 
                data = pdata, 
                family = "quasipoisson",
                cluster = 'city')
summary(reg5_p)


## specification 6
#add in fixed effects: add dow FE
reg6_p <- feglm(as.formula(paste(f, '| city + as.factor(month) + 
                                 as.factor(year) + as.factor(weekday)')), 
                data = pdata, 
                family = "quasipoisson",
                cluster = 'city')
summary(reg6_p)

## specification 7
#add in fixed effects: city x year + month FE
reg7_p <- feglm(as.formula(paste(f, '| city^as.factor(year) + as.factor(month) + 
                                 as.factor(weekday)')), 
                data = pdata, 
                family = "quasipoisson",
                cluster = 'city')
summary(reg7_p)



#########
# 2B) SUPPLEMENTAL REGRESSIONS - PRISON. CITY LEVEL
  f2 <- paste('total_death_city ~ ', paste(bin_vec2, collapse = ' + '))

  #histogram of temp distribution in prisons data.
  pdata2 %>%
    summarise_at(c('bin0',
                   'bin1',
                   'bin2',
                   'bin3',
                   'bin4',
                   'bin5',
                   'bin6',
                   'bin7',
                   'bin8'),
                 .funs = sum)
  ## specification 7
  #add in fixed effects: city x year + month FE
  reg7_p2 <- feglm(as.formula(paste(f2, '| city^as.factor(year) + as.factor(month) + 
                                   as.factor(weekday)')), 
                  data = pdata2, 
                  family = "quasipoisson",
                  cluster = 'city')
  summary(reg7_p2)

############
# 3A. MAIN REGRESSIONS - PRISON. COUNTY LEVEL
#base formula = regress # deaths on temp bins
f <- paste('total_death_county ~ ', paste(bin_vec, collapse = ' + ')) 

    # specification 1
    reg1_dic <- feglm(as.formula(f), 
                      data = main_df , 
                      family = "quasipoisson")
   
    summary(reg1_dic)
    ## specification 2
    #add in county fixed effects
    reg2_dic <- feglm(as.formula(paste(f, '| county')), 
                      data = main_df , 
                      family = "quasipoisson")
    summary(reg2_dic)
    ## specification 3
    #add in year fixed effects
    reg3_dic <- feglm(as.formula(paste(f, '| county + as.factor(year)')), 
                      data =  main_df , 
                      family = "quasipoisson")
    
    summary(reg3_dic)
    
    #cluster at county level
    reg3_1_dic <- feglm(as.formula(paste(f, '| county + as.factor(year)')), 
                        data = main_df , 
                        family = "quasipoisson",
                        cluster = 'county')
    
    summary(reg3_1_dic)
    
    ## specification 4
    # add in month fixed effects
    #cluster at county level
    reg4_1_dic <- feglm(as.formula(paste(f, '| county + as.factor(year) + as.factor(month)')), 
                        data = main_df , 
                        family = "quasipoisson",
                        cluster = 'county')
    summary(reg4_1_dic)
    
          ###filter to top 20 counties
          #vec of top N counties
          n <- 20
          topN_vec <- tx_pop %>%
            filter(year == 2024) %>%
            arrange(pop %>% desc()) %>%
            slice_head(n = n) %>%
            pull(county)
          
          reg4_1_top20_dic <- feglm(as.formula(paste(f, '| county + as.factor(year) + as.factor(month)')), 
                              data = main_df %>% 
                              filter(county %in% topN_vec), 
                              family = "quasipoisson",
                              cluster = 'county')
          summary(reg4_1_top20_dic)
          
          ###filter to bottom 20 counties
          #vec of bottom N counties
          bottomN_vec <- tx_pop %>%
            filter(year == 2024) %>%
            filter(county %in% unique(main_df$county))%>% #only keep counties in our data
            arrange(pop %>% desc()) %>%
            slice_tail(n = n) %>%
            pull(county)
          
          
          reg4_1_bottom20_dic <- feglm(as.formula(paste(f, '| county + as.factor(year) + as.factor(month)')), 
                                    data = main_df %>% 
                                      filter(county %in% bottomN_vec), 
                                    family = "quasipoisson",
                                    cluster = 'county')
          summary(reg4_1_bottom20_dic)
    ## specification 5
    #interact year and county FEs
    reg5_dic <- feglm(as.formula(paste(f, '| county^as.factor(year)')), 
                      data =  main_df , 
                      family = "quasipoisson",
                      cluster = 'county')
    summary(reg5_dic)
  
    ## specification 6
    #interact year and county FEs and add in month FE
    reg6_dic <- feglm(as.formula(paste(f, '| county^as.factor(year) + as.factor(month)')), 
                      data =  main_df , 
                      family = "quasipoisson",
                      cluster = 'county')
    summary(reg6_dic)
   
    ## specification 7
    #interact year and county FEs and add in month FE and DOW FE
    reg7_dic <- feglm(as.formula(paste(f, '| county^as.factor(year) + as.factor(month) + weekday')), 
                      data =  main_df , 
                      family = "quasipoisson",
                      cluster = 'county')
    summary(reg7_dic)
    
    
#########
# 3B) SUPPLEMENTAL REGRESSIONS - PRISON. COUNTY LEVEL
    f2 <- paste('total_death_county ~ ', paste(bin_vec2, collapse = ' + '))
    
    ## specification 4
    # add in month fixed effects
    #cluster at county level
    reg4_1_dic2 <- feglm(as.formula(paste(f2, '| county + as.factor(year) + as.factor(month)')), 
                        data = main_df2 , 
                        family = "quasipoisson",
                        cluster = 'county')
    summary(reg4_1_dic2)
    
    ###filter to top 20 counties
    #vec of top N counties
    n <- 20
    topN_vec <- tx_pop %>%
      filter(year == 2024) %>%
      arrange(pop %>% desc()) %>%
      slice_head(n = n) %>%
      pull(county)
    
    reg4_1_top20_dic2 <- feglm(as.formula(paste(f2, '| county + as.factor(year) + as.factor(month)')), 
                              data = main_df2 %>% 
                                filter(county %in% topN_vec), 
                              family = "quasipoisson",
                              cluster = 'county')
    summary(reg4_1_top20_dic2)
    
    ## specification 7
    #add in fixed effects: city x year + month FE
    reg7_dic2 <- feglm(as.formula(paste(f2, '| county^as.factor(year) + as.factor(month) + weekday')), 
                      data =  main_df2, 
                      family = "quasipoisson",
                      cluster = 'county')
    summary(reg7_dic2)
    
############
# 4. REGRESSIONS WITH LAGS.

#function to quickly create many lagged vars
#from: https://typethepipe.com/vizs-and-tips/how-to-create-multiple-lags-in-r/
calculate_lags <- function(df, var, lags){
  map_lag <- lags %>% 
    map(~partial(lag, n = .x))
  names(map_lag) <- paste0("lag", lags)

  return(df %>% 
           mutate(across(.cols = {{var}}, .fns = map_lag, .names = "{.col}_{.fn}")))
}

#construct lagged vars. 
n <- 3  #max number of lags
pdata_lag <- pdata %>%
  group_by(city) %>%
  arrange(date, .by_group = T) %>%
  calculate_lags(starts_with('bin'), 1:n) %>%
  ungroup()

lag_vec <- paste0('lag', 1:n) #vec of all the lags
#vector of the interaction terms (bins x lags)
bin_lag_vec <- expand_grid(bin_vec, lag_vec) %>%  
  mutate(., pasted=paste(bin_vec, lag_vec, sep="_")) %>%
  pull(pasted)

#base formula = regress # deaths on temp bins and lagged temp bins
f2 <- paste('total_death_city ~ ', paste(c(bin_vec, bin_lag_vec), collapse = ' + '))

##
# specification 1
lreg1 <- feglm(as.formula(f2), 
              data = pdata_lag, 
              family = "quasipoisson")
summary(lreg1)

##
# specification 2
#add in fixed effects - city
lreg2 <- feglm(as.formula(paste(f2, '| city')), 
              data = pdata_lag, 
              family = "quasipoisson")
summary(lreg2)

##
# specification 3
#add in fixed effects - city + month + year
lreg3 <- feglm(as.formula(paste(f2, '| city + as.factor(month) + as.factor(year)')), 
               data = pdata_lag, 
               family = "quasipoisson")
summary(lreg3)


##
# specification 6
#add in fixed effects + 
lreg6 <- feglm(as.formula(paste(f2, '| city^as.factor(year) + as.factor(month)')), 
               data = pdata_lag, 
               cluster = 'city',
               family = "quasipoisson")
summary(lreg6)

# GLM estimation, family = quasipoisson, Dep. Var.: total_death_city
# Observations: 337,763
# Fixed-effects: city^as.factor(year): 929,  as.factor(month): 12
# Standard-errors: Clustered (city) 
# Estimate Std. Error   t value  Pr(>|t|)    
# bin1       0.113297   0.084711  1.337455 0.1834834    
# bin2      -0.005645   0.057651 -0.097916 0.9221547    
# bin3       0.043742   0.037426  1.168739 0.2447154    
# bin4       0.009085   0.030042  0.302399 0.7628465    
# bin6       0.050866   0.056931  0.893477 0.3733054    
# bin7       0.042622   0.064696  0.658797 0.5112283    
# bin1_lag1 -0.083001   0.102523 -0.809585 0.4197050    
# bin1_lag2  0.041113   0.115682  0.355395 0.7228877    
# bin1_lag3  0.193768   0.075939  2.551634 0.0119171 *  
# bin2_lag1 -0.035262   0.068229 -0.516822 0.6061862    
# bin2_lag2  0.049958   0.053928  0.926377 0.3560211    
# bin2_lag3  0.128557   0.055057  2.334964 0.0211264 *  
# bin3_lag1 -0.076376   0.049989 -1.527867 0.1290520    
# bin3_lag2 -0.030362   0.047179 -0.643548 0.5210382    
# bin3_lag3  0.105932   0.054882  1.930179 0.0558318 .  
# bin4_lag1  0.032878   0.032597  1.008617 0.3150912    
# bin4_lag2 -0.051177   0.044639 -1.146472 0.2537724    
# bin4_lag3  0.104944   0.028951  3.624847 0.0004181 ***
# bin6_lag1 -0.040542   0.060379 -0.671461 0.5031562    
# bin6_lag2 -0.017711   0.052891 -0.334852 0.7382932    
# bin6_lag3 -0.005740   0.057789 -0.099322 0.9210406    
# bin7_lag1 -0.168483   0.083552 -2.016504 0.0458732 *  
# bin7_lag2  0.192219   0.077542  2.478913 0.0145002 *  
# bin7_lag3  0.070186   0.101573  0.690989 0.4908434    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Squared Cor.: 0.116377              
############
# 4. REGRESSIONS - LOG-LINEAR (LOG MORTALITY VS TEMP BIN)
f3 <- paste('asinh(total_death_city) ~ ', paste(bin_vec, collapse = ' + '))
  #leave out bin8 to be comparable to Deschenes WP https://docs.iza.org/dp15448.pdf
##
# specification 1
log_reg1 <- feols(as.formula(f3), 
               data = pdata_lag)
summary(log_reg1)


##
# specification 2
log_reg2 <- feols(as.formula(paste(f3, '| city')), 
                  data = pdata_lag)
summary(log_reg2)


##
# specification 3
log_reg3 <- feols(as.formula(paste(f3, '| city + as.factor(year)')), 
                  data = pdata_lag)
summary(log_reg3)



# specification 4 - interact city and year
log_reg4 <- feols(as.formula(paste(f3, '| city^as.factor(year)')), 
                  data = pdata_lag)
summary(log_reg4)

# specification 5 - city + year + month of year FEs
log_reg5 <- feols(as.formula(paste(f3, '| city + as.factor(month) + as.factor(year)')), 
                  data = pdata_lag)
summary(log_reg5)































