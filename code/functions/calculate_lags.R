# code/functions/calculate_lags.R (formerly texas/FUN_calculate_lags.R)
calculate_lags <- function(df, var, lags){
  map_lag <- lags %>% 
    map(~partial(lag, n = .x))
  names(map_lag) <- paste0("lag", lags)
  
  return(df %>% 
           mutate(across(.cols = {{var}}, .fns = map_lag, .names = "{.col}_{.fn}")))
}