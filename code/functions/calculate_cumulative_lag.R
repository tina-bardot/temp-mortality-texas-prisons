# code/functions/calculate_cumulative_lag.R (formerly texas/FUN_calculate_cumulative_lag.R)
## FUN_calculate_cumulative_lag.R
##
## Computes the cumulative log rate-ratio (and 95% CI) of a binary lagged
## exposure indicator from a fitted distributed-lag feglm model.
##
## Generalizes the one-off logic that was prototyped in
## 04ee_qp_cold_lag_cum.R (single model, single temperature measure) into a
## reusable function applied across all hot/cold x state/city lag models.
##
## Assumes coefficient names follow the pattern produced by
## calculate_lags() (FUN_calculate_lags.R) applied to a logical predictor and
## fit with feglm/fixest:
##   <base_var>TRUE           - the un-lagged, day-of-exposure term
##   <base_var>_lag<k>TRUE    - the lag-k term, k = 1..max_lag
## This matches how 03aa/03bb/03cc/03dd build their lag formulas (an
## un-lagged day-0 term plus lag1..lag14).
##
## Arguments:
##   model    - a fitted feglm/fixest model object
##   base_var - character; the un-lagged exposure variable name
##              (e.g. "hot_day_mean99" or "cold_day_min01")
##   max_lag  - integer; the maximum lag included in the model (default 14)
##   exposure - numeric; hypothetical exposure summed across days
##              (default 1, i.e. "exposed on every day from 0 through lag k")
##
## Returns a data.frame with one row per lag day (0..max_lag):
##   lag, log_RR, RR, lwr, upr (95% Wald CI)
##
## Note on the SE: this is the standard approach used to build a cumulative
## exposure-response curve from an unconstrained (unrestricted) distributed
## lag model, equivalent to what DLNM's crosspred() reports for a linear
## basis - the cumulative log-RR is a linear combination (partial sum) of
## the lag coefficients, so its variance is ones' %*% V %*% ones over the
## relevant coefficient sub-block.

calculate_cumulative_lag <- function(model, base_var, max_lag = 14, exposure = 1) {

  coef_names <- c(
    paste0(base_var, "TRUE"),
    paste0(base_var, "_lag", seq_len(max_lag), "TRUE")
  )

  missing_coefs <- setdiff(coef_names, names(coef(model)))
  if (length(missing_coefs) > 0) {
    stop(
      "calculate_cumulative_lag(): coefficient(s) not found in model: ",
      paste(missing_coefs, collapse = ", "),
      ". Check base_var / max_lag against the model formula."
    )
  }

  b <- coef(model)[coef_names]
  V <- vcov(model)[coef_names, coef_names]

  cum_logRR <- cumsum(b * exposure)

  cum_se <- sapply(seq_along(b), function(i) {
    idx <- 1:i
    ones <- rep(exposure, length(idx))
    sqrt(as.numeric(t(ones) %*% V[idx, idx, drop = FALSE] %*% ones))
  })

  data.frame(
    lag    = 0:max_lag,
    log_RR = as.numeric(cum_logRR),
    RR     = exp(cum_logRR),
    lwr    = exp(cum_logRR - 1.96 * cum_se),
    upr    = exp(cum_logRR + 1.96 * cum_se),
    row.names = NULL
  )
}
