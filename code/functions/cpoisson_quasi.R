# code/functions/cpoisson_quasi.R (formerly texas/FUN_cpoisson_quasi.R)
# censored quasi-poisson function 
# Peiley Lau, lau.peiley@epa.gov
# Nov 5, 2024 | Dec 12, 2024



cpoisson_quasi <- function(y, X, censor_limit, 
                           fixed_effects1 = NULL,
                           fixed_effects2 = NULL,
                           fixed_effects3 = NULL,
                           cluster = NULL) {
  # y: response variable (counts)
  # X: design matrix of predictors (including intercept)
  # censor_limit: the threshold below which observations are censored
  # fixed_effects: optional fixed effects (as a factor or categorical variable)
  # cluster: optional clustering variable for robust standard errors
  
  # Add fixed effects to the design matrix if provided
  if (!is.null(fixed_effects1)) {
    # Create dummy variables for the fixed effects
    fixed_effects_dummies1 <- model.matrix(~ fixed_effects1 - 1)  # Remove intercept
    X <- cbind(X, fixed_effects_dummies1)
    
    if(!is.null(fixed_effects2)){
      # Create dummy variables for the fixed effects
      fixed_effects_dummies2 <- model.matrix(~ fixed_effects2 - 1)  # Remove intercept
      X <- cbind(X, fixed_effects_dummies2)  # Add to the design matrix
      
      if(!is.null(fixed_effects3)){
        # Create dummy variables for the fixed effects
        fixed_effects_dummies3 <- model.matrix(~ fixed_effects3 - 1)  # Remove intercept
        X <- cbind(X, fixed_effects_dummies3)  # Add to the design matrix
      }
    }
  }
  
  # Initial values for the coefficients
  start_vals <- rep(0, ncol(X))
  
  # Quasi-Poisson log-likelihood function for censored data
  log_likelihood <- function(params, X, y, censor_limit) {
    # Linear predictor
    eta <- X %*% params
    lambda <- exp(eta)
    
    # Separate uncensored and censored observations
    uncensored <- which(y > censor_limit)
    censored <- which(y <= censor_limit)
    
    # Log-likelihood for uncensored observations (Poisson likelihood)
    log_lik_uncensored <- sum(dpois(y[uncensored], lambda[uncensored], log = TRUE))
    
    # Log-likelihood for censored observations (survival function of Poisson)
    log_lik_censored <- sum(ppois(censor_limit, lambda[censored], log.p = TRUE))
    
    # Return the total log-likelihood (negative for minimization in optim)
    return(-(log_lik_uncensored + log_lik_censored))
  }
  
  # Use optimParallel to maximize the log-likelihood function
    #set number of processors cores
    cores <-detectCores()
    cl <- makeCluster(4, outfile = '')
    setDefaultCluster(cl=cl)
    
    fit <- optimParallel(par = start_vals, fn = log_likelihood, 
                         X = X, y = y, censor_limit = censor_limit,
                         method = "BFGS", hessian = TRUE, 
                         verbose = T, control=list(trace=1))
    
    setDefaultCluster(cl=NULL); stopCluster(cl)
  
  # Extract parameter estimates
  coef_estimates <- fit$par
  
  # Calculate the fitted values
  eta_hat <- X %*% coef_estimates
  lambda_hat <- exp(eta_hat)
  
  # Calculate the Pearson residuals to estimate overdispersion (φ)
  residuals_pearson <- (y - lambda_hat) / sqrt(lambda_hat)
  dispersion_param <- sum(residuals_pearson^2) / (length(y) - ncol(X))  # φ estimate
  
  
  # Adjust standard errors for overdispersion by scaling the covariance matrix
  fisher_info <- solve(fit$hessian)
  vcov_quasi <- fisher_info * dispersion_param
  se_estimates <- sqrt(diag(vcov_quasi))
  
  # Compute robust (clustered) standard errors if cluster variable is provided
  if (!is.null(cluster)) {
    M <- length(unique(cluster))  # Number of clusters
    N <- length(y)  # Number of observations
    K <- ncol(X)  # Number of predictors
    u_hat <- residuals_pearson  # Use Pearson residuals for clustered SE
    
    # Clustered covariance matrix
    bread <- solve(fit$hessian)
    meat <- 0
    for (g in unique(cluster)) {
      X_g <- X[cluster == g, ]
      u_g <- u_hat[cluster == g]
      meat <- meat + t(X_g) %*% diag(u_g) %*% X_g
    }
    meat <- meat / N
    vcov_cluster <- bread %*% meat %*% bread * dispersion_param * M / (M - 1) * (N - 1) / (N - K)
    
    # Adjust standard errors for clustering and overdispersion
    se_estimates <- sqrt(diag(vcov_cluster))
  }
  
  # Return results as a list
  return(list(
    coefficients = coef_estimates,
    standard_errors = se_estimates,
    dispersion_param = dispersion_param,
    log_likelihood = -fit$value,
    convergence = fit$convergence
  ))
}