# Project: GLM Simulation Study
# Script: sim_functions.R
# Author: Mattis Min
# Date: 2025-12-09
# Purpose: Functions used for the GLM simulation study final project.
#          This file only contains function definitions
#          Functions here will be sourced and used in sim_run.R.
# R Version: 2025.05.1+513
# Package Version: sandwich 3.1-1

library(sandwich)


simulate_one <- function(N, lambda, x_vec) {
  # N: sample size (20 or 200)
  # lambda: true Poisson mean (4 or 10)
  # x_vec: binary predictor vector of length N (0/1 values)
  # Returns: a data frame with 2 rows (Poisson, Normal) and
  # summary measures for this one simulated dataset.

  # 1) Simulate outcome Y from Poisson --------------------------------------
  # Y is independent of x in this setup (true beta1 = 0)
  y <- rpois(n = N, lambda = lambda)

  # 2) Put into a data frame for glm() --------------------------------------
  dat <- data.frame(
    y = y,
    x = x_vec
  )

  # 3) Fit Poisson and Normal GLMs ------------------------------------------
  fit_pois <- glm(y ~ x,
    family = poisson, data = dat
  )
  fit_norm <- glm(y ~ x,
    family = gaussian, data = dat
  )
  # 4) Predicted means (lambda-hat) for each model --------------------------
  newdata <- data.frame(x = x_vec)
  pois_lambda_hat <- predict(fit_pois,
    newdata = newdata, type = "response"
  )
  norm_lambda_hat <- predict(fit_norm,
    newdata = newdata, type = "response"
  )
  # Average predicted lambda for this dataset
  pois_lambda_hat_avg <- mean(pois_lambda_hat)
  norm_lambda_hat_avg <- mean(norm_lambda_hat)
  # Bias of lambda-hat for this dataset
  bias_pois_lambda <- pois_lambda_hat_avg - lambda
  bias_norm_lambda <- norm_lambda_hat_avg - lambda

  # 5) Extract beta1 and its variances --------------------------------------
  # beta1 is the coefficient for x
  beta1_pois <- coef(fit_pois)[2]
  beta1_norm <- coef(fit_norm)[2]
  nvar_pois <- vcov(fit_pois)[2, 2]
  nvar_norm <- vcov(fit_norm)[2, 2]
  # robust (sandwich) variance
  rvar_pois <- sandwich(fit_pois)[2, 2]
  rvar_norm <- sandwich(fit_norm)[2, 2]
  # 6) Combine results into a 2 row data frame ------------------------------
  results <- rbind(
    data.frame(
      N           = N,
      lambda      = lambda,
      model       = "Poisson",
      bias_lambda = bias_pois_lambda,
      beta1_hat   = beta1_pois,
      nvar_beta1  = nvar_pois,
      rvar_beta1  = rvar_pois
    ),
    data.frame(
      N           = N,
      lambda      = lambda,
      model       = "Normal",
      bias_lambda = bias_norm_lambda,
      beta1_hat   = beta1_norm,
      nvar_beta1  = nvar_norm,
      rvar_beta1  = rvar_norm
    )
  )
  rownames(results) <- NULL
  return(results)
}

run_scenario <- function(N, lambda, R, x_vec) {
  # Run the simulation R times for a given (N, lambda) combination.
  # N: sample size (20 or 200)
  # lambda: true Poisson mean (4 or 10)
  # R: number of simulation replicates
  # x_vec: binary predictor vector of length N

  # 1) Initialize storage ------------------------------------------------------
  # list for storing repeated simulation outputs.
  results_list <- vector("list", length = R)
  # 2) Simulation loop ------------------------------------------------------
  for (r in seq_len(R)) {
    # Run one simulation for this scenario
    sim_res <- simulate_one(
      N      = N,
      lambda = lambda,
      x_vec  = x_vec
    )
    # Add simulation ID
    sim_res$sim_id <- r
    # Store into list
    results_list[[r]] <- sim_res
  }
  # 3) Combine results ------------------------------------------------------
  results <- do.call(rbind, results_list)
  rownames(results) <- NULL
  return(results)
}
