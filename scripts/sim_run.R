# Project: GLM Simulation Study
# Script: sim_run.R
# Purpose: Execute the simulation study by:
#     - reading in the x vectors for N = 20 and N = 200
#     - running repeated simulations for each (N, lambda) combination
#     - saving the raw simulation results for later analysis
# R Version: 2025.05.1+513
# Package Version: sandwich (3.1-1)

library(sandwich)

# Source function definitions
source("scripts/sim_functions.R")

# 1) Read in binary predictor x for N = 20 and N = 200 --------------------
x_n20 <- read.csv("data/x_n20-1.csv")
x_n200 <- read.csv("data/x_n200.csv")

# Helper to select the correct x vector based on N
get_x_vector <- function(N) {
  if (N == 20) {
    return(x_n20$x)
  } else if (N == 200) {
    return(x_n200$x)
  } else {
    stop("Unsupported N: ", N)
  }
}

# 2) Define simulation settings -------------------------------------------
N_values <- c(20, 200)
lambda_values <- c(4, 10)

# Number of simulations:
R_small <- 5 # for debugging
R_full <- 2000 # for final run


# 3) Small test run -------------------------------------------------------

set.seed(2345281) # for reproducibility

# Choose a scenario to test
N_test <- 20
lambda_test <- 4
# Retrieve the appropriate predictor vector for N = 20
x_vec_test <- get_x_vector(N_test)
# Run a small number of simulations (for debug)
test_results <- run_scenario(
  N      = N_test,
  lambda = lambda_test,
  R      = R_small,
  x_vec  = x_vec_test
)
# Inspect results
print(test_results)

# 4) Run all scenarios ----------------------------------------------------
# Create all (N, lambda) combinations
scenarios <- expand.grid(
  N      = N_values,
  lambda = lambda_values
)
# Initialize list to store results for each scenario
# 4 scenarios: (20,4), (20,10), (200,4), (200,10)
all_results_list <- vector(
  "list",
  nrow(scenarios)
)
# Loop through each scenario
for (i in seq_len(nrow(scenarios))) {
  # Extract the sample size and lambda value
  N_i <- scenarios$N[i]
  lambda_i <- scenarios$lambda[i]
  # Get the appropriate x vector for this N
  x_vec_i <- get_x_vector(N_i)
  # Run the simulation R_small times for this scenario
  cat(
    "Running scenario", i, "of",
    nrow(scenarios),
    ": N =", N_i, ", lambda =",
    lambda_i, "\n"
  )
  scenario_res <- run_scenario(
    N      = N_i,
    lambda = lambda_i,
    R      = R_small, # <- still using small R here
    x_vec  = x_vec_i
  )
  # Store results
  all_results_list[[i]] <- scenario_res
}

# Combine all scenarios into one data frame
sim_results_small <- do.call(
  rbind,
  all_results_list
)
rownames(sim_results_small) <- NULL
cat(
  "Dimensions of sim_results_small:",
  dim(sim_results_small), "\n"
)
# Check first few rows
head(sim_results_small)


# 5) Save results ---------------------------------------------------------

if (!dir.exists("results")) {
  dir.create("results")
}

save(sim_results_small,
  file = "results/sim_results_small.RData"
)

# 6) Full simulation run ------------------------------------------------------

R_full <- 2000 # number of replicates for the final simulation

set.seed(97659) # reproducibility for the full simulation run

# Prepare storage for full results
all_results_list_full <- vector("list", 
                                nrow(scenarios))

for (i in seq_len(nrow(scenarios))) {
  N_i <- scenarios$N[i]
  lambda_i <- scenarios$lambda[i]

  # Scenario-specific seed ensures independent replication sets
  set.seed(5000 + i)

  x_vec_i <- get_x_vector(N_i)

  cat(
    "[FULL RUN] Scenario", i, "of", 
    nrow(scenarios),
    ": N =", N_i, ", lambda =", 
    lambda_i, "\n"
  )

  # Now using R_full = 2000
  scenario_res_full <- run_scenario(
    N      = N_i,
    lambda = lambda_i,
    R      = R_full,
    x_vec  = x_vec_i
  )

  all_results_list_full[[i]] <- scenario_res_full
}

# Combine full results
sim_results <- do.call(rbind, 
                       all_results_list_full)
rownames(sim_results) <- NULL

# Ensure results folder exists
if (!dir.exists("results")) {
  dir.create("results")
}

# Save full simulation output
save(sim_results, file = "results/sim_results.RData")

cat(
  "Full simulation complete.\n",
  "Dimensions of sim_results:", dim(sim_results), "\n"
)
