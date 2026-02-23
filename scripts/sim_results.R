# Project: GLM Simulation Study
# Script: sim_results.R
# Purpose:
#   Summarize simulation output and produce:
#     - a table of mean bias for lambda-hat
#     - a table of mean naive and robust variances for beta1-hat
#     - a plot of 95% CIs for beta1 using naive vs robust SEs
# Package Versions:
# dplyr (1.1.4), tidyr (1.3.1), ggplot2 (3.5.2)

library(dplyr)
library(tidyr)
library(ggplot2)

# 1) Load simulation results ----------------------------------------------
load("results/sim_results.RData") # loads object sim_results

# 2) Summarize bias of lambda-hat and variances of beta1-hat --------------
summary_results <- sim_results |>
  group_by(N, lambda, model) |>
  summarise(
    mean_bias_lambda = mean(bias_lambda),
    mean_beta1_hat = mean(beta1_hat),
    mean_nvar_beta1 = mean(nvar_beta1),
    mean_rvar_beta1 = mean(rvar_beta1),
    .groups = "drop"
  )

# Table for report --------------------------------------------------------
# Table 1: Mean bias of lambda-hat by N, lambda, and model
bias_table <- summary_results |>
  select(
    N, lambda, model,
    mean_bias_lambda
  ) |>
  mutate(
    mean_bias_lambda = round(mean_bias_lambda, 3)
  ) |>
  arrange(N, lambda, model)

# Table 2: Mean naive and robust variances for beta1-hat
variance_table <- summary_results |>
  select(
    N, lambda, model, mean_nvar_beta1,
    mean_rvar_beta1
  ) |>
  mutate(
    mean_nvar_beta1 = round(mean_nvar_beta1, 4),
    mean_rvar_beta1 = round(mean_rvar_beta1, 4)
  ) |>
  arrange(N, lambda, model)


# 4) Construct data for 95% CIs for beta1 ------------------------------------
ci_data <- summary_results |>
  mutate(
    se_naive  = sqrt(mean_nvar_beta1),
    se_robust = sqrt(mean_rvar_beta1)
  ) |>
  pivot_longer(
    cols      = c(se_naive, se_robust),
    names_to  = "var_type",
    values_to = "se_beta1"
  ) |>
  mutate(
    var_type = dplyr::recode(var_type,
      se_naive  = "Naive",
      se_robust = "Robust"
    ),
    ci_lower = mean_beta1_hat - 1.96 * se_beta1,
    ci_upper = mean_beta1_hat + 1.96 * se_beta1
  )

# 5) Plot: 95% CIs for beta1 (naive vs robust) ----------------------------
ci_plot <- ggplot(
  ci_data,
  aes(
    x     = model,
    y     = mean_beta1_hat,
    ymin  = ci_lower,
    ymax  = ci_upper,
    color = var_type
  )
) +
  geom_pointrange(
    position = position_dodge(width = 0.3),
    size = 0.3       # makes the dot + line thinner + cleaner
  ) +
  facet_grid(
    lambda ~ N,
    labeller = label_both
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  labs(
    title = expression("Figure 1. 95% Confidence Intervals for " * beta[1]),
    x     = "Model",
    y     = expression(hat(beta)[1] ~ " estimate"),
    color = "Variance Estimator"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),  # CENTER title
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

