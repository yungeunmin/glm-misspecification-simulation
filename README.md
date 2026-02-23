# GLM Misspecification Simulation

Monte Carlo simulation comparing a correctly specified **Poisson GLM** to a misspecified **Gaussian linear model** for count data.

## Objective

Evaluate how distributional misspecification affects:

- Mean estimation  
- Coefficient estimates (β₁ = 0)  
- Naïve vs robust (sandwich) variance estimates  
- 95% confidence intervals  

## Simulation Design

- Data-generating process: Y ~ Poisson(λ)  
- Sample sizes: N = 20, 200  
- Means: λ = 4, 10  
- Replications: 2000 per scenario  
- Predictor: fixed binary vector, independent of outcome  

Each dataset is analyzed using:

- Poisson GLM (log link)  
- Gaussian linear model (identity link)  

## Main Finding

Misspecification does not materially affect mean estimation in this null setting, but inflates variance estimates and widens confidence intervals—especially at small sample sizes.

## Structure

```
glm-misspecification-simulation/
│
├── README.md
├── data/
│   ├── x_n20.csv
│   └── x_n200.csv
│
├── scripts/
│   ├── sim_functions.R
│   ├── sim_run.R
│   └── sim_results.R
│
└── report/
    ├── glm-misspecification-simulation.Rmd
    └── GLM_Simulation_Study_Report.pdf
```
