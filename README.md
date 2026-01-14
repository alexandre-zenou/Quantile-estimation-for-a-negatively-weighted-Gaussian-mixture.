# Quantile estimation for a negatively weighted Gaussian mixture

This repository contains an academic project from the **Monte Carlo Methods (M1)** course at **Université Paris Dauphine – PSL**, supervised by **Christian Robert**.

The project studies a *negatively weighted Gaussian mixture* and develops Monte Carlo methods for **simulation, distribution estimation, and quantile estimation**.

## Covered methods

- Inverse CDF simulation  
- Accept–reject sampling (standard and stratified)  
- Empirical CDF and empirical quantile estimation  
- Confidence intervals (CLT-based)  
- Importance sampling  
- Control variates  

Implemented in **R**, with a strong focus on **theoretical justification**, **numerical validation**, and **computational efficiency**.

## Repository content

- `monte_carlo_quantile_mixture.Rmd`: fully reproducible RMarkdown file containing all Monte Carlo implementations, figures and numerical results
- `assignment_part1.pdf`, `assignment_part2.pdf`, `assignment_part3.pdf`: official project statements released throughout the course
- `report_part1.pdf`, `report_part2.pdf`, `report_part3.pdf`: written reports presenting theoretical derivations, mathematical results and numerical experiments
- `tutorials/`: course tutorials (TP) with my personal implementations and corrections


## Reproducibility

All experiments are reproducible from `monte_carlo_quantile_mixture.Rmd`. Random seeds are fixed within the code to reproduce the reported results.

Alexandre Zenou, Samuel Elbaz, Sasha Assouly

Université Paris Dauphine
