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

- `Code projet.Rmd`: full reproducible RMarkdown code (all implementations + figures/results)
- `Consigne 1.pdf`, `Consigne 2.pdf`, `Consigne 3.pdf`: official project statements
- `Partie 1.pdf`, `Partie 3.pdf`: written reports (math results + numerical experiments)
- `TP/`: tutorials (TP) material and my personal corrections

## Reproducibility

All experiments are reproducible from `Code projet.Rmd`. Random seeds are fixed within the code to reproduce the reported results.
