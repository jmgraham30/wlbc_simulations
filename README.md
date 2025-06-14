# R Project for Stochastic fluctuations of the facultative endosymbiont *Wolbachia* due to finite host population size

This repository contains the code and data for the manuscript 
"The contributions of host population size and maternal transmission rate to fluctuating *Wolbachia* frequencies."

Simulations for the manuscript were conducted using the R programming language. 
The code for the simulations is contained in the `R` directory, see code in 
files `wlbc_sims_no_ci.R`, `wlbc_sims_with_ci_01.R`, and `wlbc_sims_with_ci_45.R`. 
To run those simulations, one must install the R package [`symbiontmodeler`](https://github.com/jmgraham30/symbiontmodeler). 
All other scripts are for data analysis and figure generation. 

Data file `wlbc_simulations_results.csv` is required to run scripts `ms_figure_02_code.R`, `ms_figure_05_code.R` and `ms_figure_06_code.R` 
for creating plots shown in the manuscript. Manuscript figures 3 and 4 are created by running scripts 
`simulations_data_summary_models_gam.R` and `beta_reg_model_marginaleffects.R`.