# script to run simulations for wolbachia infection frequency model with CI

# load packages
library(tidyverse) # for data manipulation
library(furrr) # for parallel processing
library(wlbcmodeler) # contains wolbachia infection frequency model implementation

# parallel processing setup
n_cores <- parallel::detectCores() - 2
plan(multisession, workers = n_cores)

# define parameter space values
rep_num_vals <- 1:25 # number of replicates
N_val_vals <- c(1e3,1e4,1e5,1e6) # population size
F_m_vals <- seq(1.0,1.5,by=0.025) # fecundity 
F_cv_vals <- c(0.01,0.1) # coefficient of variation for F
sh_vals <- c(0.1,0.45) # CI strength
# transmission rates and binomial proportions
mu_vect_vals <- list(c(0.001,0.8),c(0.005,0.8),c(0.01,0.8),c(0.05,0.8),
                     c(0.1,0.8),c(0.15,0.8),c(0.2,0.8),c(0.25,0.8),
                     c(0.3,0.8),c(0.35,0.8),c(0.4,0.8),c(0.45,0.8),c(0.5,0.8))
bin_props_vals <- list(c(1.0,0.0),c(0.9,0.1))

# create parameter space data frame
parms_df <- expand_grid(rep_num = rep_num_vals,
                          F_val_m = F_m_vals,
                          F_cv = F_cv_vals,
                          mu_vect = mu_vect_vals,
                          bin_props = bin_props_vals,
                          s_h = sh_vals,
                          N_val = N_val_vals
)

# run simulations
sim_with_ci_df <- future_pmap(parms_df, infection_freq_rf_rmu_ci_sim,
                                        .options = furrr_options(seed = TRUE),
                                        .progress = TRUE) |>
  list_rbind() 

# save the results, note that only every other row is required
# sim_with_ci_df[seq(1,nrow(sim_with_ci_df),by=2), ]
