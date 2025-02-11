# load packages
library(tidyverse) # for data manipulation
library(furrr) # for parallel processing
library(wlbcmodeler) # contains wolbachia infection frequency model implementation

# parallel processing setup
n_cores <- parallel::detectCores() - 2
plan(multisession, workers = n_cores)

# define parameter space values
rep_num_vals <- 1:25 # number of replicates
N_val_vals <- c(1e6) # population size
F_m_vals <- c(1.3) # fecundity 
F_cv_vals <- c(0.0) # coefficient of variation for F
#sh_vals <- c(0.1,0.45) # CI strength
# transmission rates and binomial proportions
mu_vect_vals <- list(c(0.1,0.8))
bin_props_vals <- list(c(1.0,0.0))
p_t_init <- c(0.2,0.4,0.6)

# create parameter space data frame
parms_df <- expand_grid(rep_num = rep_num_vals,
                        F_val_m = F_m_vals,
                        F_cv = F_cv_vals,
                        mu_vect = mu_vect_vals,
                        bin_props = bin_props_vals,
                        #s_h = sh_vals,
                        N_val = N_val_vals,
                        p_t_init = p_t_init
)


glimpse(parms_df)

test_function <- function(rep_num,F_val_m,F_cv,mu_vect,bin_props,N_val,p_t_init,...){
  
  
  sim_1 <- infection_freq_rf_rmu_sim(rep_num,F_val_m,F_cv,mu_vect,bin_props,N_val,p_t_init)
  return(cbind(sim_1[1, ],tibble(p_t_init = p_t_init)))
  
}

# run simulations
sim_test_df <- future_pmap(parms_df, test_function,
                           .options = furrr_options(seed = TRUE),
                           .progress = TRUE) |>
  list_rbind()

glimpse(sim_test_df)


sim_test_df |>
  ggplot(aes(x=as.character(p_t_init),y=p_t_mean)) + 
  geom_boxplot()
