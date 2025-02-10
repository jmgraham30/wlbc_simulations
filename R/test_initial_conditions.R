# load packages
library(tidyverse) # for data manipulation
library(furrr) # for parallel processing
library(wlbcmodeler) # contains wolbachia infection frequency model implementation

# parallel processing setup
n_cores <- parallel::detectCores() - 2
plan(multisession, workers = n_cores)

# define parameter space values
rep_num_vals <- 1 # number of replicates
N_val_vals <- c(1e3,1e6) # population size
F_m_vals <- seq(1.0,1.5,by=0.05) # fecundity 
F_cv_vals <- c(0.01,0.1) # coefficient of variation for F
sh_vals <- c(0.1,0.45) # CI strength
# transmission rates and binomial proportions
mu_vect_vals <- list(c(0.01,0.8),c(0.05,0.8),
                     c(0.1,0.8),c(0.15,0.8),c(0.2,0.8),
                     c(0.3,0.8),c(0.4,0.8),c(0.5,0.8))
bin_props_vals <- list(c(1.0,0.0))
p_t_init <- c(0.2,0.4,0.6)

# create parameter space data frame
parms_df <- expand_grid(rep_num = rep_num_vals,
                        F_val_m = F_m_vals,
                        F_cv = F_cv_vals,
                        mu_vect = mu_vect_vals,
                        bin_props = bin_props_vals,
                        s_h = sh_vals,
                        N_val = N_val_vals,
                        p_t_init = p_t_init
)


glimpse(parms_df)

test_function <- function(rep_num,F_val_m,F_cv,mu_vect,bin_props,s_h,N_val,p_t_init,...){
  

  sim_1 <- infection_freq_rf_rmu_sim(rep_num,F_val_m,F_cv,mu_vect,bin_props,N_val,p_t_init)
  sim_2 <- infection_freq_rf_rmu_ci_sim(rep_num,F_val_m,F_cv,mu_vect,bin_props,s_h,N_val,p_t_init)
  return(list(sim_1=sim_1,sim_2=sim_2))
    
}

# run simulations
sim_test_df <- future_pmap(parms_df, test_function,
                              .options = furrr_options(seed = TRUE),
                              .progress = TRUE) 


mean_extractor <- function(sim_test_df_input){
  
  tibble(p_t_mean_1 = sim_test_df_input$sim_1$p_t_mean[[1]],
         p_t_mean_2 = sim_test_df_input$sim_2$p_t_mean[[1]])  

}

tdlk <- sim_test_df |> 
  map(mean_extractor) |>
  list_rbind()

test_df <- cbind(parms_df,tdlk)

glimpse(test_df)

test_df |>
  drop_na() |>
  View()
