# script to run simulations for mu switching

# load packages
library(tidyverse) # for data manipulation
library(furrr) # for parallel processing
library(wlbcmodeler) # contains wolbachia infection frequency model implementation

theme_set(theme_minimal())

# parallel processing setup
n_cores <- parallel::detectCores() - 2
plan(multisession, workers = n_cores)

# define parameter space values
rep_num_vals <- 1:25 # number of replicates

mu_switching <- function(rep_num){
  
  p_t <- numeric(10000)
  p_t[1] <- 0.4
  for (iter in 2:10000){
    if (iter <= 5000){
      p_t[iter] <- infection_freq_rmu(p_t[iter-1],
                                      F_val=1.125,
                                      mu_vect=c(0.005,0.0),
                                      bin_props=c(1.0,0.0),
                                      N_val=10000)
    }else {
      p_t[iter] <- infection_freq_rmu(p_t[iter-1],
                                      F_val=1.125,
                                      mu_vect=c(0.005,0.8),
                                      bin_props=c(0.9,0.1),
                                      N_val=10000)
    }
  }
  
  return(tibble(rep_num = rep_num,p_t=list(p_t)))
  
}

sim_data <- future_map(rep_num_vals, mu_switching,
                        .options = furrr_options(seed = TRUE),
                        .progress = TRUE) |>
  list_rbind()

glimpse(sim_data)

sim_data_results <- sim_data |>
  unnest(p_t) |>
  mutate(index = rep(1:10000,25))

glimpse(sim_data_results)

sim_data_results |>
  ggplot(aes(x=index,y=p_t,color=as.character(rep_num))) + 
  geom_line() + 
  ylim(c(0,1))

sim_data_results |>
  ggplot(aes(x=index,y=p_t,color=as.character(rep_num))) + 
  geom_line() + 
  facet_wrap(~rep_num) +
  ylim(c(0,1))
