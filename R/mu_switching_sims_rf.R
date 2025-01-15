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

mu_switching_1 <- function(rep_num){
  
  p_t <- numeric(10000)
  p_t[1] <- 0.4
  
  F_val_m <- 1.125
  F_cv <- 0.1
  F_val_s <- exp(stats::rnorm(10000, mean = log(F_val_m), 
                              sd = sqrt(log(F_cv^2 + 1))))
  F_val <- base::ifelse(F_val_s < 0.001, 0.001, F_val_s)
  
  for (iter in 2:10000){
    if (iter <= 5000){
      p_t[iter] <- infection_freq_rmu(p_t[iter-1],
                                      F_val=F_val[iter],
                                      mu_vect=c(0.005,0.0),
                                      bin_props=c(1.0,0.0),
                                      N_val=10000)
    }else {
      p_t[iter] <- infection_freq_rmu(p_t[iter-1],
                                      F_val=F_val[iter],
                                      mu_vect=c(0.005,0.8),
                                      bin_props=c(0.9,0.1),
                                      N_val=10000)
    }
  }
  
  return(tibble(rep_num = rep_num,p_t=list(p_t)))
  
}

sim_data <- future_map(rep_num_vals, mu_switching_1,
                       .options = furrr_options(seed = TRUE),
                       .progress = TRUE) |>
  list_rbind()

glimpse(sim_data)

sim_data_results <- sim_data |>
  unnest(p_t) |>
  mutate(index = rep(1:10000,25))

glimpse(sim_data_results)

sim_data_results |>
  filter(index >= 4950 & index <= 5100) |>
  ggplot(aes(x=index,y=p_t,color=as.character(rep_num))) + 
  geom_line() +
  theme(legend.position = "none") + 
  labs(title="F=1.125, mu=0.005 (1 to 2 groups), s_h=0.0",
       x="Generations", y="Frequency") +
  ylim(c(0,1))


mu_switching_2 <- function(rep_num){
  
  p_t <- numeric(10000)
  p_t[1] <- 0.4
  
  F_val_m <- 1.15
  F_cv <- 0.1
  F_val_s <- exp(stats::rnorm(10000, mean = log(F_val_m), 
                              sd = sqrt(log(F_cv^2 + 1))))
  F_val <- base::ifelse(F_val_s < 0.001, 0.001, F_val_s)
  
  for (iter in 2:10000){
    if (iter <= 5000){
      p_t[iter] <- infection_freq_rmu(p_t[iter-1],
                                      F_val=F_val[iter],
                                      mu_vect=c(0.005,0.0),
                                      bin_props=c(1.0,0.0),
                                      N_val=10000)
    }else {
      p_t[iter] <- infection_freq_rmu(p_t[iter-1],
                                      F_val=F_val[iter],
                                      mu_vect=c(0.1,0.0),
                                      bin_props=c(1.0,0.0),
                                      N_val=10000)
    }
  }
  
  return(tibble(rep_num = rep_num,p_t=list(p_t)))
  
}

sim_data <- future_map(rep_num_vals, mu_switching_2,
                       .options = furrr_options(seed = TRUE),
                       .progress = TRUE) |>
  list_rbind()

glimpse(sim_data)

sim_data_results <- sim_data |>
  unnest(p_t) |>
  mutate(index = rep(1:10000,25))

glimpse(sim_data_results)

sim_data_results |>
  filter(index >= 4950 & index <= 5100) |>
  ggplot(aes(x=index,y=p_t,color=as.character(rep_num))) + 
  geom_line() +
  theme(legend.position = "none") + 
  labs(title="F=1.15, mu=0.005 to 0.1, s_h=0.0",
       x="Generations", y="Frequency") +
  ylim(c(0,1))

mu_switching_3 <- function(rep_num){
  
  p_t <- numeric(10000)
  p_t[1] <- 0.4
  
  F_val_m <- 1.025
  F_cv <- 0.1
  F_val_s_1 <- exp(stats::rnorm(5000, mean = log(F_val_m), 
                              sd = sqrt(log(F_cv^2 + 1))))
  
  F_val_m <- 1.075
  F_val_s_2 <- exp(stats::rnorm(5000, mean = log(F_val_m), 
                                sd = sqrt(log(F_cv^2 + 1))))
  
  F_val_s <- c(F_val_s_1,F_val_s_2)
  
  F_val <- base::ifelse(F_val_s < 0.001, 0.001, F_val_s)
  
  for (iter in 2:10000){
    if (iter <= 5000){
      p_t[iter] <- infection_freq_rmu_ci(p_t[iter-1],
                                         F_val=F_val[iter],
                                         mu_vect=c(0.005,0.0),
                                         bin_props=c(1.0,0.0),
                                         s_h = 0.1,
                                         N_val=10000)
    }else {
      p_t[iter] <- infection_freq_rmu(p_t[iter-1],
                                      F_val=F_val[iter],
                                      mu_vect=c(0.05,0.0),
                                      bin_props=c(1.0,0.0),
                                      N_val=10000)
    }
  }
  
  return(tibble(rep_num = rep_num,p_t=list(p_t)))
  
}

sim_data <- future_map(rep_num_vals, mu_switching_3,
                       .options = furrr_options(seed = TRUE),
                       .progress = TRUE) |>
  list_rbind()

glimpse(sim_data)

sim_data_results <- sim_data |>
  unnest(p_t) |>
  mutate(index = rep(1:10000,25))

glimpse(sim_data_results)

sim_data_results |>
  filter(index >= 4950 & index <= 5150) |>
  ggplot(aes(x=index,y=p_t,color=as.character(rep_num))) + 
  geom_line() +
  theme(legend.position = "none") + 
  labs(title="F=1.025 to 1.075, mu=0.005 to 0.05, s_h=0.1 to 0.0",
       x="Generations", y="Frequency") +
  ylim(c(0,1))



