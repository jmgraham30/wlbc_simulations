library(tidyverse)
theme_set(theme_minimal())

# Load data
sim_res <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

glimpse(sim_res)


sim_res <- sim_res |>
  filter(bin_props < 1.0,F_val_m > 1) |>
  mutate(bin_props_c = 1 - bin_props,
         mu_vect_c = 0.8,
         p_star = 1 - (F_val_m/(F_val_m - 1))*(mu_vect*bin_props + mu_vect_c*bin_props_c))

sim_res <- sim_res |>
  mutate(p_star_neg = ifelse(p_star < 0, "Yes", "No"))

glimpse(sim_res)

sim_res |>
  ggplot(aes(x=p_star_neg,y=p_t_mean)) + 
  geom_jitter(alpha=0.3)

sim_res |>
  ggplot(aes(x=p_star_neg,y=log10(sqrt(p_t_var)))) + 
  geom_jitter(alpha=0.3)

sim_res |>
  ggplot(aes(x=p_t_mean,y=log10(sqrt(p_t_var)))) + 
  facet_wrap(~p_star_neg) +
  geom_jitter(alpha=0.3) + 
  xlim(c(0,1))
