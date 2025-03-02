# load packages
library(tidyverse)
library(forcats)
library(ggthemes)

theme_set(theme_bw())

# load data from marginaleffects
p_t_mean_preds <- read_csv("mu_sims_data/p_t_mean_preds.csv")
p_t_sd_preds <- read_csv("mu_sims_data/p_t_sd_preds.csv")

glimpse(p_t_mean_preds)
glimpse(p_t_sd_preds)

p_t_mean_preds |>
  ggplot(aes(x=mu_vect, y=estimate, color=factor(as.character(F_val)))) +
  geom_line() +
  facet_grid(rows=vars(mu_groups_fct),
             cols = vars(s_h_fct)) + 
  scale_color_colorblind() + 
  labs(x = "mu",y="Mean frequency",color="F")
  
p_t_sd_preds |>
  ggplot(aes(x=mu_vect, y=estimate, color=factor(as.character(F_val)))) +
  geom_line() +
  facet_grid(rows=vars(mu_groups_fct),
             cols = vars(s_h_fct)) + 
  scale_color_colorblind() + 
  labs(x = "mu",y="Frequency sd",color="F")
