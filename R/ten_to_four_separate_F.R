# load packages
library(tidyverse) # for plots and data manipulation
library(latex2exp) # for LaTeX expressions
library(scales) # for scales
library(forcats) # for factor manipulation
library(wlbcmodeler) # for wolbachia model functions

# set plot theme
theme_set(theme_bw())

# load data
plot_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

glimpse(plot_df)

# filter 10^4
plot_df_ten_four <- plot_df |>
  filter(N_val == 1e+04) 

glimpse(plot_df_ten_four)

plot_df_ten_four |>
  ggplot(aes(x = p_t_std_log, y = p_t_mean)) + 
  geom_point(alpha=0.25) +
  facet_wrap(~ F_cv_fct) +
  labs(
    x = TeX("$p_t$ (standard deviation log-scale)"),
    y = TeX("$p_t$ (mean)"),
    title = TeX("$N = 10^4$: $F_{cv}$ coefficient of variation")
  ) 
