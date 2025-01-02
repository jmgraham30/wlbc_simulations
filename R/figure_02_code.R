# load packages
library(tidyverse) # for plots and data manipulation
library(latex2exp) # for LaTeX expressions
library(scales) # for scales
library(forcats) # for factor manipulation

# set plot theme
theme_set(theme_bw())

# load data
plot_02_mu_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

glimpse(plot_02_mu_df)

plot_02_mu_df <- plot_02_mu_df |>
  filter(N_val == 10000, s_h != 0.45, F_cv == 0.0, 
         persist_prop > 0.0, p_t_var > 0,
         fpop_scale >= 10) |>
  mutate(mu_groups_fct = factor(mu_groups_fct)) |>
  mutate(mu_group = fct_relevel(mu_groups_fct, c("mu_group = 1.0", "mu_group = 0.9")))

# glimpse data
glimpse(plot_02_mu_df)

############### CREATE PLOTS FOR FIGURE 2 ################

# breaks for the color scale
mybreaks_freq <- seq(0, 1, 0.15)
mybreaks_std <- round(log10(c(0.00004,0.00008,0.0001,0.0005,0.001,0.005,0.007,0.009)),3)


plot_02_mu_df |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(p_t_mean = mean(p_t_mean)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), fill = p_t_mean)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks_freq), "viridis", rev = FALSE), 
    breaks = mybreaks_freq
  ) +
  facet_wrap(~ s_h_fct + mu_group) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = TeX("$\\mu$"),
    y = TeX("$F$"),
    fill = TeX("Mean \n frequency"),
  )

plot_02_mu_df |>
  mutate(p_t_std_log = log10(sqrt(p_t_var))) |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(p_t_std_log = mean(p_t_std_log)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), fill = p_t_std_log)) +
  geom_tile(stat="identity") +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks_std), "viridis", rev = FALSE), 
    breaks = mybreaks_std
  ) +
  facet_wrap(~ s_h_fct + mu_group) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = TeX("$\\mu$"),
    y = TeX("$F$"),
    fill = TeX("Frequency \n std (log)"),
  )
