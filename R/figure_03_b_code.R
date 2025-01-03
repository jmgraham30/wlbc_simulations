# load packages
library(tidyverse) # for plots and data manipulation
library(latex2exp) # for LaTeX expressions
library(scales) # for scales
library(forcats) # for factor manipulation

# set plot theme
theme_set(theme_bw())

# load data
plot_03_b_mu_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

glimpse(plot_03_b_mu_df)

plot_03_b_mu_df <- plot_03_b_mu_df |>
  filter(N_val == 10000, s_h != 0.45, F_cv == 0.0) |>
  group_by(s_h_fct, mu_groups_fct, F_val_m, mu_vect) |>
  summarise(n = n(),p_t_mean = mean(p_t_mean)) |>
  mutate(mu_groups_fct = factor(mu_groups_fct)) |>
  mutate(mu_group = fct_relevel(mu_groups_fct, c("mu_group = 1.0", "mu_group = 0.9"))) |>
  mutate(dist_from_02 = abs(0.2 - p_t_mean),
         dist_from_095 = abs(0.92 - p_t_mean),
         within_11_to_32 = ifelse(p_t_mean >= 0.11 & p_t_mean <= 0.32, "Yes", "No"),
         within_91_to_97 = ifelse(p_t_mean >= 0.91 & p_t_mean <= 0.97, "Yes", "No")) |>
  ungroup()

# glimpse data
glimpse(plot_03_b_mu_df)

plot_mu_df_02_ci <- plot_03_b_mu_df |>
  select(F_val_m, mu_vect, s_h_fct, mu_group, dist_from_02, within_11_to_32) |>
  mutate(F_val_m = ifelse(within_11_to_32 == "No", NA, F_val_m),
         mu_vect = ifelse(within_11_to_32 == "No", NA, mu_vect)) |>
  filter(!is.na(F_val_m) & !is.na(mu_vect)) 

glimpse(plot_mu_df_02_ci)

plot_mu_df_095_ci <- plot_03_b_mu_df |>
  select(F_val_m, mu_vect, s_h_fct, mu_group, dist_from_095, within_91_to_97) |>
  mutate(F_val_m = ifelse(within_91_to_97 == "No", NA, F_val_m),
         mu_vect = ifelse(within_91_to_97 == "No", NA, mu_vect)) |>
  filter(!is.na(F_val_m) & !is.na(mu_vect))

glimpse(plot_mu_df_095_ci)

############### CREATE PLOTS FOR FIGURE 3 ################

# breaks for the color scale
mybreaks_dist <- seq(0, 1, 0.15)

plot_03_b_mu_df |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(dist_from_02 = mean(dist_from_02)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), 
             fill = dist_from_02)) +
  geom_tile(stat="identity") +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks_dist), "Blues", rev = FALSE), 
    breaks = mybreaks_dist
  ) +
  geom_point(data = plot_mu_df_02_ci, aes(y = F_val_m, as.character(mu_vect)), 
             color = "red", size = 5, shape = 18) +
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
    fill = TeX("Distance \n from 0.2"),
  )

plot_03_b_mu_df |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(dist_from_095 = mean(dist_from_095)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), fill = dist_from_095)) +
  geom_tile(stat="identity") +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks_dist), "Purples", rev = FALSE), 
    breaks = mybreaks_dist
  ) +
  geom_point(data = plot_mu_df_095_ci, aes(y = F_val_m, as.character(mu_vect)), 
             color = "red", size = 5, shape = 18) +
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
    fill = TeX("Distance \n from 0.95"),
  )
