# load packages
library(tidyverse) # for plots and data manipulation
library(latex2exp) # for LaTeX expressions
library(scales) # for scales
library(forcats) # for factor manipulation

# set plot theme
theme_set(theme_bw())

# load data
plot_03_a_mu_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

glimpse(plot_03_a_mu_df)

plot_03_a_mu_df <- plot_03_a_mu_df |>
  filter(N_val == 10000, s_h != 0.45, F_cv == 0.0, persist_prop > 0.0) |>
  group_by(s_h_fct, mu_groups_fct, F_val_m, mu_vect) |>
  summarise(n = n(),p_t_mean = mean(p_t_mean)) |>
  mutate(mu_groups_fct = factor(mu_groups_fct)) |>
  mutate(mu_group = fct_relevel(mu_groups_fct, c("mu_group = 1.0", "mu_group = 0.9"))) |>
  mutate(dist_from_04 = abs(0.4 - p_t_mean),
         dist_from_088 = abs(0.88 - p_t_mean),
         within_24_to_58 = ifelse(p_t_mean >= 0.24 & p_t_mean <= 0.58, "Yes", "No"),
         within_78_to_93 = ifelse(p_t_mean >= 0.78 & p_t_mean <= 0.93, "Yes", "No")) |>
  ungroup()

# glimpse data
glimpse(plot_03_a_mu_df)

plot_mu_df_04_ci <- plot_03_a_mu_df |>
  select(F_val_m, mu_vect, s_h_fct, mu_group, dist_from_04, within_24_to_58) |>
  mutate(F_val_m = ifelse(within_24_to_58 == "No", NA, F_val_m),
         mu_vect = ifelse(within_24_to_58 == "No", NA, mu_vect)) |>
  filter(!is.na(F_val_m) & !is.na(mu_vect)) 

glimpse(plot_mu_df_04_ci)

plot_mu_df_088_ci <- plot_03_a_mu_df |>
  select(F_val_m, mu_vect, s_h_fct, mu_group, dist_from_088, within_78_to_93) |>
  mutate(F_val_m = ifelse(within_78_to_93 == "No", NA, F_val_m),
         mu_vect = ifelse(within_78_to_93 == "No", NA, mu_vect)) |>
  filter(!is.na(F_val_m) & !is.na(mu_vect))

glimpse(plot_mu_df_088_ci)

############### CREATE PLOTS FOR FIGURE 3 ################

# breaks for the color scale
mybreaks_dist <- seq(0, 1, 0.15)

plot_03_a_mu_df |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(dist_from_04 = mean(dist_from_04)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), 
             fill = dist_from_04)) +
  geom_tile(stat="identity") +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks_dist), "Blues", rev = FALSE), 
    breaks = mybreaks_dist
  ) +
  geom_point(data = plot_mu_df_04_ci, aes(y = F_val_m, as.character(mu_vect)), 
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
    fill = TeX("Distance \n from 0.4"),
  )

plot_03_a_mu_df |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(dist_from_088 = mean(dist_from_088)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), fill = dist_from_088)) +
  geom_tile(stat="identity") +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks_dist), "Purples", rev = FALSE), 
    breaks = mybreaks_dist
  ) +
  geom_point(data = plot_mu_df_088_ci, aes(y = F_val_m, as.character(mu_vect)), 
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
    fill = TeX("Distance \n from 0.88"),
  )
