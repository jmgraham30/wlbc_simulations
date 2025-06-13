# load packages
library(tidyverse) # for plots and data manipulation
library(latex2exp) # for LaTeX expressions
library(scales) # for scales
library(forcats) # for factor manipulation
library(cowplot) # for combining plots

# set plot theme
theme_set(theme_bw())

# load data
plot_02_mu_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

glimpse(plot_02_mu_df)

plot_02_mu_df <- plot_02_mu_df |>
  filter(N_val == 10000, F_cv == 0.0, persist_prop == 1, p_t_var > 0) |>
  mutate(mu_groups_fct = factor(mu_groups_fct)) |>
  mutate(mu_group = fct_relevel(mu_groups_fct, c("mu_group = 1.0", "mu_group = 0.9")))

# glimpse data
glimpse(plot_02_mu_df)

############### CREATE PLOTS FOR FIGURE 6 ################

# breaks for the color scale
mybreaks_freq <- seq(0, 1, 0.15)
mybreaks_std <- c(0.0001, 0.001, 0.01, 0.1)

p1 <- plot_02_mu_df |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(p_t_mean = mean(p_t_mean)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), fill = p_t_mean)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.4f", p_t_mean)), size = 3, color = "black") +
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
    fill = TeX("Mean \nfrequency"),
  )

p1

p2 <-  plot_02_mu_df |>
  mutate(p_t_std = sqrt(p_t_var)) |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(p_t_std = mean(p_t_std)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), fill = p_t_std)) +
  geom_tile(stat="identity") +
  geom_text(aes(label = sprintf("%.4f", p_t_std)), size = 3, color = "black") +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks_std), "Zissou 1", rev = FALSE), 
    breaks = mybreaks_std,
    limits = c(0.0001, 0.17),
    trans = "log"
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
    # fill = TeX("Frequency \nstd (log)"),
    fill = TeX("Frequency \nstd"),
  )

p2

plot_02_mu_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

plot_02_mu_df <- plot_02_mu_df |>
  filter(N_val == 10000, F_cv == 0.1, persist_prop == 1, p_t_var > 0) |>
  mutate(mu_groups_fct = factor(mu_groups_fct)) |>
  mutate(mu_group = fct_relevel(mu_groups_fct, c("mu_group = 1.0", "mu_group = 0.9")))

# glimpse data
glimpse(plot_02_mu_df)

p3 <-  plot_02_mu_df |>
  mutate(p_t_std = sqrt(p_t_var)) |>
  group_by(F_val_m,mu_vect,s_h_fct,mu_group) |>
  summarise(p_t_std = mean(p_t_std)) |>
  ggplot(aes(y = F_val_m, x = as.character(mu_vect), fill = p_t_std)) +
  geom_tile(stat="identity") +
  geom_text(aes(label = sprintf("%.4f", p_t_std)), size = 3, color = "black") +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks_std), "Zissou 1", rev = FALSE), 
    breaks = mybreaks_std,
    # limits = c(0, 0.0131)
    limits = c(0.0001, 0.17),
    trans = "log"
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
    # fill = TeX("Frequency \nstd (log)"),
    fill = TeX("Frequency \nstd"),
  )

p3

combinedPlots <- plot_grid(p2, p3, p1, align = "v", nrow=3, rel_widths = c(1, 1))
combinedPlots

ggsave("~/Desktop/Fig6_N10000_p.pdf", combinedPlots, width=21, height=36, dpi=300, useDingbats=FALSE)
