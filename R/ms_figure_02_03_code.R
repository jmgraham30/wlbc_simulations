# load packages
library(tidyverse) # for plots and data manipulation
library(latex2exp) # for LaTeX expressions
library(scales) # for scales
library(forcats) # for factor manipulation
library(ggthemes) # for color theme
library(patchwork) # for combining plots
library(cowplot) # for combining plots
library(scales) # for avoiding plotting scientific numbers

# set plot theme
theme_set(theme_bw())

# load data
plot_04_mu_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv") 

# glimpse data
glimpse(plot_04_mu_df)

plot_04_mu_df <- plot_04_mu_df |>
  filter(persist_prop == 1, p_t_var > 0.0) |>
  mutate(p_t_std = sqrt(p_t_var),
         p_t_std_log = log10(p_t_std)) |>
  mutate(mu_group = factor(mu_groups_fct)) |>
  mutate(mu_group = fct_relevel(mu_group, c("mu_group = 1.0", "mu_group = 0.9"))) |>
  mutate(F_cv_group = factor(F_cv_fct)) |>
  mutate(F_cv_group = fct_relevel(F_cv_group, c("F_cv = 0.0", "F_cv = 0.01", "F_cv = 0.1")))

# glimpse data
glimpse(plot_04_mu_df)

# View(subset(plot_04_mu_df, p_t_mean < 0.1 & N_val == 1e+04))

mybreaks_freq <- seq(0, 1, 0.15)

### plot std data ###
p_0 <- plot_04_mu_df |>
  filter(s_h_fct == "s_h = 0.0") |>
  ggplot(aes(x = mu_group, y = p_t_std, fill = F_cv_group)) +
  geom_boxplot(outliers=FALSE) + 
  geom_point(aes(color = p_t_mean), 
             position = position_jitterdodge(jitter.width=0.15),size=0.45) + 
  facet_wrap(~N_val, nrow=1) +
  scale_color_gradientn(
    colours = hcl.colors(length(mybreaks_freq), "viridis", rev = FALSE), 
    breaks = mybreaks_freq
  ) +
  labs(
    x = TeX("$\\mu$ group"), 
    y = "Frequency standard deviation", 
    color = "Mean \n frequency", 
    fill = "CV",
    title = TeX("$s_h = 0.0$")) +
  scale_y_continuous(trans = 'log10', 
                     labels = label_comma(),
                     limits = c(0.000006, 0.2)) +
  annotation_logticks(sides="l")

p_1 <- plot_04_mu_df |>
  filter(s_h_fct == "s_h = 0.1") |>
  ggplot(aes(x = mu_group, y = p_t_std, fill = F_cv_group)) +
  geom_boxplot(outliers = FALSE) + 
  geom_point(aes(color = p_t_mean), 
             position = position_jitterdodge(jitter.width=0.15),size=0.45) + 
  facet_wrap(~N_val, nrow=1) +
  scale_color_gradientn(
    colours = hcl.colors(length(mybreaks_freq), "viridis", rev = FALSE), 
    breaks = mybreaks_freq
  ) +
  labs(
    x = TeX("$\\mu$ group"), 
    y = "Frequency standard deviation", 
    color = "Mean \n frequency", 
    fill = "CV",
    title = TeX("$s_h = 0.1$")) +
  scale_y_continuous(trans = 'log10', 
                     labels = label_comma(),
                     limits = c(0.000006, 0.2)) +
  annotation_logticks(sides="l")

p_2 <- plot_04_mu_df |>
  filter(s_h_fct == "s_h = 0.45") |>
  ggplot(aes(x = mu_group, y = p_t_std, fill = F_cv_group)) +
  geom_boxplot(outliers = FALSE) + 
  geom_point(aes(color = p_t_mean), 
             position = position_jitterdodge(jitter.width=0.15),size=0.45) + 
  facet_wrap(~N_val, nrow=1) +
  scale_color_gradientn(
    colours = hcl.colors(length(mybreaks_freq), "viridis", rev = FALSE), 
    breaks = mybreaks_freq
  ) +
  labs(
    x = TeX("$\\mu$ group"), 
    y = "Frequency standard deviation", 
    color = "Mean \n frequency", 
    fill = "CV",
    title = TeX("$s_h = 0.45$")) +
  scale_y_continuous(trans = 'log10', 
                     labels = label_comma(),
                     limits = c(0.000006, 0.2)) +
  annotation_logticks(sides="l")

combinedPlots <- plot_grid(p_0, p_1, p_2, align = "v", nrow=3, rel_widths = c(1, 1))
combinedPlots

ggsave("~/Desktop/Fig3_std_p.pdf", combinedPlots, width=12, height=12, dpi=300, useDingbats=FALSE)

####### Plot frequency p data ########
p_3 <- plot_04_mu_df |>
  filter(s_h_fct == "s_h = 0.0") |>
  ggplot(aes(x = mu_group, y = p_t_mean, fill = F_cv_group)) +
  geom_boxplot(outliers=FALSE) + 
  geom_point(position = position_jitterdodge(jitter.width=0.15),size=0.45) + 
  facet_wrap(~N_val, nrow=1) +
  labs(
    x = TeX("$\\mu$ group"), 
    y = "Mean frequency (p)", 
    color = "Mean \n frequency", 
    fill = "CV",
    title = TeX("$s_h = 0.0$")
  ) + ylim(c(0,1))

p_4 <- plot_04_mu_df |>
  filter(s_h_fct == "s_h = 0.1") |>
  ggplot(aes(x = mu_group, y = p_t_mean, fill = F_cv_group)) +
  geom_boxplot(outliers=FALSE) + 
  geom_point(position = position_jitterdodge(jitter.width=0.15),size=0.45) + 
  facet_wrap(~N_val, nrow=1) +
  labs(
    x = TeX("$\\mu$ group"), 
    y = "Mean frequency (p)", 
    color = "Mean \n frequency", 
    fill = "CV",
    title = TeX("$s_h = 0.1$")
  ) + ylim(c(0,1))

p_5 <- plot_04_mu_df |>
  filter(s_h_fct == "s_h = 0.45") |>
  ggplot(aes(x = mu_group, y = p_t_mean, fill = F_cv_group)) +
  geom_boxplot(outliers=FALSE) + 
  geom_point(position = position_jitterdodge(jitter.width=0.15),size=0.45) + 
  facet_wrap(~N_val, nrow=1) +
  labs(
    x = TeX("$\\mu$ group"), 
    y = "Mean frequency (p)", 
    color = "Mean \n frequency", 
    fill = "CV",
    title = TeX("$s_h = 0.45$")
  ) + ylim(c(0,1))

p_3 / p_4 / p_5

combinedPlots <- plot_grid(p_3, p_4, p_5, align = "v", nrow=3, rel_widths = c(1, 1))
combinedPlots

ggsave("~/Desktop/Fig2_mean_p.pdf", combinedPlots, width=12, height=12, dpi=300, useDingbats=FALSE)

