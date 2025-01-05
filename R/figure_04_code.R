# load packages
library(tidyverse) # for plots and data manipulation
library(latex2exp) # for LaTeX expressions
library(scales) # for scales
library(forcats) # for factor manipulation
library(ggthemes) # for color theme
library(patchwork) # for combining plots

# set plot theme
theme_set(theme_bw())

# load data
plot_04_mu_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv") 

# glimpse data
glimpse(plot_04_mu_df)

plot_04_mu_df <- plot_04_mu_df |>
  filter(s_h != 0.45, p_t_var > 0.0, persist_prop == 1.0) |>
  mutate(mu_group = factor(mu_groups_fct)) |>
  mutate(mu_group = fct_relevel(mu_group, c("mu_group = 1.0", "mu_group = 0.9"))) |>
  mutate(F_cv_group = factor(F_cv_fct)) |>
  mutate(F_cv_group = fct_relevel(F_cv_group, c("F_cv = 0.0", "F_cv = 0.01", "F_cv = 0.1")))

# glimpse data
glimpse(plot_04_mu_df)

mybreaks_freq <- seq(0, 1, 0.15)

### plot ###
p_0 <- plot_04_mu_df |>
  filter(s_h_fct == "s_h = 0.0") |>
  ggplot(aes(x = mu_group, y = p_t_std_log, fill = F_cv_group)) +
  geom_boxplot(outliers=FALSE) + 
  geom_point(aes(color = p_t_mean), 
             position = position_jitterdodge(jitter.width=0.15),size=0.45) + 
  facet_wrap(~N_val, nrow=1) +
  scale_fill_colorblind() +
  #scale_color_gradient(low = "lightgreen", high = "darkgreen") +
  scale_color_gradientn(
    colours = hcl.colors(length(mybreaks_freq), "viridis", rev = FALSE), 
    breaks = mybreaks_freq
  ) +
  labs(
    x = TeX("$\\mu$ group"), 
    y = "Frequency standard deviation (log)", 
    color = "Mean \n frequency", 
    fill = "CV",
    title = TeX("$s_h = 0.0$")
  ) #+ ylim(c(-3.5,-0.5))

p_1 <- plot_04_mu_df |>
  filter(s_h_fct == "s_h = 0.1") |>
  ggplot(aes(x = mu_group, y = p_t_std_log, fill = F_cv_group)) +
  geom_boxplot(outliers = FALSE) + 
  geom_point(aes(color = p_t_mean), 
             position = position_jitterdodge(jitter.width=0.15),size=0.45) + 
  facet_wrap(~N_val, nrow=1) +
  scale_fill_colorblind() +
  #scale_color_gradient(low = "lightgreen", high = "darkgreen") +
  scale_color_gradientn(
    colours = hcl.colors(length(mybreaks_freq), "viridis", rev = FALSE), 
    breaks = mybreaks_freq
  ) +
  labs(
    x = TeX("$\\mu$ group"), 
    y = "Frequency standard deviation (log)", 
    color = "Mean \n frequency", 
    fill = "CV",
    title = TeX("$s_h = 0.1$")
  ) #+ ylim(c(-3.5,-0.5))


p_0 / p_1

