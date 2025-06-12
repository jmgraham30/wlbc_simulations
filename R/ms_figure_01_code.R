# load packages
library(tidyverse) # for plots and data manipulation
library(latex2exp) # for LaTeX expressions
library(scales) # for scales
library(forcats) # for factor manipulation
library(wlbcmodeler) # for wolbachia model functions

# set plot theme
theme_set(theme_bw())


# use the `no_ci` function to compute the stable equilibrium values
# in case of no CI whenever F (1 - mu) > 1

# build F and mu ranges
F_val <- seq(1.0, 1.5, by = 0.02)
mu_val <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2,
            0.25, 0.3, 0.35, 0.4)
F_mu_df <- expand_grid(F_val = F_val, mu_val = mu_val)
sh_val <- c(0.1,0.45)
F_mu_ci_df <- expand_grid(F_val = F_val, mu_val = mu_val, sh_val = sh_val)

# compute the stable equilibrium values
F_mu_df <- F_mu_df |>
  mutate(
    p_ast = map2_dbl(F_val, mu_val, ~no_ci(F = .x, mu = .y))
  )

F_mu_ci_df$p_ast <- pmap_dbl(F_mu_ci_df, ci_stable)

# check results
glimpse(F_mu_df)
glimpse(F_mu_ci_df)

############### CREATE PLOTS FOR FIGURE 1 ################

# breaks for the color scale
mybreaks <- seq(0, 1, 0.15)

# plot the stable equilibrium values in no CI case
F_mu_df |>
  filter(!is.na(p_ast)) |> # remove NA values
  ggplot(aes(y = F_val, x = as.character(mu_val), fill = p_ast)) +
  geom_tile() +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks), "viridis", rev = FALSE), 
    breaks = mybreaks
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
  ) +
  #  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = TeX("$\\mu$"),
    y = TeX("$F$"),
    fill = TeX("Equilibrium \n frequency"),
    title = TeX("$s_h = 0.0$")
  )

# plot the stable equilibrium values for CI case with s_h = 0.1
F_mu_ci_df |>
  filter(!is.na(p_ast),p_ast >= 0.0,sh_val == 0.1) |> # remove NA values
  ggplot(aes(y = F_val, x = as.character(mu_val), fill = p_ast)) +
  geom_tile() +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks), "viridis", rev = FALSE), 
    breaks = mybreaks
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
  ) +
  #  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = TeX("$\\mu$"),
    y = TeX("$F$"),
    fill = TeX("Equilibrium \n frequency"),
    title = TeX("$s_h = 0.1$")
  )

# plot the stable equilibrium values for CI case with s_h = 0.45
F_mu_ci_df |>
  filter(!is.na(p_ast),p_ast >= 0.0,sh_val == 0.45) |> # remove NA values
  ggplot(aes(y = F_val, x = as.character(mu_val), fill = p_ast)) +
  geom_tile() +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks), "viridis", rev = FALSE), 
    breaks = mybreaks
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
  ) +
  #  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = TeX("$\\mu$"),
    y = TeX("$F$"),
    fill = TeX("Equilibrium \n frequency"),
    title = TeX("$s_h = 0.45$")
  )

