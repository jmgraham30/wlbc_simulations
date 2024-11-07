library(tidyverse)
library(gt)

# Load the data
model_mean <- read_csv("data/p_t_mean_model_results.csv")
model_sd <- read_csv("data/p_t_sd_model_results.csv")
model_steps <- read_csv("data/gen_steps_model_results.csv")

# Create a table of the mean model results
model_mean[2:14, ] |>
  gt() |>
  tab_header(
    title = "Model Results for Mean Proportion"
  ) |>
  fmt_scientific() |>
  opt_stylize() |>
  opt_horizontal_padding(scale = 3) 

model_sd[2:14, ] |>
  gt() |>
  tab_header(
    title = "Model Results for Proportion Std. Dev."
  ) |>
  fmt_scientific() |>
  opt_stylize() |>
  opt_horizontal_padding(scale = 3) 

model_steps[2:14, ] |>
  gt() |>
  tab_header(
    title = "Model Results for Number of Generations"
  ) |>
  fmt_scientific() |>
  opt_stylize() |>
  opt_horizontal_padding(scale = 3) 

