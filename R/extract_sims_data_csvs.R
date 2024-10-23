library(tidyverse)

data_check <- readRDS("mu_sims_data/no_ci_sims.rds")

no_ci_df <- data_check |>
  select(-prop_sim)

glimpse(no_ci_df)

write_csv(no_ci_df,"mu_sims_data/no_ci_sims_stats.csv")

data_check <- readRDS("mu_sims_data/with_ci_sims_01.rds")

with_ci_01_df <- data_check |>
  select(-prop_sim)

glimpse(with_ci_01_df)

write_csv(with_ci_01_df,"mu_sims_data/with_ci_01_sims_stats.csv")
