library(tidyverse)

# Load the data
no_ci_df <- read_csv("mu_sims_data/no_ci_sims_stats.csv") |>
  filter(N_val != 1000 | mu_vect != 0.001 | bin_props != 1.0)
ci_01_df <- read_csv("mu_sims_data/with_ci_01_sims_stats.csv") |>
  filter(N_val != 1000 | mu_vect != 0.001 | bin_props != 1.0)
ci_45_df <- read_csv("mu_sims_data/with_ci_45_sims_stats.csv") |>
  filter(N_val != 1000 | mu_vect != 0.001 | bin_props != 1.0)

no_ci_df_oof <- read_csv("mu_sims_data/no_ci_sims_stats_over_one.csv")
ci_01_df_oof <- read_csv("mu_sims_data/with_ci_01_sims_stats_over_one.csv")
ci_45_df_oof <- read_csv("mu_sims_data/with_ci_45_sims_stats_over_one.csv")

no_ci_df <- rbind(no_ci_df, no_ci_df_oof)
ci_01_df <- rbind(ci_01_df, ci_01_df_oof)
ci_45_df <- rbind(ci_45_df, ci_45_df_oof)

# Glimpse data
glimpse(no_ci_df)
glimpse(ci_01_df)
glimpse(ci_45_df)


# Compute and add persistence proportions column
no_ci_pp <- no_ci_df |>
  group_by(F_val_m,F_cv,mu_vect,bin_props,s_h,N_val) |>
  mutate(persist_bool = ifelse(gen_steps == 10000,1,0)) |>
  summarise(persist_count = sum(persist_bool), n = n(), persist_prop = persist_count/n) |>
  select(-c(n,persist_count))

ci_01_pp <- ci_01_df |>
  group_by(F_val_m,F_cv,mu_vect,bin_props,s_h,N_val) |>
  mutate(persist_bool = ifelse(gen_steps == 10000,1,0)) |>
  summarise(persist_count = sum(persist_bool), n = n(), persist_prop = persist_count/n) |>
  select(-c(n,persist_count))

ci_45_pp <- ci_45_df |>
  group_by(F_val_m,F_cv,mu_vect,bin_props,s_h,N_val) |>
  mutate(persist_bool = ifelse(gen_steps == 10000,1,0)) |>
  summarise(persist_count = sum(persist_bool), n = n(), persist_prop = persist_count/n) |>
  select(-c(n,persist_count))

# Glimpse data
glimpse(no_ci_pp)
glimpse(ci_01_pp)
glimpse(ci_45_pp)

# Add persistence proportions to the original data
no_ci_df <- no_ci_df |> 
  left_join(no_ci_pp, by = c("F_val_m","F_cv","mu_vect","bin_props","s_h","N_val"))

ci_01_df <- ci_01_df |>
  left_join(ci_01_pp, by = c("F_val_m","F_cv","mu_vect","bin_props","s_h","N_val"))

ci_45_df <- ci_45_df |>
  left_join(ci_45_pp, by = c("F_val_m","F_cv","mu_vect","bin_props","s_h","N_val"))

# Glimpse data
glimpse(no_ci_df)
glimpse(ci_01_df)
glimpse(ci_45_df)


# Remove observations with missing values
no_ci_df <- no_ci_df |> drop_na()
ci_01_df <- ci_01_df |> drop_na()
ci_45_df <- ci_45_df |> drop_na()

# Glimpse data
glimpse(no_ci_df)
glimpse(ci_01_df)
glimpse(ci_45_df)

# Combine the data
wlbc_simulations_results_df <- bind_rows(
  no_ci_df,
  ci_01_df,
  ci_45_df
)

# Glimpse data
glimpse(wlbc_simulations_results_df)

# Factorize F_cv, bin_props, and s_h
wlbc_simulations_results_df <- wlbc_simulations_results_df |>
  mutate(F_cv_fct = ifelse(F_cv == 0.0, "F_cv = 0.0",
                           ifelse(F_cv == 0.01, "F_cv = 0.01", "F_cv = 0.1")),
         mu_groups_fct = ifelse(bin_props == 1.0, "mu_group = 1.0", "mu_group = 0.9"),
         s_h_fct = ifelse(s_h == 0.0, "s_h = 0.0", ifelse(s_h == 0.1,"s_h = 0.1","s_h = 0.45")))

# Glimpse data
glimpse(wlbc_simulations_results_df)

wlbc_simulations_results_df <- wlbc_simulations_results_df |>
  mutate(p_t_std = sqrt(p_t_var),
         p_t_std_log = log10(p_t_std),
         fpop_scale = round(final_p_t * N_val),
         rn = 1:nrow(wlbc_simulations_results_df)) |> 
  filter(persist_prop > 0.0)

# Glimpse data
glimpse(wlbc_simulations_results_df)


wlbc_simulations_results_df <- wlbc_simulations_results_df |>
  filter(fpop_scale > 20) 
#  |>
#  filter(gen_steps < 10000 | fpop_scale > 16)

# Glimpse data
glimpse(wlbc_simulations_results_df)

# Save the data
write_csv(wlbc_simulations_results_df, "mu_sims_data/wlbc_simulations_results.csv")
