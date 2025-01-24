library(tidyverse)
library(gamlss)
library(gamlss.dist)
library(gamlss.ggplots)
library(broom)

theme_set(theme_bw())

data_df <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

glimpse(data_df)

data_df <- data_df |>
  filter(persist_prop == 1.0) |>
  mutate(N_val = N_val - min(N_val),
         mu_vect = mu_vect - min(mu_vect),
         F_val = F_val_m - min(F_val_m),
         p_t_sd = sqrt(p_t_var))

data_df$mu_groups_fct <- factor(data_df$mu_groups_fct, levels = c("mu_group = 1.0","mu_group = 0.9"))
data_df$s_h_fct <- factor(data_df$s_h_fct, levels = c("s_h = 0.0","s_h = 0.1","s_h = 0.45"))


glimpse(data_df)

data_df |>
  ggplot(aes(x=p_t_mean)) +
  geom_histogram()
data_df |>
  ggplot(aes(x=p_t_sd)) +
  geom_histogram()

p_t_mean_model <- gamlss(p_t_mean ~ N_val + s_h_fct + 
                           mu_vect + mu_groups_fct + mu_groups_fct:mu_vect + 
                           F_val + F_cv_fct + mu_vect:F_val,
                         family = BE,
                         data=data_df)

p_t_sd_model <- gamlss(p_t_sd ~ N_val + s_h_fct + 
                         mu_vect + mu_groups_fct + mu_groups_fct:mu_vect + 
                         F_val + F_cv_fct + mu_vect:F_val,
                       family = BE,
                       data=data_df,
                       control = gamlss.control(n.cyc = 200))

#gen_steps_model <- gamlss(gen_steps ~ N_val + s_h_fct + 
#                            mu_vect + mu_groups_fct + mu_groups_fct:mu_vect + 
#                            F_val + F_cv_fct + mu_groups_fct:F_val,
#                          family = BEINF,
#                          data=data_df,
#                          control = gamlss.control(n.cyc = 50),
#                          method = CG())


p_t_mean_model_results <- summary(p_t_mean_model)
p_t_sd_model_results <- summary(p_t_sd_model)
#gen_steps_model_results <- summary(gen_steps_model)

data.frame(p_t_mean_model_results) |> rownames_to_column()


write_csv(data.frame(p_t_mean_model_results) |> rownames_to_column()
          ,"mu_sims_data/p_t_mean_model_results.csv")
write_csv(data.frame(p_t_sd_model_results) |> rownames_to_column()
          ,"mu_sims_data/p_t_sd_model_results.csv")
#write_csv(data.frame(gen_steps_model_results) |> rownames_to_column()
#          ,"mu_sims_data/gen_steps_model_results.csv")


