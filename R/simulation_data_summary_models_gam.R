library(tidyverse)
library(mgcv)
library(marginaleffects)
library(forcats)

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

p_t_mean_model_l <- bam(p_t_mean ~ N_val + s_h_fct + 
                          mu_vect + mu_groups_fct + mu_groups_fct:mu_vect + 
                          F_val + F_cv_fct,
                        family = betar,
                        data=data_df,
                        method="REML")

p_t_sd_model_l <- bam(p_t_sd ~ N_val + s_h_fct + 
                        mu_vect + mu_groups_fct + mu_groups_fct:mu_vect + 
                        F_val + F_cv_fct,
                      family = betar,
                      data=data_df,
                      method="REML")


plot_predictions(p_t_mean_model_l,condition = c("mu_vect"))
plot_predictions(p_t_mean_model_l,condition = c("F_val"))
plot_predictions(p_t_sd_model_l,condition = c("mu_vect"))
plot_predictions(p_t_sd_model_l,condition = c("F_val"))

plot_predictions(p_t_mean_model_l,condition = c("mu_vect","F_val","s_h_fct","mu_groups_fct"))
plot_predictions(p_t_sd_model_l,condition = c("mu_vect","F_val","s_h_fct","mu_groups_fct"))

p_t_mean_preds <- plot_predictions(p_t_mean_model_l,
                                   condition = c("F_val","mu_vect","s_h_fct","mu_groups_fct"),
                                   draw = FALSE)

p_t_sd_preds <- plot_predictions(p_t_sd_model_l,
                                   condition = c("F_val","mu_vect","s_h_fct","mu_groups_fct"),
                                   draw = FALSE)

p_t_mean_preds <- p_t_mean_preds |>
  mutate(mu_vect = fct_recode(mu_vect, "0" = "0.000",
                              "0.005" = "0.004",
                              "0.05" = "0.049",
                              "0.1" = "0.099",
                              "0.3" = "0.299"))

p_t_sd_preds <- p_t_sd_preds |>
  mutate(mu_vect = fct_recode(mu_vect, "0" = "0.000",
                              "0.005" = "0.004",
                              "0.05" = "0.049",
                              "0.1" = "0.099",
                              "0.3" = "0.299"))

write_csv(p_t_mean_preds,"mu_sims_data/p_t_mean_preds.csv")
write_csv(p_t_sd_preds,"mu_sims_data/p_t_sd_preds.csv")

p_t_mean_model_n <- bam(p_t_mean ~ N_val + s_h_fct + 
                          s(mu_vect,k=3,bs='cr') + mu_groups_fct + s(mu_vect,by=mu_groups_fct,k=3,bs='cr') + 
                          s(F_val,k=3,bs='cr') + F_cv_fct,
                         family = betar,
                         data=data_df,
                      method="REML")

p_t_sd_model_n <- bam(p_t_sd ~ N_val + s_h_fct + 
                        s(mu_vect,k=3,bs='cr') + mu_groups_fct + s(mu_vect,by=mu_groups_fct,k=3,bs='cr') + 
                        s(F_val,k=3,bs='cr') + F_cv_fct,
                    family = betar,
                    data=data_df,
                    method="REML")



plot_predictions(p_t_mean_model_n,condition = c("mu_vect"))
plot_predictions(p_t_mean_model_n,condition = c("F_val"))
plot_predictions(p_t_sd_model_n,condition = c("mu_vect"))
plot_predictions(p_t_sd_model_n,condition = c("F_val"))

plot_predictions(p_t_mean_model_n,condition = c("F_val","mu_vect","s_h_fct","mu_groups_fct"))
plot_predictions(p_t_sd_model_n,condition = c("F_val","mu_vect","s_h_fct","mu_groups_fct"))
                 