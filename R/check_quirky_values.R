library(tidyverse)
library(ggthemes)
library(wlbcmodeler)
theme_set(theme_minimal(base_size = 12))



# Load data
sim_res <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

glimpse(sim_res)

sim_res <- sim_res |>
  mutate(fpop_scale = round(final_p_t * N_val))

range(sim_res$fpop_scale)

sim_res |> ggplot(aes(x=log10(fpop_scale))) + 
  geom_histogram(binwidth = 1,fill="lightblue",color="white") +
  geom_vline(xintercept = 1, color = "red")

sim_res <- sim_res |> filter(fpop_scale >= 10)

sim_res_b <- sim_res |>
  filter(F_val_m == 1, s_h == 0) |>
  mutate(bin_props_c = 1 - bin_props,
         mu_vect_c = 0.8,
         p_star_criterion = "No")

glimpse(sim_res_b)

sim_res_s_h_0 <- sim_res |>
  filter(F_val_m > 1, s_h == 0) |>
  mutate(bin_props_c = 1 - bin_props,
         mu_vect_c = 0.8,
         p_star = 1 - (F_val_m/(F_val_m - 1))*(mu_vect*bin_props + mu_vect_c*bin_props_c),
         p_star_sgn = as.character(sign(p_star)),
         p_star_criterion = ifelse(p_star_sgn == "-1","No","Yes")) |>
  select(-c(p_star,p_star_sgn))

glimpse(sim_res_s_h_0)

sim_res_s_h_p <- sim_res |>
  filter(s_h > 0) |>
  mutate(bin_props_c = 1 - bin_props,
         mu_vect_c = 0.8,
         p_star = F_val_m*(1 - (mu_vect*bin_props + mu_vect_c*bin_props_c)),
         p_star_criterion = ifelse(p_star > 1,"Yes","No")) |>
  select(-c(p_star))

glimpse(sim_res_s_h_p)

sim_res <- bind_rows(sim_res_b, sim_res_s_h_0, sim_res_s_h_p)

glimpse(sim_res)

sim_res |>
  #filter(p_star_criterion == "No") |>
  ggplot(aes(x=p_t_mean,y=log10(sqrt(p_t_var)),color=F_val_m)) + 
  facet_grid(s_h_fct~mu_groups_fct + F_cv_fct) +
  geom_jitter(alpha=0.5) + 
  labs(color="mean F") +
  #scale_color_colorblind() +
  xlim(c(0,1))

sim_res |>
  filter(bin_props == 1.0, s_h == 0.0, F_cv == 0.1, p_t_mean > 0.0, p_star_criterion == "No") |> View()

infection_freq_rf_rmu_iteration(1.0,0.1,c(0.001,0.0),c(1.0,0.0),10000) |>
  plot_a_simulation() + ylim(c(0,1))




# Load data
sim_res <- read_csv("mu_sims_data/wlbc_simulations_results.csv")

sim_res <- sim_res |>
  filter(F_val_m > 1) |>
  mutate(bin_props_c = 1 - bin_props,
         mu_vect_c = 0.8,
         turr_cond = F_val_m*(1 - (mu_vect*bin_props + mu_vect_c*bin_props_c)),
         tc_met = ifelse(turr_cond_1 > 1, "Yes", "No"),
         p_star = 1 - (F_val_m/(F_val_m - 1))*(mu_vect*bin_props + mu_vect_c*bin_props_c),
         p_star_sgn = as.character(sign(p_star)),
         p_star_criterion = ifelse(p_star_sgn == "-1","No","Yes"))

glimpse(sim_res)

sim_res |>
  ggplot(aes(x=turr_cond_1,y=turr_cond_2,color=tc_met)) +
  geom_point() + facet_wrap(~mu_groups_fct) + 
  scale_color_colorblind()

sim_res |>
  ggplot(aes(x=p_t_mean,y=log10(sqrt(p_t_var)),color=p_star_sgn)) + 
  facet_grid(s_h_fct~tc_met) +
  geom_jitter(alpha=0.5) + 
  scale_color_colorblind() +
  xlim(c(0,1))

sim_res_1 <- sim_res |>
  filter(s_h == 0, bin_props < 1, F_val_m > 1) |>
  mutate(p_star = 1 - (F_val_m/(F_val_m - 1))*(mu_vect*bin_props + mu_vect_c*bin_props_c))

glimpse(sim_res_1)

sim_res_2 <- sim_res |>
  filter(s_h > 0, bin_props < 1, F_val_m > 1) |>
  mutate(p_star = (s_h+1-F_val_m - sqrt((s_h+1-F_val_m)^2 - 4*s_h*(F_val_m*(1 - mu_vect*bin_props + mu_vect_c*bin_props_c) - 1)*(1 - F_val_m*(mu_vect*bin_props + mu_vect_c*bin_props_c))))/(2*s_h*(1 - F_val_m*(mu_vect*bin_props + mu_vect_c*bin_props_c))))

glimpse(sim_res_2)

sim_res <- bind_rows(sim_res_1, sim_res_2)

sim_res <- sim_res |>
  mutate(p_star_neg = ifelse(p_star < 0 | is.nan(p_star), "Yes", "No"))

glimpse(sim_res)

sim_res |>
  ggplot(aes(x=p_t_mean,y=log10(sqrt(p_t_var)),color=p_star_neg)) + 
  facet_wrap(~s_h_fct) +
  geom_jitter(alpha=0.3) + 
  scale_color_colorblind() +
  xlim(c(0,1))
