library(tidyverse)
library(wlbcmodeler)

theme_set(theme_minimal())

# Load data
abs_change_df <- readRDS("mu_sims_data/data_for_abs_change.rds")

glimpse(abs_change_df)

abs_change_df |>
  ggplot(aes(x=factor(as.character(rep_num),
                      levels=as.character(1:25)),y=max_abs_change)) +
  geom_point(size=2) +
  # add max_abs_change as text to the plot
  geom_text(aes(label=round(max_abs_change, 3)), vjust=-1.0, size=4) +
  labs(x="Replication Number", y="Max Absolute Change") 










calc_abs_changes <- function(a_sim){
  
  abs_change_vct <- numeric(9995)
  
  for (i in 1:9995){
    abs_change_vct[i] <- abs(a_sim[i+4] - a_sim[i])
  }
  
  return(list(abs_change_vct = abs_change_vct))
  
}

# test function
calc_abs_changes(abs_change_df$prop_sim[[1]]) |> unlist() |> max()

# Apply function to all simulations
abs_change_df <- abs_change_df |>
  mutate(abs_change = map(prop_sim, calc_abs_changes),
         max_abs_change = map_dbl(abs_change, ~max(.x$abs_change_vct)))

glimpse(abs_change_df)


saveRDS(abs_change_df, "mu_sims_data/data_for_abs_change.rds")

# Plot
abs_change_df$prop_sim[[1]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[2]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[3]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[4]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[5]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[6]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[7]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[8]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[9]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[10]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[11]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[12]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[13]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[14]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[15]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[16]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[17]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[18]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[19]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[20]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[21]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[22]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[23]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[24]] |>
  plot_a_simulation()
abs_change_df$prop_sim[[25]] |>
  plot_a_simulation()
