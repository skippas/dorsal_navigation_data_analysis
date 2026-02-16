# this script is just to save a plot which i thought was cool looking (tile plot)
# though i probably wont end up using it! and it cluttered the main analysis script

library(tidyverse)
source("scripts//loading_cleaning.R")
source("scripts//custom_themes_and_colour_palettes.R")

bin_trials <- function(
    data, bin_size = 15,
    group_vars = c("individual", "experiment", "manipulation")) {
  data %>%
    # all_of throws error if grouping col not found. across allows for 'dynamic eval' of group vars
    group_by(across(all_of(group_vars))) %>%  
    mutate(
      trial_bin = ceiling(rank_trial / bin_size),
      trial_range = paste0((trial_bin - 1) * bin_size + 1, "-", trial_bin * bin_size)
    ) %>%
    ungroup()
}

choices <- bin_trials(choices)

# tile plot

choices %>%
  filter(manipulation == "test") %>%
  ggplot(aes(y = individual, x = as.integer(rank_trial), fill = as.character(decision))) +
  geom_tile() +
  fill_scale +
  facet_grid(experiment ~ trial_bin, scales = "free") +
  theme_bw()
