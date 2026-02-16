# ive removed this from the main analysis script to declutter it. I think there's
# better ways to analyse / compare experiment performance than this.
# One such way is just to fit models and make the comparison at different trial numbers

rm(list = ls())
source("scripts//loading_cleaning.R")
source("scripts//custom_themes_and_colour_palettes.R")

# comparing the last n trials of different experiments

# just looking at prop correct over the last n trials of test phase
source("functions//subset_lastn_trials.R")
last_15_trials <- subset_last_n_trials(choices, trial_variable = "rank_trial")
source("functions//calc_cis.R")
last_15_trials_prop <- calc_cis(last_15_trials, group_vars = c("experiment"))

# bring in the nat or can variable into the df
nat_or_art <- unique(choices[, c("experiment", "nat_or_art")])
last_15_trials_prop <- left_join(last_15_trials_prop, nat_or_art,
                                 by = "experiment", keep = F)

x <- last_15_trials_prop %>%
  ggplot(aes(x = nat_or_art,
             y = mean,
             group = experiment))+
  geom_point(position = position_dodge(width = 0.5), size = 3)+
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.5),
                width = 0.2)+
  geom_hline(lty = 2, yintercept = 0.5)+
  scale_y_continuous(labels = scales::percent)+
  labs(y = "% choices for reward in last 15 trials",
       x = "Stimuli")+
  theme_classic()+
  theme(legend.position = "none")
ggsave("plots//naturalistic_vs_artificial//nat_vs_art_last_n.png",
       plot = x, width = 3, height = 4.5, dpi = 300)

