# Load the necessary package
library(tidyverse)
source("custom_themes_and_colour_palettes.R")
source("loading_cleaning.R")
apis <- choices %>% filter(stimulus == "thick_oblique_apis")

# tile plot to look at decisions across trials per individual
# this will be an underestimate bc I still havent added first 4 trials to datasetv

# reordering by indiv with many trials (54, 44, 431,432, 42, 22, 33, 24)
custom_order <- c("22", "24", "33", "42", "44", "54", "431", "432")
apis$individual <- fct_relevel(apis$individual, custom_order)

apis %>%
  mutate(decision = as.character(decision),
         trial = as.integer(trial),
         decision_within_trial = as.character(decision_within_trial)) %>%
  filter(decision_within_trial == "1") %>%
  ggplot(aes(y = individual, x = trial,
             fill = as.character(decision)))+
  geom_tile()+
  #facet_wrap(facets = "manipulation")
  theme_bw()

#  bar plots: allocating trials into bins of 10 ####
# ranking doesnt work properly if more than one decision per individual per trial
apis <- apis %>%
  group_by(individual) %>%
  mutate(trial_bin = ceiling(rank_trial / 10),
         trial_range = paste0((trial_bin - 1) * 10 + 1, "-", trial_bin * 10))

# Ensure trial ranges are in sequential order
apis <- apis %>%
  mutate(trial_range = factor(trial_range, levels = unique(trial_range[order(trial_bin)])))

# Calculating prop correct and CIs of bins and ordering ####
proportion_data <- apis %>%
  group_by(trial_bin, trial_range, decision) %>%
  summarise(wasp_n = n_distinct(individual),
            cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
  unnest(tst)

proportion_data %>% filter(decision == "1") %>%
  ggplot(aes(x = as.factor(trial_range), y = mean))+
  geom_point(position = position_dodge(width = 0.3), size = 4)+
  geom_line(position = position_dodge(width = 0.3), size = 1.5)+
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.3), width = 0.2)+
  geom_hline(lty = 2, yintercept = 0.5, size = 1.5)+
  # geom_text(aes(label = paste("Nw =", wasp_n), y = 0.45))+
  # geom_text(aes(label = paste("Nt =", total), y = 0.4))+
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::percent)+
  labs(y = "% of choices for reward",
       x = "trial bin")+
  custom_theme_text_enlarged+
  theme(axis.line = element_line(colour = 'black', linewidth = 1.5),
        axis.ticks = element_line(colour = "black", size = 1.5),
        axis.ticks.length=unit(.3, "cm"),
        plot.margin = margin(15,1,5,1),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 20, size = 20, vjust = 0.75, hjust = 0.8),
        legend.position = "none",
        panel.margin = unit(1, "lines"),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line())

# by individual:
prop_data_ind <- apis %>%
  group_by(individual, trial_bin, trial_range, decision) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
  tidyr::unnest(tst) %>%
  mutate(data_type = "individual") #%>%
  #group_by(manipulation) %>%
  #mutate(min = min(total))

prop_data_overall <- apis %>%
  group_by(trial_bin, trial_range, decision) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
  tidyr::unnest(tst) %>%
  mutate(data_type = "overall")

prop_data <- rbind(prop_data_ind, prop_data_overall) 

p <- prop_data %>% filter(decision == 1) %>%
  ggplot(aes(x = trial_range, y = mean, ymin = lower,  ymax = upper))+
  geom_pointrange(data = . %>% filter(data_type == "overall")) +
  geom_pointrange(aes(color = factor(individual)),
                  data = . %>% filter(data_type == "individual"),
                  position = position_jitter(width = 0.2), size = 0.5, alpha = 0.4) +
  # geom_text(data = . %>% filter(data_type == "overall"),
  #           aes(x = manipulation, y = 0.15, label = paste("n =", total)), 
  #           size = 3, vjust = 1, hjust = 0.5, color = "black") +
  # geom_text(data = . %>% filter(data_type == "individual"),
  #           aes(x = manipulation, y = 0.1, label = paste("nmin =", min)), 
  #           size = 3, vjust = 1, hjust = 0.5, color = "black") +
  labs(y = "proportion of choices correct", color = "Individual")+
  geom_hline(yintercept = 0.5, lty = 2)+
  theme_classic()+
  theme(legend.position = "bottom") 

# proportion correct across all trials
decision_counts <- apis %>%
  group_by(decision) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(count, total, method = "wilson"))) %>%
  tidyr::unnest(tst)

p <- decision_counts %>% filter(decision == 1) %>%
  ggplot(aes(x = decision,y = mean))+
  geom_pointrange(aes(ymin = lower, ymax = upper))+
  labs(y = "proportion of decisions correct")+
  geom_hline(yintercept = 0.5, lty = 2)
ggsave("apis_performance_estimates.png", p,
       width = 4, height = 4)

# does rewarding pattern side predict honeybee chosen side? after accounting for 
# individuals and other factors?
