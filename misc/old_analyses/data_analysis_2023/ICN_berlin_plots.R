# ICN berlin plots

# para and perp: ####
proportion_data <- choices %>%
  filter(experiment == "perp_para", trial_group == 'last_n') %>%
  filter( ! (individual %in% c('51', '33', '11') & trial_group == "last_n")) %>% 
  # above filter is removing rows / trials from individuals that had few trials overall and 
  # are these trials are in the last n group, as this biases avg last_n downwards.
  # first n trials for those individuals are kept, as this is still informative
  group_by(trial_group, first) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(cases, total, conf.level=0.95)))) %>%
  tidyr::unnest(tst)

ggplot(proportion_data, aes(x = trial_group, y = estimate,
                            fill = as.character(first)))+ 
  geom_col(position =  position_dodge(), color = "black", size = 1.5)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge2( width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5, size = 1.5)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_x_discrete(expand = c(0.07,0.4))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1), sec.axis = dup_axis(), labels=scales::percent)+
  #facet_wrap(facets = "trial_group", scales = 'free_x', ncol = 4)+
  #labs(y = "Correct / incorrect decision chance")+
  labs(x = NULL, y = NULL)+
  custom_theme_text_enlarged+
  theme(legend.position = "none",
        #panel.border = element_rect(
        #linetype = "solid", colour = "black", size=5, fill = NA),
        axis.line = element_line(colour = 'black', size = 1.5),
        axis.ticks = element_line(colour = "black", size = 1.5),
        axis.ticks.length=unit(.3, "cm"),
        plot.margin = margin(15,1,5,1),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("//Users//andrescheepers//Desktop/berlin_ICN_poster//perp_para_barplot_perf_across_trial_group.png", plot = last_plot(),
       width = 5.5, height = 8, dpi =300)

# thin oblique ####

# reorder individuals so that facet figures of trial groups (particularly inbetween
# trials where trial numbers are different) look neat
max_values <- choices %>%
  group_by(individual) %>%
  summarise(max_value = max(rank_trial)) %>%
  arrange(max_value)
# Reorder the levels of the individual factor based on the maximum values
choices$individual <- factor(choices$individual, levels = max_values$individual)

# Add a new column indicating which trial group the trial belongs to
# is trial within first 15 or last 15 trials
choices <- choices %>%
  group_by(individual, experiment, experiment_type) %>%
  mutate(max_trial = max(rank_trial)) %>%
  ungroup() %>% # Add a new column to denote the trial group
  mutate(trial_group = case_when(
    rank_trial <= 15 ~ "first_n",
    rank_trial > max_trial - 15 ~ "last_n",
    TRUE ~ "inbetween"
  )) 

# change thin_oblique control to a trial group within experiment 
choices <- choices %>% ungroup %>%
  mutate(trial_group = if_else(experiment_type == 'control', 'control', trial_group))

# compare first n of test to last n of test and to the first ~15 of control
# reset trial numbering within trial_groups: first 15 group, inbetween trials, and last 15 group
choices <- choices %>% 
  group_by(experiment, individual, trial_group) %>% 
  arrange(rank_trial) %>% 
  mutate(rank_win_group = 1:n()) %>% 
  ungroup()

# reorder trial_groups for the facetted plot to aid comparison. early trials -> later, L -> R
choices$trial_group = factor(choices$trial_group,
                             levels=c('first_n','inbetween', 'last_n', 'control'))

# Tile plot: individuals' performance across different trial groups
# (1st 15, last 15 and control) for thin_oblique exp
choices %>% filter(experiment == "thin_oblique", trial_group != 'inbetween') %>%
  filter(individual %in% c('51', '2', '32', '55')) %>%
  ggplot(aes(y = individual, x = rank_win_group, fill = as.character(first)))+
  geom_tile()+
  fill_scale+
  facet_wrap(facets = "trial_group",  ncol = 4, scales = "free_x")
ggsave("figures//thin_oblique_tileplot_perf_across_individuals_and_trial_group.png",
       plot = last_plot(), width = 8, height = 8, dpi =300)

# Calculate the proportion of correct decisions for each trial group and CIs
proportion_data <- choices %>%
  filter(experiment == "thin_oblique", trial_group == 'control') %>%
  group_by(trial_group, first) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(cases, total, conf.level=0.95)))) %>%
  tidyr::unnest(tst)

# Barplot with CIs: proportion correct / incorrect for different trial groups 
# averaged across individuals
proportion_data %>% filter(first == "0") %>%
  ggplot(aes(x = trial_group, y = estimate,
             fill = as.character(first)))+ 
  geom_col(position =  position_dodge(0.94), width = 0.9, color = "black", size = 1.5)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .33,
                position = position_dodge2(width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5, size = 1.5)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_x_discrete(expand = c(0.08,0.4))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1),
                     labels=scales::percent, position =  "left")+
  #facet_wrap(facets = "trial_group", scales = 'free_x', ncol = 4)+
  #labs(y = "Correct / incorrect decision chance")+
  labs(x = NULL, y = NULL)+
  custom_theme_text_enlarged+
  theme(legend.position = "none",
        #panel.border = element_rect(
        #linetype = "solid", colour = "black", size=5, fill = NA),
        axis.line = element_line(colour = 'black', size = 1.5),
        axis.ticks = element_line(colour = "black", size = 1.5),
        axis.ticks.length=unit(.3, "cm"),
        plot.margin = margin(15,1,5,1),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("//Users//andrescheepers//Desktop/berlin_ICN_poster//thin_oblique_barplot_perf_control_incorrect.png", plot = last_plot(),
       width = 3.2, height = 8, dpi =300)


#  plots for nat can experiment ####

# Tile plot: individuals' performance across different trial groups
# (1st 15, last 15 and control) for nat can exp
choices %>% filter(experiment == "nat_can") %>%
  #filter(trial_group != 'inbetween') %>%
  filter(! individual %in% c("35", "11")) %>%
  ggplot(aes(y = individual, x = rank_win_group, fill = as.character(first)))+
  geom_tile()+
  fill_scale+
  facet_wrap(facets = "trial_group",  ncol = 4, scales = "free_x")+
  theme_classic()
ggsave("figures//perp_para_tileplot_perf_across_individuals_and_trial_group.png",
       plot = last_plot(), width = 8, height = 8, dpi =300)

# its evident from this figure that 33, 11 and even 51 can be excluded from the figure,
# not enough trials. probably still informative for the logistic regression however
# What is also evident is that the logistic regression for perp para is going to be based mainly
# on one individual in later trials, which perhaps hampers conclusions drawn from comparing
# learning slopes between experiments

# Calculate the proportion of correct decisions for each trial group and CIs
proportion_data <- choices %>%
  filter(experiment == "nat_can", trial_group != 'inbetween') %>%
  filter( ! (individual %in% c('35', '11') & trial_group == "last_n")) %>% 
  # above filter is removing rows / trials from individuals that had few trials overall and 
  # are these trials are in the last n group, as this biases avg last_n downwards.
  # first n trials for those individuals are kept, as this is still informative
  group_by(trial_group, first) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(cases, total, conf.level=0.95)))) %>%
  tidyr::unnest(tst)

# Barplot with CIs: proportion correct / incorrect for different trial groups 
# averaged across individuals
proportion_data %>% 
  ggplot(aes(x = trial_group, y = estimate,
             fill = as.character(first)))+ 
  geom_col(position =  position_dodge(0.94), width = 0.9, color = "black", size = 1.5)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge2(width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5, size = 1.5)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_x_discrete(expand = c(0.08,0.4))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1),
                     labels=scales::percent, position =  "left")+
  #facet_wrap(facets = "trial_group", scales = 'free_x', ncol = 4)+
  #labs(y = "Correct / incorrect decision chance")+
  labs(x = NULL, y = NULL)+
  custom_theme_text_enlarged+
  theme(legend.position = "none",
        #panel.border = element_rect(
        #linetype = "solid", colour = "black", size=5, fill = NA),
        axis.line = element_line(colour = 'black', size = 1.5),
        axis.ticks = element_line(colour = "black", size = 1.5),
        axis.ticks.length=unit(.3, "cm"),
        plot.margin = margin(15,1,5,1),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )
ggsave("//Users//andrescheepers//Desktop/berlin_ICN_poster//natcan_perf_across_trial_group.png",
       plot = last_plot(),
       width = 6, height = 8, dpi =300)

# thick_stripe exp ####

# Tile plot: individuals' performance across different trial groups
# (1st 15, last 15 and control) for thin_oblique exp
choices %>% filter(experiment == "thick_oblique") %>%
  filter(trial_group != 'inbetween') %>%
  filter(! individual %in% c('33', '11')) %>%
  ggplot(aes(y = individual, x = rank_win_group, fill = as.character(first)))+
  geom_tile()+
  fill_scale+
  facet_wrap(facets = "trial_group",  ncol = 4, scales = "free_x")
ggsave("figures//thick_oblique_tileplot_perf_across_individuals_and_trial_group.png",
       plot = last_plot(), width = 8, height = 8, dpi =300)


# its evident from this figure that 33, 11 and even 51 can be excluded from the figure,
# not enough trials. probably still informative for the logistic regression however
# What is also evident is that the logistic regression for perp para is going to be based mainly
# on one individual in later trials, which perhaps hampers conclusions drawn from comparing
# learning slopes between experiments

# Calculate the proportion of correct decisions for each trial group and CIs
proportion_data <- choices %>%
  filter(experiment == "thick_oblique", trial_group != 'inbetween') %>%
  filter( ! (individual %in% c('33', '11') & trial_group == "last_n")) %>% 
  # above filter is removing rows / trials from individuals that had few trials overall and 
  # are these trials are in the last n group, as this biases avg last_n downwards.
  # first n trials for those individuals are kept, as this is still informative
  group_by(trial_group, first) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(cases, total, conf.level=0.95)))) %>%
  tidyr::unnest(tst)

# Barplot with CIs: proportion correct / incorrect for different trial groups 
# averaged across individuals
proportion_data %>% 
  ggplot(aes(x = trial_group, y = estimate,
             fill = as.character(first)))+ 
  geom_col(position =  position_dodge(0.94), width = 0.9, color = "black", size = 1.5)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge2(width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5, size = 1.5)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_x_discrete(expand = c(0.08,0.4))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1),
                     labels=scales::percent, position =  "left")+
  #facet_wrap(facets = "trial_group", scales = 'free_x', ncol = 4)+
  #labs(y = "Correct / incorrect decision chance")+
  labs(x = NULL, y = NULL)+
  custom_theme_text_enlarged+
  theme(legend.position = "none",
        #panel.border = element_rect(
        #linetype = "solid", colour = "black", size=5, fill = NA),
        axis.line = element_line(colour = 'black', size = 1.5),
        axis.ticks = element_line(colour = "black", size = 1.5),
        axis.ticks.length=unit(.3, "cm"),
        plot.margin = margin(15,1,5,1),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )
ggsave("//Users//andrescheepers//Desktop/berlin_ICN_poster//thick_stripe_perf_across_trial_group.png",
       plot = last_plot(),
       width = 6, height = 8, dpi =300)
