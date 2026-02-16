# Thin oblique plots ####

# reorder individuals so that facet figures of trial groups (particularly inbetween
# trials where trial numbers are different) look neat
max_values <- choices %>%
  group_by(individual) %>%
  summarise(max_value = max(rank_trial)) %>%
  arrange(max_value)
# Reorder the levels of the individual factor based on the maximum values
choices$individual <- factor(choices$individual, levels = max_values$individual)
# below can be used to reassign sequential numerical codes for individuals
# choices <- choices %>%
#  mutate(individual = match(individual, unique(individual)))

# note that there was a training type (this is an old comment, what does it mean, when were the training types?!)
# and finally, some individuals came into experiment later and may have missed training type?
# Where did the blocks occur? This might affect learning.
# remove illegible handwriting.

# performance across trials within experiments
# thin stripe test and control

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
  filter(experiment == "thin_oblique", trial_group != 'inbetween') %>%
  group_by(trial_group, first) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(cases, total, conf.level=0.95)))) %>%
  tidyr::unnest(tst)

# Barplot with CIs: proportion correct / incorrect for different trial groups 
# averaged across individuals
ggplot(proportion_data, aes(x = trial_group, y = estimate, fill = as.character(first)))+ 
  geom_col(position =  position_dodge())+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge2( width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_y_continuous(labels=scales::percent)+
  theme_classic()+
  facet_wrap(facets = "trial_group", scales = 'free_x', ncol = 4)
ggsave("figures//thin_oblique_barplot_perf_trial_group.png",
       plot = last_plot(), width = 8, height = 8, dpi =300)

# perhaps plot trial groups separately for easier manipulation in inkscape

# Another way of visualizing which individuals' trials are used in trial_group barplots ####
# is by highlighting the groups with grey rectangles
choices_sub <- choices %>% filter(experiment == "thin_oblique",
                                  individual %in% c('51', '2', '32', '55'))
ggplot(choices_sub, aes(y = individual, x = rank_trial, fill = as.character(first)))+
  geom_tile()+
  fill_scale+
  facet_wrap(facets = "experiment",  ncol = 2, scales = "free_y")+
  # Add shaded areas for first n and last n trials. Could add lineranges instead
  geom_rect(data = choices_sub %>% filter(rank_trial <= 15),
            aes(xmin = rank_trial - 0.5, xmax = rank_trial + 0.5,
                ymin = as.numeric(factor(individual)) - 0.5,
                ymax = as.numeric(factor(individual)) + 0.5),
            fill = "grey30", alpha = 0.5, inherit.aes = FALSE) +
  geom_rect(data = choices_sub %>% group_by(individual) %>%
              filter(rank_trial > max(rank_trial) - 15),
            aes(xmin = rank_trial - 0.5, xmax = rank_trial + 0.5,
                ymin = as.numeric(factor(individual)) - 0.5,
                ymax = as.numeric(factor(individual)) + 0.5),
            fill = "grey30", alpha = 0.5, inherit.aes = FALSE)

# repeat plots for thick_stripe exp ####

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
  # I'm removing particular individuals from last_n because this might bias last_n downwards.
  # What am I actually trying to do with last_n? Give a representation of how well the wasps 
  # can do the tasks after a significant number of trials? To simplify this,
  # Could I just look at the perf after n_trials instead? I guess that would depend on
  # whether there are differences in learning speed or something?
  filter( ! (individual %in% c('33', '11') & trial_group == "last_n")) %>%  
  # first n trials for those individuals are kept, as this is still informative
  group_by(trial_group, first) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(cases, total, conf.level=0.95)))) %>%
  tidyr::unnest(tst)

# Barplot with CIs: proportion correct / incorrect for different trial groups 
# averaged across individuals
ggplot(proportion_data, aes(x = trial_group, y = estimate, fill = as.character(first)))+ 
  geom_col(position =  position_dodge())+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge2( width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_y_continuous(labels=scales::percent)+
  theme_classic()+
  facet_wrap(facets = "trial_group", scales = 'free_x', ncol = 4)
ggsave("figures//thick_oblique_barplot_perf_trial_group.png",
       plot = last_plot(), width = 8, height = 8, dpi =300)



```

repeat plots for perp_para exp
```{r}
# repeat above tile and bar plots for perp_para exp

# Tile plot: individuals' performance across different trial groups
# (1st 15, last 15 and control) for thin_oblique exp
choices %>% filter(experiment == "perp_para") %>%
  filter(trial_group != 'inbetween') %>%
  filter(! individual %in% c('51', '33', '11')) %>%
  ggplot(aes(y = individual, x = rank_win_group, fill = as.character(first)))+
  geom_tile()+
  fill_scale+
  facet_wrap(facets = "trial_group",  ncol = 4, scales = "free_x")+
  theme_classic()
ggsave("figures//perp_para_tileplot_perf_across_individuals_and_trial_group.png",
       plot = last_plot(), width = 8, height = 4, dpi =300)

# its evident from this figure that 33, 11 and even 51 can be excluded from the figure,
# not enough trials. probably still informative for the logistic regression however
# What is also evident is that the logistic regression for perp para is going to be based mainly
# on one individual in later trials, which perhaps hampers conclusions drawn from comparing
# learning slopes between experiments

# Calculate the proportion of correct decisions for each trial group and CIs
proportion_data <- choices %>%
  filter(experiment == "perp_para", trial_group != 'inbetween') %>%
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

# Barplot with CIs: proportion correct / incorrect for different trial groups 
# averaged across individuals
ggplot(proportion_data, aes(x = trial_group, y = estimate,
                            fill = as.character(first)))+ 
  geom_col(position =  position_dodge())+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge2( width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5, size = 0.8)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_y_continuous(sec.axis = dup_axis(), labels=scales::percent)+
  facet_wrap(facets = "trial_group", scales = 'free_x', ncol = 4)+
  labs(y = "Correct / incorrect decision chance")+
  custom_theme_text_enlarged
ggsave("figures//perp_para_barplot_perf_across_trial_group.png", plot = last_plot(),
       width = 8, height = 8, dpi =300)

# Add in canopy exp data, for Megalopta

# repeat plots for nat_can exp ####

# repeating plots for nat can experiment

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
ggplot(proportion_data, aes(x = trial_group, y = estimate,
                            fill = as.character(first)))+ 
  geom_col(position =  position_dodge())+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge2( width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5, size = 0.8)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(facets = "trial_group", scales = 'free_x', ncol = 4)+
  labs(y = "Correct / incorrect decision chance")+
  custom_theme_text_enlarged+
  theme(legend.position = NULL)
ggsave("figures//perp_para_barplot_perf_across_trial_group.png", plot = last_plot(),
       width = 8, height = 8, dpi =300)

