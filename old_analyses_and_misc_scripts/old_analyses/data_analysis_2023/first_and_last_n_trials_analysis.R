# grouping by first n and last n trials comparing prop correct across different treatments
rm(list = ls())
source("loading_cleaning.R")
source("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/projects/wasp project/analysis/custom_themes_and_colour_palettes.R")

# note that there was a training phase (this is an old comment, what does it mean, when were the training phase?!)
# and finally, some individuals came into experiment later and may have missed training phase?
# Where did the blocks occur? This might affect learning.

# assigning trials to different groups and reordering data ####
# Add a new column indicating which trial group the trial belongs to
# first n, last n, and inbetween. test and control
choices <- choices %>%
  group_by(individual, experiment, experiment_type) %>%
  mutate(max_trial = max(rank_trial)) %>%
  ungroup() %>% # Add a new column to denote the trial group
  mutate(trial_group = if_else(
    experiment_type == "test", case_when(
      rank_trial <= 15 ~ "first_n",
      rank_trial > max_trial - 40 ~ "last_n",
      TRUE ~ "inbetween"),
    "control")
  ) 

# So that tile plots can be neatly displayed, reset trial numbering within trial_groups:
choices <- choices %>% 
  group_by(experiment, individual, trial_group) %>% 
  arrange(rank_trial) %>% 
  mutate(rank_win_group = 1:n()) %>% 
  ungroup()
# reorder trial_groups for the facetted plot to aid comparison. early trials -> later, L -> R
choices$trial_group = factor(choices$trial_group,
                             levels=c('first_n','inbetween', 'last_n', 'control'))

# reorder individuals so that facet figures of trial groups (particularly inbetween
# trials where trial numbers are different) look neat. Below solution from T rinkers blog: https://trinkerrstuff.wordpress.com/2016/12/23/ordering-categories-within-ggplot2-facets
choices <- choices %>%
  group_by(experiment, individual) %>%
  mutate(max_value = max(rank_trial)) %>%
  arrange(max_value) %>% 
  ungroup() %>%
  mutate(x = factor(paste(individual, experiment, sep = "__"), 
                    levels = unique(paste(individual, experiment, sep = "__"))))
# could reassign sequential indiv numbers to limit cognitive load of figures

# Tile plot: individuals' perf across the different trial groups (first_n, last_n, inbetw, control) ####
choices %>%
  # to filter specific exps / trial groups / indivs, use below lines:
  # filter(experiment == "thin_oblique", trial_group != 'inbetween') %>% 
  # filter(individual %in% c('51', '2', '32', '55')) %>%
  ggplot(aes(y = x, x = rank_win_group, fill = as.character(first)))+
  geom_tile()+
  fill_scale+
  facet_grid(experiment ~ trial_group, scales = "free")+
  theme_bw()
# ggsave("figures//thin_oblique_tileplot_perf_across_individuals_and_trial_group.png",
#        plot = last_plot(), width = 8, height = 8, dpi =300)

# Calculate the proportion of correct decisions for each trial group and CIs ####
proportion_data <- choices %>%
  # here we filter out the individuals from each experiment that would bias the last n panel (bc their last n is actually quite early)
  # filter(trial_group != 'inbetween') %>%
  filter( ! (individual %in% c('33', '11') & trial_group == "last_n" &
               experiment == "thick_oblique")) %>%
  filter( ! (individual %in% c('51', '33', '11') & trial_group == "last_n" &
               experiment == "perp_para")) %>%
  filter( ! (individual %in% c('35', '11') & trial_group == "last_n" &
               experiment == "nat_can")) %>% 
  # no filtering for thin oblique as all individuals had sufficient learning phase
  group_by(experiment, trial_group, first) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
  tidyr::unnest(tst)

# comparing same trial groups across different stimulus pairs ####
# Proportion differences for each trial group calculated and p values corrected for mult comps too.
# I want to do this having filtered out particular individuals as in proportion_data above
# Calculate proportions
# proportion df above is redundant. the variables cases and total for rows with 1 or 0 will determine the value of the other row.
lastn_comp <- proportion_data %>% filter(first == "1", trial_group == "last_n") 
# Create vectors of counts and totals for pairwise.prop.test
counts <- lastn_comp$cases
names(counts) <- lastn_comp$experiment
totals <- lastn_comp$total
names(totals) <- lastn_comp$experiment

pairwise_results <- pairwise.prop.test(counts, totals, p.adjust.method = "bonferroni")
# nothing is sig! Unless you look at last 15 trials or so. I stopped too early?
# Thin oblique is v similar to everything, while thick oblique is almost sig diff to 
# nat can, and slightly diff to perp para (based on looking at p values rather than)
# effect sizes
# Also, in combination with the fact that nat can is actually quite high. Limits finding diffs
# Still, I need to plot these data just to make sure.
# and compare with the comparisons I've been making using corresponding trials, 
# where I have been finding more significance.

# Barplot: proportion correct / incorrect for different trial groups and CIs ####
# first arrange order in which experiments appear in facet plot:
proportion_data$experiment <- factor(
  proportion_data$experiment,
  levels = c('perp_para', 'thin_oblique', "thick_oblique", "nat_can"))

proportion_data %>%
  filter(trial_group != "inbetween") %>%
  ggplot(aes(x = trial_group, y = mean, fill = as.character(first)))+ 
  geom_col(position =  position_dodge(), color = "black")+
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge2( width = 0.7, padding = 0.7))+
  geom_hline(lty = 2, yintercept = 0.5)+
  scale_fill_manual(values = c("1" = '#27b376', '0' = '#bf212f'))+
  scale_y_continuous(labels=scales::percent)+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_grid(experiment ~ trial_group, scales = 'free_x')
# ggsave("figures//facet_grid_barplot_perf_by_experiment_and_trial_group.png",
#        plot = last_plot(), width = 8, height = 8, dpi =300)
