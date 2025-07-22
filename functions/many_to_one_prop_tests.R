# Another way of comparing performance between stimuli with different numbers of trials
# is to only make comparisons between stimuli with the same number of trials.
# So compare thick oblique stimuli with all others for the same number of trials
# Then compare the stimulus with the next highest number of trials to all the other stimuli
# after the corresponding number of trials, and so on.
# 
# comparing all other stimuli to a focal stimulus with a given number of trials basically.


# is plot data below correct? drop in perp para below 50?
# make logistic regression (smoothed) plot combined with bin plot. find code in berlin icn script?
# neaten figure (labels readable?)
# quite an ugly figure. distracting.
# look at other relevant bin groups. eg. just the 3rd bin vs other 3rd bins
p<- choices_subset %>% 
  # removing thick oblique from the 345 bin, very few obs anyway. 
  #filter(! (bin_group == "345" & experiment == "thick_oblique") ) %>% 
  filter(! (bin_group == "rest") ) %>% 
  ggplot(aes(x = as.factor(experiment), y = mean, color = as.character(experiment), group = as.character(experiment)))+
  geom_point(position = position_dodge(width = 0.3), size = 4)+
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.3), width = 0.2)+
  geom_hline(lty = 2, yintercept = 0.5, size = 1.5)+
  geom_text(aes(label = paste("N wasp =", wasp_n), y = 0.45))+
  geom_text(aes(label = paste("N trial =", total), y = 0.44))+
  scale_color_viridis_d()+
  scale_y_continuous(labels = scales::percent)+
  labs(y = "% of choices for reward",
       x = "stimulus-pair")+
  custom_theme_text_enlarged+
  theme(axis.line = element_line(colour = 'black', size = 1.5),
        axis.ticks = element_line(colour = "black", size = 1.5),
        axis.ticks.length=unit(.3, "cm"),
        plot.margin = margin(15,1,5,1),
        #strip.text = element_blank(),
        axis.text.x = element_text(angle = 20, size = 20, vjust = 0.75, hjust = 0.8),
        legend.position = "none",
        panel.margin = unit(1, "lines"),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line())+
  facet_wrap(facets = "bin_group", scales = "free_x", nrow = 1)


# separating dfs into bins, comparing stim pairs within bins, generating p-values, adding to plot #### 
library(ggprism) # can i get rid of these?
library(rstatix)

bin123 <- choices %>% filter(manipulation == "test", trial_bin %in% c(1,2,3)) 
bin123 <- table(bin123$experiment, bin123$decision)

# run chi sq test and pairwise prop tests
chisq.test(bin123)

pvals_bin123 <- as.data.frame(
  rstatix::pairwise_prop_test(bin123, p.adjust.method = "bonferroni")) %>%
  filter(group1 == "thick_oblique" | group2 == "thick_oblique") 
# swap values so focus group always in group1 column
pvals_bin123[pvals_bin123$group2 == "thick_oblique", c("group1", "group2")] <-
  pvals_bin123[pvals_bin123$group2 == "thick_oblique", c("group2", "group1")]

# set label positions and add facet group
pvals_bin123 <- pvals_bin123 %>%
  mutate(bin_subset = "bin123",
         y.position = case_when(
           group2 == "thin_oblique" ~ 0.8,
           group2 == "nat_can" ~ 0.85, 
           TRUE ~ 0.825
         )) %>%
  select(-p.adj, -p.adj.signif)

# Repeat for different bin groups
# thin oblique bin345 vs other stim-pairs corresp bins
bin345 <- choices %>% filter(experiment_type == "test",
                             trial_bin %in% c(3,4,5)) 

bin345 <- table(bin345$experiment, bin345$first)
chisq.test(bin345)
pvals_bin345 <- as.data.frame(
  pairwise_prop_test(bin345, p.adjust.method = "bonferroni")) %>%
  filter(group1 == "thin_oblique" | group2 == "thin_oblique") 

pvals_bin345[pvals_bin345$group2 == "thin_oblique", c("group1", "group2")] <-
  pvals_bin345[pvals_bin345$group2 == "thin_oblique", c("group2", "group1")]

pvals_bin345 <- pvals_bin345 %>%
  mutate(bin_subset = "bin345",
         y.position = case_when(
           group2 == "perp_para" ~ 0.8,
           group2 == "nat_can" ~ 0.85, 
           TRUE ~ 0.825
         )) %>%
  select(-p.adj, -p.adj.signif)

# perp para bin 6789 vs natcan corresp bins
bin6789 <- choices %>% filter(experiment_type == "test",
                              trial_bin %in% c(6,7,8,9)) 

bin6789 <- table(bin6789$experiment, bin6789$first)
chisq.test(bin6789)
pvals_bin6789 <- as.data.frame(
  pairwise_prop_test(bin6789, p.adjust.method = "bonferroni")) %>%
  filter(group1 == "perp_para" | group2 == "perp_para") 

pvals_bin6789[pvals_bin6789$group2 == "perp_para", c("group1", "group2")] <-
  pvals_bin6789[pvals_bin6789$group2 == "perp_para", c("group2", "group1")]

pvals_bin6789 <- pvals_bin6789 %>%
  mutate(bin_subset = "bin6789",
         y.position = case_when(
           group2 == "thin_oblique" ~ 0.8,
           group2 == "nat_can" ~ 0.85, 
           TRUE ~ 0.825
         )) %>%
  select(-p.adj, -p.adj.signif)

# combine pval dfs
pvals_bins <- rbind(pvals_bin123, pvals_bin345, pvals_bin6789) %>%
  # below removes p value for comp to thick_oblique in bin345 to neaten figure
  # perhaps this is misleading or confusing and should just be left in.
  filter(! (bin_subset == "bin345" & group2 == "thick_oblique")) 

p+ add_pvalue(pvals_bins, color = "black", group = "black", tip.length = 0)


# tables ####
# want a table that forms the basis of the chi square test: 
# ie. shows breakdown of relationship betw decision variable and exp variable
library(gt)
x<- bin_subsets %>% group_by(bin_subset) %>%
  select(bin_subset, experiment, first, cases) %>%
  pivot_wider(values_from = "cases", names_from = c("experiment")) %>%
  rename("Decision" = first) %>%
  mutate(Decision = fct_recode(as.factor(Decision), Rewarding = "1",
                               Unrewarding = "0"))  %>%
  # reorder decision 
  arrange(desc(Decision))

x<- gt(x)
gtsave(x, "tally_table.docx")

# display adjusted p values
# remove thick_oblique from second facet. (for sake of neatness basically)
# what are effect sizes? why isnt nat can coming out as sig diff vs others?
# could also plot the DIFFERENCES between stimulus-pairs and the CIs of those. 
# Would make p-values more interpretable possibly.

# comparisons / points to make:
# i may be underpowering my analysis by only selecting particular bins. 
# given no slope for nat can, perhaps I can use all observations
# perhaps I would get more power by analysing all this in a modeling framework
# make comparisons against the 50% line for the final bins of each test experiment
# report mean correct choices for those bins
# do the same for the controls - compare versus 50% line and the final bins of 
# of each experiment.
# using final 3 bins for certain comparisons is conservative given that performances
# appear to not have peaked in some of those bins (eg. thick and thin oblique)
# make points about apparent differences in slopes, esp for perp para. Could this
# be down to low n for wasps?
# try to show n wasps and decisions in each bin and for each experiment.

