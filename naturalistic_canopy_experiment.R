### Naturalistic canopy (holldobler) experiment analysis

##### data loading and cleaning ####
library(tidyverse)

natcan230917 <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx", 
                                   sheet = "17_09_2023_holldobler_canopies",
                                   col_types = "text", n_max = 8)
natcan230918 <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx",                      
                                   sheet = "18_09_2023_holldobler_canopies",
                                   col_types = "text", n_max = 8)
natcan230919 <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx",                      
                                   sheet = "19_09_2023_holldobler_canopies",
                                   col_types = "text", n_max = 8)
natcan230920 <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx",                      
                                   sheet = "20_09_2023_holldobler_canopies",
                                   col_types = "text", n_max = 8)
natcan230919_cont <- readxl::read_excel("data//data_summer_2023_20230817_2.xlsx",                 
                                        sheet = "19_09_2023_holldobler_control",
                                        col_types = "text", n_max = 8)

# pivoting data into long format
# I want a dataframe with the variables: date, individual, trial, choice number within trial and choice
# I also want to export this an excel file 
 natcan230917 <- pivot_longer(natcan230917, !individual, values_to = "decision", 
                                 names_to = "trial_number_side") %>%
   mutate(date = "2023-09-17")
 natcan230918 <- pivot_longer(natcan230918, !individual, values_to = "decision", 
                                                      names_to = "trial_number_side", values_drop_na = TRUE) %>%
   mutate(date = "2023-09-18")
 natcan230919 <- pivot_longer(natcan230919, !individual, values_to = "decision", 
                                                      names_to = "trial_number_side", values_drop_na = TRUE) %>%
   mutate(date = "2023-09-19")
 natcan230920 <- pivot_longer(natcan230920, !individual, values_to = "decision", 
                                                      names_to = "trial_number_side", values_drop_na = TRUE) %>%
   mutate(date = "2023-09-20")
 
 natcan230919_cont <- pivot_longer(natcan230919_cont, !individual, values_to = "decision", 
                                                             names_to = "trial_number_side", values_drop_na = TRUE) %>%
   mutate(date = "2023-09-20")
 
 natcan <- rbind(natcan230917, natcan230918, natcan230920, natcan230919)
 rm(list = c("natcan230917", "natcan230918", "natcan230919", "natcan230920"))
 
 natcan$trial_number <- as.numeric(gsub("[^0-9]+", "", natcan$trial_number_side))
 natcan$reward_side <- sub("^\\d+", "", natcan$trial_number_side)
 natcan$trial_number_side <- NULL
 
 # all filtering of data should happen here:
 excluded <- natcan %>% 
   filter(is.na(decision) | # what was wrong with these NAs?
                       date == "2023-09-20") 

natcan <- natcan %>% 
   filter(decision != "NA",
                     # removing data on this day because it was after I ran a control. 
                       # They may have gotten worse? I should plot it anyway at some point
                       date != "2023-09-20") 


# Because some wasps were not present for some trials, we need to make a variable
  # that describes the number of trials a specific wasp has undergone, not the overall
  # trial number. 
  # Another complication is how to treat multiple decisions per trial. I should look 
  # at both wasp trial and wasp decision number.
  ## order the trial number variable from smallest to largest for each individual and start a new counter ranking the trials from smallest to largest for each individual
  
  natcan <- natcan %>%
   group_by(individual) %>%
   mutate(rank_trial = rank(trial_number))
natcan <- natcan %>% separate(decision, into = c("decision1", "subsequent_decisions"),
                                                   extra = "merge", sep = ",", remove = T) %>%
   mutate(decision1 = as.numeric(decision1))

##### analysis ####
##### performance across trials / experience ####
# how does performance vary across trial / experience?
  natcan %>% 
   ggplot(aes(x = rank_trial, y = decision1))+
   geom_jitter(height = 0.03)
natcan_pcent_corr <- natcan %>%
   group_by(rank_trial) %>%
   summarise(percent_correct = mean(decision1) * 100)
# above is not very informative. Instead lets look at how the average performance varies over trial
  natcan_pcent_corr <- natcan %>%
   group_by(rank_trial) %>%
   summarise(percent_correct = mean(decision1) * 100)
# Plot the percentage of correct decisions across trials
  ggplot(natcan_pcent_corr, aes(x = rank_trial, y = percent_correct)) +
   geom_line() +
   geom_smooth()+
   labs(title = "Percentage of Correct Decisions Across Trials",
                 x = "Trial Number",
                 y = "Percentage Correct")

  # then Plot the percentage of correct decisions across trials
  ggplot(natcan_pcent_corr, aes(x = rank_trial, y = percent_correct)) +
   geom_line() +
   geom_smooth(colour = "red")+
   labs(title = "Percentage of Correct Decisions Across Trials",
                 x = "Trial Number",
                 y = "Percentage Correct")

  # then Plot the percentage of correct decisions across trials
  ggplot(natcan_pcent_corr, aes(x = rank_trial, y = percent_correct)) +
   geom_line(colour = "lightgrey") +
   geom_smooth(colour = "red")+
   labs(title = "Percentage of Correct Decisions Across Trials",
                 x = "Trial Number",
                 y = "Percentage Correct")

  # rolling mean of correct decisions across trials
  library("tidyquant")
p <- ggplot(natcan_pcent_corr, aes(x = rank_trial, y = percent_correct)) +
   geom_line(colour = 'lightgrey', size = 2) +
   labs(title = "Percentage of Correct Decisions Across Trials",
                 x = "Trial Number",
                 y = "Percentage Correct")+
   geom_ma(ma_fun = SMA, n = 8, color = "red", size = 3, lty =1) +
   theme_light()+
  theme(text = element_text(size = 20))
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/lineplot_performance_asfunc_experience.png",
       plot = p, width = 10, height = 8, dpi = 300)
ggsave("figures//lineplot_performance_asfunc_experience.png",
       plot = p, width = 10, height = 8, dpi = 300)




natcan %>% 
   filter(date != "2023-09-20", decision1 != 'NA') %>%
   mutate(decision1 = as.character(decision1)) %>%
   ggplot(aes(y = individual, x = rank_trial, fill = decision1))+
   geom_tile()

# No relationship between experience and performance, but is performance better than chance?
natcan %>% 
   filter(date != "2023-09-20", decision1 != 'NA') %>%
   ggplot(aes(x = as.character(individual),
                           fill = as.character(decision1))) +
   geom_bar(position = "dodge") 
natcan %>% 
   filter(date != "2023-09-20", decision1 != 'NA') %>%
   ggplot(aes(x = as.character(individual),
                                   fill = as.character(decision1))) +
   geom_bar(position = "fill") 

  natcan %>% 
   filter(date != "2023-09-20", decision1 != 'NA') %>%
   ggplot(aes(x = as.character(individual),
                           fill = as.character(decision1))) +
   geom_bar(position = "dodge") 
  
natcan %>% 
   filter(date != "2023-09-20", decision1 != 'NA') %>%
   ggplot(aes(x = as.character(individual),
                                   fill = as.character(decision1))) +
   geom_bar(position = "fill") 

##### overall performance summed across individuals ####
library(binom)

# figure inspo from https://stackoverflow.com/questions/52032323/how-to-make-single-stacked-bar-chart-in-ggplot2
succ_fail <- table(natcan$decision1)
ci <- binom.confint(x = succ_fail["1"], n = succ_fail["1"] + succ_fail["0"], 
                    methods = "asymptotic")
ci[2,4] <- 1 - ci[, "mean"]
ci[, "decision"] <- c("Correct", "Incorrect")

props <- prop.table(succ_fail)
names(props) <- c("Incorrect", "Correct")
props <- tibble(decision = names(props), proportion = props)

# need to reverse order of legend
p <- ci %>% 
   ggplot(aes(x = "", y = mean, fill = decision))+
   geom_col(position =  position_fill(reverse = T))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 3)+
geom_text(aes(label = paste0(round(mean, 2))), 
          position = position_fill(reverse = TRUE, vjust = 0.5),
          size = 10)+
  geom_hline(yintercept = 0.5, lty = 2,  size= 3)+
  scale_fill_brewer(palette = "Set1", direction = -1)+
  labs(y = "proportion of decisions")+
  theme(axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        text =  element_text(size = 30))
ggsave("/Users/andrescheepers/Library/CloudStorage/OneDrive-LundUniversity/PhD/my_phd/images/barplot_decision_proportion_indiv_summed.png",
       plot = p, width = 10, height = 8, dpi = 300)
ggsave("figures//barplot_decision_proportion_indiv_summed.png",
       plot = p, width = 10, height = 8, dpi = 300)





# To do:
# what about data the control and data collected after the control? How does it look?
natcan230919_cont$trial_number <- as.numeric(gsub("[^0-9]+", "",
                                                  natcan230919_cont$trial_number_side))
natcan230919_cont$reward_side <- sub("^\\d+", "",
                                     natcan230919_cont$trial_number_side)
natcan230919_cont$trial_number_side <- NULL

# Because some wasps were not present for some trials, we need to make a variable
# that describes the number of trials a specific wasp has undergone, not the overall
# trial number. 
natcan230919_cont <- natcan230919_cont %>% 
  filter(decision != 'NA') %>%
  group_by(individual) %>%
  mutate(rank_trial = rank(trial_number))

natcan230919_cont <- natcan230919_cont %>% separate(decision, into = c("decision1", "subsequent_decisions"),
                              extra = "merge", sep = ",", remove = T) %>%
  mutate(decision1 = as.numeric(decision1))


natcan230919_cont %>% 
  mutate(decision1 = as.character(decision1)) %>%
  ggplot(aes(y = individual, x = rank_trial, fill = decision1))+
  geom_tile()

ggplot(natcan230919_cont, aes(x = as.character(individual),
                   fill = as.character(decision1))) +
  geom_bar(position = "dodge") 

# data collected after control:
natcan %>%
  filter(date == "2023-09-20", decision1 != 'NA') %>%
  ggplot(aes(x = rank_trial, y = decision1))+
  geom_jitter(height = 0.03)

natcan %>% 
  filter(date == "2023-09-20", decision1 != 'NA') %>%
  mutate(decision1 = as.character(decision1)) %>%
  ggplot(aes(y = individual, x = rank_trial, fill = decision1))+
  geom_tile()

natcan %>% 
  filter(date == "2023-09-20", decision1 != 'NA') %>%
  ggplot(aes(x = as.character(individual),
             fill = as.character(decision1))) +
  geom_bar(position = "dodge") 

natcan %>% 
  filter(date == "2023-09-20", decision1 != 'NA') %>%
  ggplot(aes(x = as.character(individual),
             fill = as.character(decision1))) +
  geom_bar(position = "fill") 


natcan %>% 
  mutate(decision1 = as.character(decision1)) %>%
  ggplot(aes(y = individual, x = rank_trial, fill = decision1))+
  geom_tile()+
  scale_fill_manual(values = c("0" = "#e41a1c", "1" = "#4daf4a"), 
                    breaks = c( "1","0"), 
                    labels = c("Correct", "Incorrect"))+
  scale_y_discrete(labels = c("Control", "Test"))+
  labs(x = "Trial", y = "Experiment", fill = "First decision")+
  facet_wrap(facets = "individual",  ncol = 1, scales = "free_y",
             labeller = labeller(individual = ~ paste("Individual: ", .),
                                 .multi_line = F))+
  theme_bw()+
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
ggsave("optflow_tileplot_vg_version.png", plot = last_plot(), width = 6, height = 8, dpi = 300)


natcan %>% filter(date != "2023-09-20", decision1 != 'NA') %>%
  ggplot(aes(x = rank_trial))+
  geom_histogram()+
  facet_wrap(facets = "decision1", ncol = 1)

summary(natcan)
natcan_mod <- glm(decision1 ~ rank_trial, data = natcan, family = binomial)
summary(natcan_mod)

thick$pred_prob <- predict(m_thick, type = "response")
thin$pred_prob <- predict(m_thin, type = "response")

combined_data <- bind_rows(#thick %>% select(first, rank_trial, pred_prob),
  thin %>% select(first, rank_trial, pred_prob))

ggplot(natcan, aes(x = rank_trial, y = pred_prob)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "glm", method.args = list(family = binomial),
              fill = "grey", colour = "black", alpha = 0.5) +
  geom_hline(yintercept = 0.5, lty = 2)+
  labs(x = "Trial", y = "Predicted Probability") +
  theme_minimal()+
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
ggsave("figures//optflow_log_reg_thin_stripes.png", plot = last_plot(), width = 5, height = 3, dpi = 300)


#

