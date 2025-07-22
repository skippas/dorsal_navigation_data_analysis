library(tidyverse)
library(readxl)
source("loading_cleaning.r")

# Tile plot: individuals' perf across the different trial groups ####
thick_oblique_diff <- choices %>% filter(stimulus == "thick_oblique_diff")
thick_oblique_diff %>%
  ggplot(aes(y = individual, x = rank_trial, fill = as.character(decision)))+
  geom_tile()+
  #facet_wrap(facets = "manipulation")
  theme_bw()

# only looking at the trials where a change of side occurred:

# Function to filter trials with alternating sides
filter_alternating_sides <- function(df) {
  df <- df %>%
    arrange(individual, rank_trial) %>% # Ensure the dataframe is ordered by trial_number within each individual
    group_by(individual) %>%
    # does this ignore the NAs? ie. it should be the previous trial for the individual, not the previous trial of the experiment (which they may or may not have participated in)
    mutate(previous_side = lag(reward_side)) %>% 
    filter(is.na(previous_side) | reward_side != previous_side) %>%
    #select(-previous_side) # Remove the helper column
    
    return(df)
}

transition_choices <- filter_alternating_sides(thick_oblique_diff) %>%
  filter(individual != "11")

transition_choices$manipulation <- 
  factor(transition_choices$manipulation,
         levels = c("no_diffuser", "diffuser_bottom", "diffuser_whole", "control"))

library(broman)
brocolors("crayons")["Green"]
brocolors("crayons")["Red"]
p <- transition_choices %>% 
  ggplot(aes(y = individual, x = rank_trial, fill = as.character(decision)))+
  geom_tile()+
  labs(x = "Trial number", fill = "Choice", y = "Individual")+
  scale_fill_manual(values = c("0" = "#ee204d", "1" = "#1cac78"),
                    labels = c("0" = "incorrect", "1" = "correct"))+
  facet_wrap(facets = "manipulation", scales = "free_x", nrow = 1)+
  theme_classic()+
  theme(legend.position = "bottom")
ggsave("plots//diffuser_manipulation_tileplot.png", p,
       width = 6, height = 4)

prop_data_ind <- transition_choices %>%
  group_by(manipulation, individual, decision) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
  tidyr::unnest(tst) %>%
  mutate(data_type = "individual") %>%
  group_by(manipulation) %>%
  mutate(min = min(total))

prop_data_overall <- transition_choices %>%
  group_by(manipulation, decision) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
  tidyr::unnest(tst) %>%
  mutate(data_type = "overall")

prop_data <- rbind(prop_data_ind, prop_data_overall) 
  
# min sample size for each individual x treatment would be nice
p <- prop_data %>% filter(decision == 1) %>%
  ggplot(aes(x = manipulation, y = mean, ymin = lower,  ymax = upper))+
  geom_pointrange(data = . %>% filter(data_type == "overall")) +
  geom_pointrange(aes(color = factor(individual)),
                  data = . %>% filter(data_type == "individual"),
                  position = position_jitter(width = 0.2), size = 0.5, alpha = 0.4) +
  geom_text(data = . %>% filter(data_type == "overall"),
            aes(x = manipulation, y = 0.15, label = paste("n =", total)), 
            size = 3, vjust = 1, hjust = 0.5, color = "black") +
  geom_text(data = . %>% filter(data_type == "individual"),
            aes(x = manipulation, y = 0.1, label = paste("nmin =", min)), 
            size = 3, vjust = 1, hjust = 0.5, color = "black") +
  labs(y = "proportion of choices correct", color = "Individual")+
  geom_hline(yintercept = 0.5, lty = 2)+
  theme(legend.position = "bottom") 
ggsave("plots//reflection_control_diffuser_manipulation_performance_estimates.png", p,
       width = 6, height = 4)
  
# Fit a logistic regression model with interaction between side and individual
# Make sure 'individual' and 'side' are factors in your dataset
thick_oblique_diff <- thick_oblique_diff %>%
  ungroup() %>%
  mutate(across(c(reward_side, individual), as.factor),
         decision = as.numeric(decision))

thick_oblique_diff$individual
model <- glm(decision ~ reward_side * individual, 
             data = thick_oblique_diff, 
             family = binomial)
summary(model)
# Calculate proportions of correct decisions by individual and side
proportions <- thick_oblique_diff %>%
  group_by(individual, reward_side) %>%
  summarise(correct_proportion = mean(decision))

proportions <- thick_oblique_diff %>%
  group_by(individual) %>%
  summarise(correct_proportion = mean(decision))

# Print the proportions
print(proportions)

# Do individuals favor a side?
thick_oblique_diff %>%
  ggplot(aes(y = individual, x = rank_trial, fill = as.character(chosen_side)))+
  geom_tile()+
  #facet_wrap(facets = "manipulation")
  theme_bw()
proportion_side_choice <- thick_oblique_diff %>%
  group_by(manipulation ,individual, chosen_side) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
  tidyr::unnest(tst)
proportion_side_choice %>% filter(chosen_side == "R") %>%
  ggplot(aes(x = individual, y = mean))+
  geom_pointrange(aes(ymin = lower, ymax = upper))+
  geom_hline(yintercept = 0.5, lty = 2)+
  facet_grid(~manipulation)
# no consistant side prefs? doesnt seem preference elevated in the control versus other treatments?
# what about the pattern of alternation? do they transition less during the control?
# side prefs across the entire exp for each individual? merge decisions across 
# manipulation to get extra stats power

# model correct decisions as predicted by trial number, individual, reward_side and interaction
# of individual and side
# check chat gpts' suggestion of using a mixed effects model


