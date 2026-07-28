# >50% for nat can due to side bias? Below analysis only looking at decisions after alternation of side

# just to make sure that df is sorted by trial number. though this is done inside the function
choices <- choices %>% 
  group_by(experiment, individual, trial_group) %>% 
  arrange(rank_trial) %>% 
  mutate(rank_win_group = 1:n()) %>% 
  ungroup()

# Function to filter trials with alternating sides
filter_alternating_sides <- function(df) {
  df <- df %>%
    arrange(experiment, experiment_type, individual, rank_trial) %>% # Ensure the dataframe is ordered by trial_number within each individual
    group_by(individual, experiment, experiment_type) %>%
    mutate(previous_side = lag(reward_side)) %>%
    filter(is.na(previous_side) | reward_side != previous_side) %>%
    #select(-previous_side) # Remove the helper column
    
    return(df)
}

choices <- filter_alternating_sides(choices)

proportion_data <- choices %>%
  # here we filter out the individuals from each experiment that would bias the last n panel
  # filter(trial_group != 'inbetween') %>%
  filter( ! (individual %in% c('33', '11') & trial_group == "last_n" &
               experiment == "thick_oblique")) %>%
  filter( ! (individual %in% c('51', '33', '11') & trial_group == "last_n" &
               experiment == "perp_para")) %>%
  filter( ! (individual %in% c('35', '11') & trial_group == "last_n" &
               experiment == "nat_can")) %>% 
  # no filtering for thin oblique as all individuals had sufficient learning phase
  group_by(experiment, first) %>%
  summarise(cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(broom::tidy(prop.test(cases, total, conf.level=0.95)))) %>%
  tidyr::unnest(tst)

# label trials as successive or alternating, and visualize the prob correct 
# for those trials.
# also to do: turn consecutive into a cont variable and see if increasing # consec trial
# affects p correct decision
# also to do: see if side (over-sampling on one side) can explain above-chance in nat can
# Also to do: Run a full model with individual, side, alt / consec to see if nat_can still sig above 50%. 
# Or just do more modelling.

# Function to label trials as consecutive or alternating
label_consecutive_alternating <- function(df) {
  df <- df %>%
    arrange(experiment, experiment_type, individual, trial_number) %>% # Ensure the dataframe is ordered by trial_number within each individual
    group_by(individual, experiment, experiment_type) %>%
    mutate(previous_side = lag(reward_side),
           label = ifelse(is.na(previous_side), NA,
                          ifelse(reward_side == previous_side, "consecutive", "alternating"))) %>%
    filter(!is.na(label)) %>% # Remove the first trial of each individual as it cannot be labeled
    #select(-previous_side) # Remove the helper column
    return(df)
}

# Apply the function
labeled_data <- label_consecutive_alternating(choices)

# Calculate proportions of correct decisions
proportions <- labeled_data %>%
  group_by(label, experiment, experiment_type) %>%
  summarise(proportion_correct = mean(first))

# Print proportions
print(proportions)

# Perform a Chi-squared test
contingency_table <- table(labeled_data$label, labeled_data$correct)
chi_squared_result <- chisq.test(contingency_table)

# Print the result of the Chi-squared test
print(chi_squared_result)

# Visualize the results
proportions %>%
  mutate(exp_x_exp_type = paste0(experiment, experiment_type)) %>%
  ggplot(aes(x = label, y = proportion_correct, fill = label, group = exp_x_exp_type)) +
  geom_point(stat = "identity", position = "dodge") +
  geom_line()+
  labs(title = "Proportion of Correct Decisions by Trial Pattern",
       x = "Trial Pattern",
       y = "Proportion of Correct Decisions") +
  scale_fill_manual(values = c("consecutive" = "blue", "alternating" = "red")) +
  theme_minimal()

