source("loading_cleaning.r")

# Tile plot: individuals' perf across the different trial groups ####
thick_oblique_diff <- choices %>% filter(stimulus == "thick_oblique_diff")
thick_oblique_diff %>%
  ggplot(aes(y = individual, x = rank_trial, fill = as.character(decision)))+
  geom_tile()+
  #facet_wrap(facets = "manipulation")
  theme_bw()

# Fit a logistic regression model with interaction between side and individual
# Make sure 'individual' and 'side' are factors in your dataset
thick_oblique_diff <- thick_oblique_diff %>%
  ungroup() %>%
  mutate(across(c(reward_side, individual), as.factor),
         decision = as.numeric(decision))

model <- glm(decision ~ reward_side * individual, 
             data = thick_oblique_diff, 
             family = binomial)
summary(model)
# Calculate proportions of correct decisions by individual and side
proportions <- thick_oblique_diff %>%
  group_by(individual, reward_side) %>%
  summarise(correct_proportion = mean(decision))

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
