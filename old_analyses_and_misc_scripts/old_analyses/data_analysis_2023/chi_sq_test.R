# Chi sq tests for evidence of a difference in proportions amongst groups
rm(list = ls())
source("loading_cleaning.R")
source("custom_themes_and_colour_palettes.R")

library(tidyr)
library(rstatix)

# Assigning trials to bins (bins of 15 trials) and ordering data ####
choices <- choices %>%
  group_by(individual, experiment, manipulation) %>%
  mutate(trial_bin = ceiling(rank_trial / 15),
         trial_range = paste0((trial_bin - 1) * 15 + 1, "-", trial_bin * 15))

# Ensure trial ranges are in sequential order
choices <- choices %>%
  mutate(trial_range = factor(trial_range, levels = unique(trial_range[order(trial_bin)])))

# For each group / bin, calculate the prop correct and conf intervals ####
proportion_data <- choices %>%
  group_by(experiment, manipulation, trial_bin, trial_range, decision) %>%
  summarise(wasp_n = n_distinct(individual),
            cases = n()) %>%
  mutate(total = sum(cases)) %>%
  rowwise() %>%
  mutate(tst = list(binom::binom.confint(cases, total, method = "wilson"))) %>%
  unnest(tst) %>%
  ungroup()

# make desired bin groupings to test: 
# filter out the experiments you dont want to look at
# combine the bins that you want to test.
# eg. make combined bin from bins 123 to test

# make and store contingency tables for each desired bin grouping
# splitting into a list by bins and then using lapply may work better
continge_tables <- xtabs(~experiment + decision + trial_bin, data = choices) # seems possible to index matrix slice by name or position
apply(continge_tables, 3, chisq.test)
apply(continge_tables[,,1:3], 3, pairwise.prop.test)

continge_tables <- xtabs(~experiment + decision, data = choices) 
apply(continge_tables[1:2,], 1, prop.test)
prop.test(continge_tables[1:2,])

# Assuming 'continge_tables' is a 2xN matrix (rows are successes and failures)
x <- continge_tables[,1]  # First row: successes
n <- rowSums(continge_tables)  # Column-wise totals (successes + failures)

# Apply prop.test across groups
results <- mapply(function(successes, trials) {
  prop.test(successes, trials)
}, x, n)

# ideally what i actually want to do is form the set of comparisons and then pass those
# to prop.test. how can i neatly pass prop test my different pairwise comparisons, and receive neat output bacK?