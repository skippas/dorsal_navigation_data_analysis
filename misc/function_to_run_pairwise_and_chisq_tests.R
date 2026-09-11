# elaborate and rather silly function. did this much better just using xtabs / table with apply.
# Function to perform chi-square test and pairwise comparisons for each bin
perform_tests <- function(df) {
  # Prepare the table for chi-square test
  contingency_table <- table(df$pattern_pair, df$correct_choices)
  
  # Chi-Square Test
  chi_res <- chisq.test(contingency_table)
  
  # Pairwise Comparisons
  pairwise_res <- pairwise.prop.test(
    x = df$correct_choices,
    n = df$total_trials,
    p.adjust.method = "bonferroni"
  )
  
  list(chi_res_p_value = chi_res$p.value, pairwise_res = pairwise_res)
}

# store results of those tests in a data structure
test_results <- proportion_data %>%
  filter(manipulation == "test", trial_bin == 3, decision == "1") %>%
  select(stimulus, trial_bin, trial_range, correct_choices, total_trials) %>%
  group_by(trial_bin) %>% # i guess this structure allows running tests on many bin groups. could be useful
  nest(.key = "nested_cols") %>%
  mutate(tests = map(nested_cols, perform_tests))

# Extract and summarize results
chi_square_results <- test_results %>%
  select(trial_bin, tests) %>%
  mutate(chi_res_p_value = map_dbl(tests, "chi_res_p_value"))

pairwise_results <- test_results %>%
  select(bin, tests) %>%
  mutate(pairwise_res = map(tests, "pairwise_res"))
