# given a dataframe, calculate the proportion correct choices for each group and CIs

calc_cis <- function(data, group_vars, count_var = "decision", method = "wilson") {
  result <- data %>%
    group_by(across(all_of(group_vars))) %>%
    # calculate values required for binom.confint function
    summarise(
      wasp_n = n_distinct(individual),
      count_1 = sum(.data[[count_var]] == 1, na.rm = TRUE),
      count_0 = sum(.data[[count_var]] == 0, na.rm = TRUE),
      total = count_1 + count_0
    ) %>%
    rowwise() %>%
    mutate(tst = list(binom::binom.confint(count_1, total, method = "wilson"))) %>%
    unnest(tst) %>%
    ungroup()
}
