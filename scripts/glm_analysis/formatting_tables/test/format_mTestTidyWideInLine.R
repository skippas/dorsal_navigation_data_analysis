mTestTidyWideInLine <- mTestTidyInLine %>%
  rename(est = estimate, pval = p.value) %>%
  select(term_label, est, pval, pFmt, confint, experiment) %>%
  pivot_wider(
    names_from = "term_label",
    values_from = c("est", "pval", "pFmt", "confint"),
    names_glue = "{term_label}_{.value}"
  )
