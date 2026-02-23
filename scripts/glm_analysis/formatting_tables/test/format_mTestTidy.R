mTestTidyFmt <- mTestTidy %>%
  mutate(pFmt = pvalue(mTestTidy$p.value, accuracy = 0.001),
         oddsMultiplier = sprintf("%.3f", exp(estimate))) %>%
  fmtNumCols() %>%
  mutate(
    confint = paste(conf.low, conf.high, sep = "-"),
    term = recode(term, !!!term_labels)
  ) %>%
  arrange(nat_or_art, experiment)

mTestTidyInLine <- mTestTidyFmt

mTestTidyWideInLine <- mTestTidyInLine %>%
  select(term, estimate, p.value, pFmt, confint, experiment) %>% # why am i keeping p.value here?
  pivot_wider(
    names_from = "term",
    values_from = c("estimate", "p.value", "pFmt", "confint"),
    names_glue = "{term}_{.value}"
  )

mTestCoefsMsTab <- mTestTidyFmt %>% 
  select(experiment, nat_or_art, term, estimate, confint, pFmt)

testTrialSlopeInLine <- mTestTidyFmt %>%
  filter(term == "rank_trial") %>%
  select(experiment, nat_or_art, estimate, confint, pFmt, oddsMultiplier) %>%
  tibble::column_to_rownames("experiment")

testTrialSlopeMsTab <- testTrialSlopeInLine %>%
  tibble::rownames_to_column("experiment")
