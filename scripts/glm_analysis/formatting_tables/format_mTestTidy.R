mTestTidyFmt <- mTestTidy %>%
  rename(logOdds = estimate) %>%
  mutate(
    pFmt = pvalue(mTestTidy$p.value, accuracy = 0.001),
    oddsMultiplier = exp(logOdds)
  ) %>%
  fmtNumCols() %>%
  make_confint_col(prefix = "[", suffix = "]") %>%
#  mutate(term = recode(term, !!!term_labels)) %>% not sure i want to recode the terms
  arrange(nat_or_art, experiment)

mTestTidyInLine <- mTestTidyFmt

mTestTidyWideInLine <- mTestTidyInLine %>%
  select(term, logOdds, p.value, pFmt, confint, experiment) %>% # why am i keeping p.value here?
  pivot_wider(
    names_from = "term",
    values_from = c("logOdds", "p.value", "pFmt", "confint"),
    names_glue = "{term}_{.value}"
  )

mTestCoefsMsTab <- mTestTidyFmt %>% 
  transmute(
    experiment, nat_or_art, term,
    logOdds95CI = sprintf("%s %s", logOdds, confint),
    pFmt
  )

testTrialSlopeInLine <- mTestTidyFmt %>%
  filter(term == "rank_trial") %>%
  select(experiment, nat_or_art, logOdds, confint, pFmt, oddsMultiplier) %>%
  tibble::column_to_rownames("experiment")

testTrialSlopeMsTab <- testTrialSlopeInLine %>%
  tibble::rownames_to_column("experiment") %>%
  transmute(
    experiment, nat_or_art,
    trialLogOdds95CI = sprintf("%s %s", logOdds, confint),
    pFmt,
    oddsMultiplier
  )
