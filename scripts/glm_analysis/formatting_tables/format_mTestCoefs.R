# objects created:
# mTestCoefsFmt, mTestCoefsRef, mTestCoefsTbl,
# testTrialSlopeRef, testTrialSlopeTbl

mTestCoefsFmt <- mTestCoefs %>%
  rename(logOdds = estimate) %>%
  mutate(
    term           = if_else(term == "sd__(Intercept)", "Individual (SD)", term),
    pFmt           = pvalue(mTestCoefs$p.value, accuracy = 0.001),
    oddsMultiplier = exp(logOdds)
  ) %>%
  fmtNumCols() %>%
  make_confint_col(prefix = "[", suffix = "]") %>%
#  mutate(term = recode(term, !!!term_labels)) %>% not sure i want to recode the terms
  arrange(nat_or_art, experiment)

mTestCoefsRef <- mTestCoefsFmt %>%
  select(term, logOdds, p.value, pFmt, confint, experiment) %>%
  pivot_wider(
    names_from = "term",
    values_from = c("logOdds", "p.value", "pFmt", "confint"),
    names_glue = "{term}_{.value}"
  ) %>%
  tibble::column_to_rownames("experiment")

mTestCoefsTbl <- mTestCoefsFmt %>%
  transmute(
    experiment, nat_or_art, term,
    logOdds95CI = sprintf("%s %s", logOdds, confint),
    pFmt
  ) %>%
  mutate(
    logOdds95CI = if_else(term == "Individual (SD)", logOdds, logOdds95CI),
    pFmt        = if_else(term == "Individual (SD)", "\u2014", pFmt)
  )

testTrialSlopeRef <- mTestCoefsFmt %>%
  filter(term == "rank_trial") %>%
  select(experiment, nat_or_art, logOdds, confint, pFmt, oddsMultiplier) %>%
  tibble::column_to_rownames("experiment")

testTrialSlopeTbl <- testTrialSlopeRef %>%
  tibble::rownames_to_column("experiment") %>%
  transmute(
    experiment, nat_or_art,
    trialLogOdds95CI = sprintf("%s %s", logOdds, confint),
    pFmt,
    oddsMultiplier
  )
