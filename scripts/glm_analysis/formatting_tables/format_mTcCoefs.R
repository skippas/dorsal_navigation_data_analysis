# objects created:
# mTcCoefsFmt, mTcCoefsRef, mTcCoefsTbl,
# tcTrialSlopeRef, tcTrialSlopeTbl

mTcCoefsFmt <- mTcCoefs %>%
  rename(logOdds = estimate) %>%
  mutate(
    term           = if_else(term == "sd__(Intercept)", "Individual (SD)", term),
    pFmt           = pvalue(p.value, accuracy = 0.001),
    oddsMultiplier = exp(logOdds)
  ) %>%
  fmtNumCols() %>%
  make_confint_col(prefix = "[", suffix = "]") %>%
  arrange(nat_or_art, experiment)

mTcCoefsRef <- mTcCoefsFmt %>%
  select(term, logOdds, p.value, pFmt, confint, experiment) %>%
  pivot_wider(
    names_from = "term",
    values_from = c("logOdds", "p.value", "pFmt", "confint"),
    names_glue  = "{term}_{.value}"
  ) %>%
  tibble::column_to_rownames("experiment")

mTcCoefsTbl <- mTcCoefsFmt %>%
  transmute(
    experiment, nat_or_art, term,
    logOdds95CI = sprintf("%s %s", logOdds, confint),
    pFmt
  ) %>%
  mutate(
    logOdds95CI = if_else(term == "Individual (SD)", logOdds, logOdds95CI),
    pFmt        = if_else(term == "Individual (SD)", "\u2014", pFmt)
  )

tcTrialSlopeRef <- mTcCoefsFmt %>%
  filter(term == "rank_trial") %>%
  select(experiment, nat_or_art, logOdds, confint, pFmt, oddsMultiplier) %>%
  tibble::column_to_rownames("experiment")

tcTrialSlopeTbl <- tcTrialSlopeRef %>%
  tibble::rownames_to_column("experiment") %>%
  transmute(
    experiment, nat_or_art,
    trialLogOdds95CI = sprintf("%s %s", logOdds, confint),
    pFmt,
    oddsMultiplier
  )
