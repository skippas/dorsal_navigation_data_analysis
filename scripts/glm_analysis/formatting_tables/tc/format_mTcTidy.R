mTcTidyFmt <- mTcTidy %>%
  left_join(
    unique(choices[, c("experiment", "nat_or_art")]),
    by = "experiment"
  ) %>%
  rename(logOdds = estimate) %>%
  mutate(
    pFmt = pvalue(p.value, accuracy = 0.001),
    oddsMultiplier = exp(logOdds)
  ) %>%
  fmtNumCols() %>%
  make_confint_col(prefix = "[", suffix = "]") %>%
  arrange(nat_or_art, experiment)

mTcTidyInLine <- mTcTidyFmt

mTcTidyWideInLine <- mTcTidyInLine %>%
  select(term, logOdds, p.value, pFmt, confint, experiment) %>%
  pivot_wider(
    names_from = "term",
    values_from = c("logOdds", "p.value", "pFmt", "confint"),
    names_glue  = "{term}_{.value}"
  )

mTcCoefsMsTab <- mTcTidyFmt %>%
  select(experiment, nat_or_art, term, logOdds, confint, pFmt)

tcTrialSlopeInLine <- mTcTidyFmt %>%
  filter(term == "rank_trial") %>%
  select(experiment, nat_or_art, logOdds, confint, pFmt, oddsMultiplier) %>%
  tibble::column_to_rownames("experiment")

tcTrialSlopeMsTab <- tcTrialSlopeInLine %>%
  tibble::rownames_to_column("experiment")
