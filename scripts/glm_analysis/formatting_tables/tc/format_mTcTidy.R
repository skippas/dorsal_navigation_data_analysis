mTcTidyInLine <- mTcTidy %>%
  mutate(pFmt = pvalue(mTcTidy$p.value, accuracy = 0.001)) %>%
  fmtNumCols() %>%
  mutate(
    confint = paste(conf.low, conf.high, sep = "-"),
    term_label = recode(term, !!!term_labels)
  )

mTcTidyWideInLine <- mTcTidyInLine %>%
  rename(est = estimate, pval = p.value) %>%
  select(term_label, est, pval, pFmt, confint, experiment) %>%
  pivot_wider(
    names_from = "term_label",
    values_from = c("est", "pval", "pFmt", "confint"),
    names_glue  = "{term_label}_{.value}"
  )

tcTrialSlopeInLine <- mTcTidy %>%
  filter(term == "rank_trial") %>%
  left_join(
    unique(choices[, c("experiment", "nat_or_art")]),
    by = "experiment"
  ) %>%
  transmute(
    experiment, nat_or_art,
    trialLogOdds = sprintf("%.3f", estimate),
    ci95 = sprintf("%.3f-%.3f", conf.low, conf.high),
    pFmt = scales::pvalue(p.value, accuracy = 0.001),
    oddsMultiplierPerTrial = sprintf("%.3f", exp(estimate))
  ) %>%
  arrange(nat_or_art, experiment) %>%
  tibble::column_to_rownames("experiment")

tcTrialSlopeMsTab <- tcTrialSlopeInLine %>%
  tibble::rownames_to_column("experiment")

mTcCoefsMsTab <- mTcTidy %>%
  left_join(
    unique(choices[, c("experiment", "nat_or_art")]),
    by = "experiment"
  ) %>%
  transmute(
    experiment, nat_or_art,
    term = recode(term, !!!term_labels),
    logOdds = sprintf("%.3f", estimate),
    ci95 = sprintf("%.3f-%.3f", conf.low, conf.high),
    pFmt = scales::pvalue(p.value, accuracy = 0.001)
  )
