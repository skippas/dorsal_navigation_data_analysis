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
