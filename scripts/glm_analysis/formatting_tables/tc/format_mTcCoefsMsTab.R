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
