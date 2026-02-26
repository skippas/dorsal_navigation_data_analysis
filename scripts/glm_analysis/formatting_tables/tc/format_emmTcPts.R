emmTcPtsWide <- emmTcPts %>%
  group_by(experiment, nat_or_art) %>%
  mutate(
    trial_pos = case_when(
      rank_trial == min(rank_trial, na.rm = TRUE) ~ "ctrlStart",
      rank_trial == max(rank_trial, na.rm = TRUE) ~ "testEnd"
    )
  ) %>%
  filter(!is.na(trial_pos)) %>%
  ungroup() %>%
  fmtNumCols() %>%
  make_confint_col(
    lower_col = "asymp.LCL",
    upper_col = "asymp.UCL",
    out_col = "confint",
    prefix = "[",
    suffix = "]"
  ) %>%
  select(experiment, nat_or_art, rank_trial, prob, confint, trial_pos) %>%
  pivot_wider(
    names_from = trial_pos,
    values_from = c(prob, confint, rank_trial)
  )

emmTcPtsInLine <- emmTcPtsWide %>%
  column_to_rownames("experiment")

emmTcPtsMsTab <- emmTcPtsWide %>%
  transmute(
    experiment, nat_or_art,
    controlStartPcorr95CI = sprintf("%s %s", prob_ctrlStart, confint_ctrlStart),
    testEndPcorr95CI = sprintf("%s %s", prob_testEnd, confint_testEnd),
    testEndTrial = rank_trial_testEnd
  )
