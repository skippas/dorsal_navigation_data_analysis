emmTcPtsInLine <- emmTcPts %>%
  group_by(experiment, nat_or_art) %>%
  mutate(
    trial_pos = case_when(
      rank_trial == 1 ~ "ctrlStart",
      rank_trial == max(rank_trial, na.rm = TRUE) ~ "testEnd"
    ),
    confintFmt = sprintf("%.3f-%.3f", asymp.LCL, asymp.UCL),
    probFmt = sprintf("%.3f", prob),
    probConfint = sprintf("%.3f (%.3f-%.3f)", prob, asymp.LCL, asymp.UCL)
  ) %>%
  select(experiment, rank_trial, probFmt, confintFmt, probConfint, trial_pos) %>%
  pivot_wider(
    names_from = trial_pos,
    values_from = c(probFmt, confintFmt, probConfint, rank_trial)
  )

emmTcPtsInLine <- column_to_rownames(emmTcPtsInLine, "experiment")

emmTcPtsMsTab <- emmTcPtsInLine %>%
  tibble::rownames_to_column("experiment") %>%
  transmute(
    experiment,
    controlStartPcorr95CI = probConfint_ctrlStart,
    testEndPcorr95CI = probConfint_testEnd,
    testEndTrial = rank_trial_testEnd
  )
