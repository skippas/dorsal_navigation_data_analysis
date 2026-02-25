emmTcPtsInLine <- emmTcPts %>%
  group_by(experiment, nat_or_art) %>%
  mutate(
    trial_pos = case_when(
      rank_trial == 1 ~ "ctrlStart",
      rank_trial == max(rank_trial, na.rm = TRUE) ~ "testEnd"
    ),
    confint = sprintf("%.3f-%.3f", asymp.LCL, asymp.UCL),
    prob = sprintf("%.3f", prob),
    probConfint = sprintf("%s (%.3f-%.3f)", prob, asymp.LCL, asymp.UCL)
  ) %>%
  select(experiment, rank_trial, prob, confint, probConfint, trial_pos) %>%
  pivot_wider(
    names_from = trial_pos,
    values_from = c(prob, confint, probConfint, rank_trial)
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
