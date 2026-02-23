emmTestPtsInLine <- emmTestPts %>%
  group_by(experiment, nat_or_art) %>%
  mutate(
    trial_pos = case_when(
      rank_trial == 1 ~ "first",
      rank_trial == max(rank_trial, na.rm = TRUE) ~ "last",
      rank_trial > 1 & rank_trial < max(rank_trial, na.rm = TRUE) ~ "middle"
    ),
    probFmt = sprintf("%.3f", prob),
    confintFmt = sprintf("%.3f-%.3f", asymp.LCL, asymp.UCL)
  ) %>%
  select(experiment, nat_or_art, rank_trial, probFmt, confintFmt, trial_pos) %>%
  pivot_wider(
    names_from = trial_pos,
    values_from = c(probFmt, confintFmt, rank_trial),
    names_glue = "{trial_pos}_{.value}"
  )

emmTestPtsInLine <- column_to_rownames(emmTestPtsInLine, "experiment")
