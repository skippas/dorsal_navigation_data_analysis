emmTestPtsWide <- emmTestPts %>%
  group_by(experiment, nat_or_art) %>%
  mutate(
    trial_pos = case_when(
      rank_trial == min(rank_trial, na.rm = TRUE) ~ "first",
      rank_trial == max(rank_trial, na.rm = TRUE) ~ "last",
      TRUE ~ "middle"
    )
  ) %>%
  ungroup() %>%
  fmtNumCols() %>%
  make_confint_col(
    lower_col = "asymp.LCL",
    upper_col = "asymp.UCL",
    out_col = "confint"
  ) %>%
  select(experiment, nat_or_art, rank_trial, prob, confint, trial_pos) %>%
  pivot_wider(
    names_from = trial_pos,
    values_from = c(prob, confint, rank_trial),
    names_glue = "{trial_pos}_{.value}"
  )

emmTestPtsInLine <- emmTestPtsWide %>%
  column_to_rownames("experiment")

emmTestPtsMsTab <- emmTestPtsWide %>%
  transmute(
    experiment,
    startPcorr95CI = sprintf("%s %s", first_prob, first_confint),
    midPcorr95CI = sprintf("%s %s", middle_prob, middle_confint),
    endPcorr95CI = sprintf("%s %s", last_prob, last_confint),
    middleTrial = middle_rank_trial,
    endTrial = last_rank_trial
  )
# to do: remove all these test prefixes from emmtestptsmstab variable names
# change sig digits to 2
# begin refactoring tc formatting files
# add all table captions and figure captions
# where is manipulation term gone in tc results?
# write q3 results
