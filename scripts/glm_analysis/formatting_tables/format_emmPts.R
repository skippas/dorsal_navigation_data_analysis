# Objects created:
# emmPtsFmt, emmPtsInLine, emmPtsMsTab
#
# emmPts must exist in the environment (produced by run_analysis.R).
# Columns in emmPtsInLine: first_*, middle_*, last_test_*, ctrl_start_*
# ctrl_start_* columns are NA for experiments without a control phase.

emmPtsFmt <- emmPts %>%
  fmtNumCols() %>%
  make_confint_col(
    lower_col = "asymp.LCL",
    upper_col = "asymp.UCL",
    out_col   = "confint",
    prefix    = "[",
    suffix    = "]"
  ) %>%
  select(experiment, nat_or_art, rank_trial, prob, confint, trial_pos) %>%
  pivot_wider(
    names_from  = trial_pos,
    values_from = c(prob, confint, rank_trial),
    names_glue  = "{trial_pos}_{.value}"
  )

emmPtsInLine <- emmPtsFmt %>%
  column_to_rownames("experiment")

emmPtsMsTab <- emmPtsFmt %>%
  transmute(
    experiment,
    nat_or_art,
    firstPcorr95CI     = sprintf("%s %s", first_prob,      first_confint),
    midPcorr95CI       = sprintf("%s %s", middle_prob,     middle_confint),
    lastTestPcorr95CI  = sprintf("%s %s", last_test_prob,  last_test_confint),
    ctrlStartPcorr95CI = ifelse(
      is.na(ctrl_start_prob), NA_character_,
      sprintf("%s %s", ctrl_start_prob, ctrl_start_confint)
    ),
    lastTestTrial = last_test_rank_trial
  )
