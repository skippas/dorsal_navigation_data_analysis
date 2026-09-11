# Objects created:
# emmPtsFmt, emmPtsRef, emmPtsTbl
#
# emmPts must exist in the environment (produced by 2_fit_models.R).
# Columns in emmPtsRef: first_*, middle_*, last_test_*, ctrl_start_*
# ctrl_start_* columns are NA for experiments without a control phase.

emmPtsFmt <- emmPts %>%
  mutate(
    prob      = as.character(as.integer(round(prob      * 100))),
    asymp.LCL = as.character(as.integer(round(asymp.LCL * 100))),
    asymp.UCL = as.character(as.integer(round(asymp.UCL * 100)))
  ) %>%
  make_confint_col(
    lower_col = "asymp.LCL",
    upper_col = "asymp.UCL",
    out_col   = "confint",
    prefix    = "[",
    suffix    = "]%"
  ) %>%
  select(experiment, nat_or_art, rank_trial, prob, confint, trial_pos) %>%
  pivot_wider(
    names_from  = trial_pos,
    values_from = c(prob, confint, rank_trial),
    names_glue  = "{trial_pos}_{.value}"
  )

emmPtsRef <- emmPtsFmt %>%
  column_to_rownames("experiment")

# In the overview table every P_corr column sits under a single "(%)" header,
# so a per-cell % sign on all 55 cells is redundant -- strip it here. Note
# this is deliberately NOT done to emmPtsRef, which feeds the inline prose,
# where each value does need its own unit.
dropPct <- function(x) sub("%$", "", x)

emmPtsTbl <- emmPtsFmt %>%
  transmute(
    experiment,
    nat_or_art,
    firstPcorr95CI     = sprintf("%s %s", first_prob,      dropPct(first_confint)),
    midPcorr95CI       = sprintf("%s %s", middle_prob,     dropPct(middle_confint)),
    lastTestPcorr95CI  = sprintf("%s %s", last_test_prob,  dropPct(last_test_confint)),
    ctrlStartPcorr95CI = ifelse(
      is.na(ctrl_start_prob), NA_character_,
      sprintf("%s %s", ctrl_start_prob, dropPct(ctrl_start_confint))
    ),
    lastTestTrial = last_test_rank_trial
  )
