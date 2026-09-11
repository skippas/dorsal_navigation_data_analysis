# Objects created:
# contTcRef, contTcTbl
#
# contTcRsp must exist in the environment (produced by 2_fit_models.R).
# Formats the test-end vs. control-start contrast on the ODDS RATIO scale
# (contTcRsp), not the log-odds scale (contTcLn), so that all contrasts
# reported in the manuscript -- within-experiment and cross-experiment --
# use the same, more interpretable scale. The p-value is unaffected: it is
# computed on the log-odds scale either way.

contTcRef <- contTcRsp %>%
  mutate(pFmt = pvalue(p.value)) %>%
  fmtNumCols() %>%
  make_confint_col(
    lower_col = "asymp.LCL",
    upper_col = "asymp.UCL",
    out_col = "confint",
    prefix = "[",
    suffix = "]"
  )

contTcTbl <- contTcRef %>%
  transmute(
    experiment, nat_or_art,
    oddsRatio95CI = sprintf("%s %s", odds.ratio, confint),
    pFmt,
    testEndTrial = test_end
  )

contTcRef <- contTcRef %>% column_to_rownames("experiment")

q2ArtTableOrder <- c("perpPara_170725", "thickOb_140823", "thinOb_150823", "brightDiff_250725")
