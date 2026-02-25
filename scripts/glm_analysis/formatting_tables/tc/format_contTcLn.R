contTcLnInLine <- contTcLn %>%
  mutate(pFmt = pvalue(p.value)) %>%
  fmtNumCols() %>%
  make_confint_col(
    lower_col = "asymp.LCL",
    upper_col = "asymp.UCL",
    out_col = "confint"
  )

contTcLnMsTab <- contTcLnInLine %>%
  transmute(
    experiment,
    logOddsRatio,
    confint,
    pFmt,
    controlStartTrial = control_start,
    testEndTrial = test_end
  )
