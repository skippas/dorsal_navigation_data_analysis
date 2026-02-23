contTcLnMsTab <- contTcLnInLine %>%
  transmute(
    experiment,
    logOddsRatio,
    ci95 = confint,
    pFmt,
    controlStartTrial = control_start,
    testEndTrial = test_end
  )
