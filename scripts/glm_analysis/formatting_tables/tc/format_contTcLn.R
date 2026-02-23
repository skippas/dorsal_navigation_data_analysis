contTcLnInLine <- contTcLn %>%
  mutate(pFmt = pvalue(p.value)) %>%
  fmtNumCols() %>%
  mutate(confint = paste(asymp.LCL, asymp.UCL, sep = "-"))

contTcLnMsTab <- contTcLnInLine %>%
  transmute(
    experiment,
    logOddsRatio,
    ci95 = confint,
    pFmt,
    controlStartTrial = control_start,
    testEndTrial = test_end
  )
