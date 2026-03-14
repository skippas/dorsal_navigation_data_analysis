contTcLnRef <- contTcLn %>%
  mutate(pFmt = pvalue(p.value)) %>%
  fmtNumCols() %>%
  make_confint_col(
    lower_col = "asymp.LCL",
    upper_col = "asymp.UCL",
    out_col = "confint",
    prefix = "[",
    suffix = "]"
  )

contTcLnTbl <- contTcLnRef %>%
  transmute(
    experiment, nat_or_art,
    logOddsRatio95CI = sprintf("%s %s", logOddsRatio, confint),
    pFmt,
    testEndTrial = test_end
  )

contTcLnRef <- contTcLnRef %>% column_to_rownames("experiment")