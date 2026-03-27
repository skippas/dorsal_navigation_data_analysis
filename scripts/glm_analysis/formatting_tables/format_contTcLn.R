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

# this code / table below is actually not used anywhere and can be removed
q2ArtTableOrder <- c("perpPara_170725", "thickOb_140823", "thinOb_150823", "brightDiff_250725")

q2ArtTcContrastTable <- contTcLnTbl %>%
  filter(nat_or_art == "artificial") %>%
  mutate(experiment = forcats::fct_relevel(experiment, q2ArtTableOrder)) %>%
  arrange(experiment) %>%
  select(experiment, logOddsRatio95CI, pFmt, testEndTrial) %>%
  rename(
    `Experiment`              = experiment,
    `Log-odds ratio [95% CI]` = logOddsRatio95CI,
    `P`                       = pFmt,
    `Test end trial`          = testEndTrial
  )