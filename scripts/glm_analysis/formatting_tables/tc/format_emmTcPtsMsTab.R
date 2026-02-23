emmTcPtsMsTab <- emmTcPtsInLine %>%
  tibble::rownames_to_column("experiment") %>%
  transmute(
    experiment,
    controlStartPcorr95CI = probConfint_ctrlStart,
    testEndPcorr95CI = probConfint_testEnd,
    testEndTrial = rank_trial_testEnd
  )
