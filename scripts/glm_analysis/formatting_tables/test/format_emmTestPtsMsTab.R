emmTestPtsMsTab <- emmTestPtsInLine %>%
  tibble::rownames_to_column("experiment") %>%
  transmute(
    experiment,
    testStartPcorr95CI = sprintf("%s (%s)", first_probFmt, first_confintFmt),
    testMidPcorr95CI = sprintf("%s (%s)", middle_probFmt, middle_confintFmt),
    testEndPcorr95CI = sprintf("%s (%s)", last_probFmt, last_confintFmt),
    middleTrial = middle_rank_trial,
    testEndTrial = last_rank_trial
  )
