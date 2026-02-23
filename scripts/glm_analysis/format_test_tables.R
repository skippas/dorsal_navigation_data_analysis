# in this section the three or four data structures produced above are re-formatted
# into one / several different formats including:
# dfs for plotting (no suffix and no formatting needed)
# dfs for in-line referencing (InLine suffix)
# dfs to act as manuscript tables (MsTab suffix)

# prettify tables for reporting in line
source("functions/format_numeric_cols.R")
source("functions/format_tables.R")

# rename terms in model tables so they are easy to access in inline writing
term_labels <- c(
  "(Intercept)"      = "int",
  "rank_trial"       = "trial",
  "reward_sideRight" = "sideR",
  "sideAlt_trialTRUE" = "sideAltT"
)

# 1. in-line formatting
# a) test pts results
emmTestPtsInLine <- emmTestPts %>%
  group_by(experiment, nat_or_art) %>%
  # add a 'trial pos' variable that will make indexing table easier:
  mutate(
    trial_pos = case_when(
      rank_trial == 1 ~ "first",
      rank_trial == max(rank_trial, na.rm = TRUE) ~ "last",
      rank_trial > 1 & rank_trial < max(rank_trial, na.rm = TRUE) ~ "middle"),
    probFmt = sprintf("%.3f", prob),
    confintFmt = sprintf("%.3f-%.3f", asymp.LCL, asymp.UCL)
    ) %>%
  # pivot the data wider to allow for easy indexing of table
  select(experiment, nat_or_art, rank_trial, probFmt, confintFmt, trial_pos) %>%
  pivot_wider(names_from = trial_pos,
              values_from = c(probFmt, confintFmt, rank_trial),
              names_glue = "{trial_pos}_{.value}")
emmTestPtsInLine <- column_to_rownames(emmTestPtsInLine, "experiment")

# b) test model coefficients (display table)
mTestTidyInLine <- mTestTidy %>%
  mutate(pFmt = pvalue(mTestTidy$p.value, accuracy = 0.001)) %>%
    fmtNumCols() %>%
  mutate(confint = paste(conf.low, conf.high, sep = "-"),
         term_label = recode(term, !!!term_labels))

mTestTidyWideInLine <- mTestTidyInLine %>%
  rename(est = estimate, pval = p.value) %>%
  select(term_label, est, pval, pFmt, confint, experiment) %>%
  pivot_wider(
    names_from = "term_label",
    values_from = c("est", "pval", "pFmt", "confint"),
    names_glue = "{term_label}_{.value}"
  )

# 2. MS table formatting

# test-phase points MS table
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

testTrialSlopeInLine <- mTestTidy %>%
  filter(term == "rank_trial") %>%
  left_join(
    unique(choices[, c("experiment", "nat_or_art")]),
    by = "experiment"
  ) %>%
  transmute( # allows you to mutate and drop columns not needed in one call.
    experiment, nat_or_art,
    trialLogOdds = sprintf("%.3f", estimate),
    ci95 = sprintf("%.3f-%.3f", conf.low, conf.high),
    pFmt = scales::pvalue(p.value, accuracy = 0.001),
    oddsMultiplierPerTrial = sprintf("%.3f", exp(estimate))
  ) %>%
  arrange(nat_or_art, experiment) %>%
  tibble::column_to_rownames("experiment")

# test slope MS table
testTrialSlopeMsTab <- testTrialSlopeInLine %>%
  tibble::rownames_to_column("experiment")

# test model-coefficient MS table
mTestCoefsMsTab <- mTestTidy %>%
  left_join(
    unique(choices[, c("experiment", "nat_or_art")]),
    by = "experiment"
  ) %>%
  transmute(
    experiment, nat_or_art,
    term = recode(term, !!!term_labels),
    logOdds = sprintf("%.3f", estimate),
    ci95 = sprintf("%.3f-%.3f", conf.low, conf.high),
    pFmt = scales::pvalue(p.value, accuracy = 0.001)
  )

testRes <- list(
  emmAll = emmTestAll,
  emmPts = emmTestPts,
  emmPtsInLine = emmTestPtsInLine,
  emmPtsMsTab = emmTestPtsMsTab,
  mCoefs = mTestTidy,
  mCoefsInLine = mTestTidyWideInLine,
  mCoefsMsTab = mTestCoefsMsTab,
  trialSlopeInLine = testTrialSlopeInLine,
  trialSlopeMsTab = testTrialSlopeMsTab
)

# clean up objects created in this file
rm(
  mTest, mTestTidy, mTestTidyInLine, mTestTidyWideInLine,
  testTrialSlopeInLine, testTrialSlopeMsTab, mTestCoefsMsTab,
  emmTestAll, emmTestPts, emmTestPtsInLine,
  term_labels
)
