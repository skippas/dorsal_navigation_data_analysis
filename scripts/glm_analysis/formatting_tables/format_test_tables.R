source("functions/format_numeric_cols.R")
source("functions/format_tables.R")

term_labels <- c(
  "(Intercept)"      = "int",
  "rank_trial"       = "trial",
  "reward_sideRight" = "sideR",
  "sideAlt_trialTRUE" = "sideAltT"
)

source("scripts/glm_analysis/formatting_tables/format_emmTestPtsInLine.R")
source("scripts/glm_analysis/formatting_tables/format_mTestTidyInLine.R")
source("scripts/glm_analysis/formatting_tables/format_mTestTidyWideInLine.R")
source("scripts/glm_analysis/formatting_tables/format_testTrialSlopeInLine.R")
source("scripts/glm_analysis/formatting_tables/format_emmTestPtsMsTab.R")
source("scripts/glm_analysis/formatting_tables/format_testTrialSlopeMsTab.R")
source("scripts/glm_analysis/formatting_tables/format_mTestCoefsMsTab.R")
source("scripts/glm_analysis/formatting_tables/format_testRes.R")
source("scripts/glm_analysis/formatting_tables/format_test_cleanup.R")
