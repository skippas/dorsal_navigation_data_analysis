# =============================================================================
# format_results.R
# Transforms raw model outputs into display-ready tables for the manuscript.
# Must be run after run_analysis.R.
# Output: scripts/glm_analysis/formatted_results.RData
#
# Run from the project root directory (where data_analysis.Rproj lives).
# =============================================================================

setwd(rprojroot::find_rstudio_root_file())

load("scripts/glm_analysis/analysis_results.RData")

source("functions/format_numeric_cols.R")
source("functions/make_confint_col.R")
source("functions/format_tables.R")

formattingTablesDir <- "scripts/glm_analysis/formatting_tables"
sourceFmt <- function(file) source(file.path(formattingTablesDir, file))

# -----------------------------------------------------------------------------
# Format combined emmeans predictions
# Produces: emmPtsInLine, emmPtsMsTab (top-level, shared across testRes/tcRes)
# -----------------------------------------------------------------------------

sourceFmt("format_emmPts.R")     # -> emmPtsInLine, emmPtsMsTab

# -----------------------------------------------------------------------------
# Format test-phase model results
# Produces: testRes
# -----------------------------------------------------------------------------

sourceFmt("format_mTestTidy.R")  # -> mTestTidyWideInLine, mTestCoefsMsTab,
                                 #    testTrialSlopeInLine, testTrialSlopeMsTab

testRes <- list(
  mCoefs           = mTestTidy,
  mCoefsInLine     = mTestTidyWideInLine,
  mCoefsMsTab      = mTestCoefsMsTab,
  trialSlopeInLine = testTrialSlopeInLine,
  trialSlopeMsTab  = testTrialSlopeMsTab
)

# -----------------------------------------------------------------------------
# Format test-control model results
# Produces: tcRes
# -----------------------------------------------------------------------------

sourceFmt("format_contTcLn.R")   # -> contTcLnInLine, contTcLnMsTab
sourceFmt("format_mTcTidy.R")    # -> mTcTidyWideInLine, mTcCoefsMsTab,
                                 #    tcTrialSlopeInLine, tcTrialSlopeMsTab

tcRes <- list(
  mCoefsInLine     = mTcTidyWideInLine,
  mCoefsMsTab      = mTcCoefsMsTab,
  contrastsInLine  = contTcLnInLine,
  contrastsMsTab   = contTcLnMsTab,
  trialSlopeInLine = tcTrialSlopeInLine,
  trialSlopeMsTab  = tcTrialSlopeMsTab
)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(
  emmAll,
  emmPts,
  emmPtsInLine,
  emmPtsMsTab,
  testRes,
  tcRes,
  choices_rolling,
  plotStyle,
  file = "scripts/glm_analysis/formatted_results.RData"
)

message("formatted_results.RData saved.")
