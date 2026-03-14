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
# Produces: emmPtsRef, emmPtsTbl (top-level, shared across testRes/tcRes)
# -----------------------------------------------------------------------------

sourceFmt("format_emmPts.R")     # -> emmPtsRef, emmPtsTbl

# -----------------------------------------------------------------------------
# Format test-phase model results
# Produces: testRes
# -----------------------------------------------------------------------------

sourceFmt("format_mTestCoefs.R") # -> mTestCoefsRef, mTestCoefsTbl,
                                 #    testTrialSlopeRef, testTrialSlopeTbl

testRes <- list(
  mCoefs           = mTestCoefs,
  mCoefsRef     = mTestCoefsRef,
  mCoefsTbl      = mTestCoefsTbl,
  trialSlopeRef = testTrialSlopeRef,
  trialSlopeTbl  = testTrialSlopeTbl
)

# -----------------------------------------------------------------------------
# Format test-control model results
# Produces: tcRes
# -----------------------------------------------------------------------------

sourceFmt("format_contTcLn.R")   # -> contTcLnRef, contTcLnTbl
sourceFmt("format_mTcCoefs.R")   # -> mTcCoefsRef, mTcCoefsTbl,
                                 #    tcTrialSlopeRef, tcTrialSlopeTbl

tcRes <- list(
  mCoefsRef     = mTcCoefsRef,
  mCoefsTbl      = mTcCoefsTbl,
  contrastsRef  = contTcLnRef,
  contrastsTbl   = contTcLnTbl,
  trialSlopeRef = tcTrialSlopeRef,
  trialSlopeTbl  = tcTrialSlopeTbl
)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(
  emmAll,
  emmPts,
  emmPtsRef,
  emmPtsTbl,
  testRes,
  tcRes,
  choices_rolling,
  plotStyle,
  file = "scripts/glm_analysis/formatted_results.RData"
)

message("formatted_results.RData saved.")
