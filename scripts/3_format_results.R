# =============================================================================
# 3_format_results.R
# Transforms raw model outputs into display-ready tables for the manuscript.
# Must be run after 2_fit_models.R.
# Output: scripts/intermediate_outputs/formatted_results.RData
#
# Run from the project root directory (where data_analysis.Rproj lives).
# =============================================================================

setwd(rprojroot::find_rstudio_root_file())

library(tidyverse)
library(scales)

load("scripts/intermediate_outputs/analysis_results.RData")
load("scripts/intermediate_outputs/experiment_comparisons.RData")

source("functions/format_numeric_cols.R")
source("functions/make_confint_col.R")
source("functions/format_tables.R")

# -----------------------------------------------------------------------------
# Format combined emmeans predictions
# Produces: emmPtsRef, emmPtsTbl
# -----------------------------------------------------------------------------

source("scripts/format_results_helpers/format_emmPts.R")     # -> emmPtsRef, emmPtsTbl

# -----------------------------------------------------------------------------
# Format model coefficients (test-only and test-control combined)
# Produces: mAllCoefsFmt, mAllCoefsRef, mAllCoefsTbl,
#           allTrialSlopeRef, allTrialSlopeTbl
# -----------------------------------------------------------------------------

mAllCoefs <- bind_rows(mTestCoefs, mTcCoefs)
source("scripts/format_results_helpers/format_mAllCoefs.R")

# -----------------------------------------------------------------------------
# Format test-control contrasts
# Produces: contrastRes (contrasts only)
# -----------------------------------------------------------------------------

source("scripts/format_results_helpers/format_contTcLn.R")   # -> contTcLnRef, contTcLnTbl

contrastsRef <- contTcLnRef
contrastsTbl <- contTcLnTbl

# -----------------------------------------------------------------------------
# Format the perpendicular/parallel vs. oblique planned comparison
# Produces: artCompareRef
# -----------------------------------------------------------------------------

source("scripts/format_results_helpers/format_artCompare.R")   # -> artCompareRef

# -----------------------------------------------------------------------------
# Sample size summaries
# Produces: nLabels (figure annotations), nSummary (Table S1 columns)
# -----------------------------------------------------------------------------

nLabels <- choices %>%
  group_by(experiment, manipulation) %>%
  summarise(
    n_wasps   = n_distinct(individual),
    n_choices = n(),
    label     = paste0("n = ", n_wasps, " wasps, ", n_choices, " choices"),
    .groups = "drop"
  )

nSummary <- choices %>%
  group_by(experiment, manipulation) %>%
  summarise(
    n_wasps   = n_distinct(individual),
    n_choices = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = manipulation,
    values_from = c(n_wasps, n_choices),
    names_glue  = "{.value}_{manipulation}"
  ) %>%
  mutate(
    across(c(n_wasps_control, n_choices_control),
           ~ if_else(is.na(.), "---", as.character(.))),
    across(c(n_wasps_test, n_choices_test), as.character)
  )

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(
  emmAll,
  emmPts,
  emmPtsRef,
  emmPtsTbl,
  mAllCoefsFmt,
  mAllCoefsRef,
  mAllCoefsTbl,
  allTrialSlopeRef,
  allTrialSlopeTbl,
  contrastsRef,
  contrastsTbl,
  artCompareRef,
  q2ArtTableOrder,
  q2ArtTcContrastTable,
  choices_rolling,
  nLabels,
  nSummary,
  plotStyle,
  file = "scripts/intermediate_outputs/formatted_results.RData"
)

message("formatted_results.RData saved.")
