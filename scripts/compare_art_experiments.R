# =============================================================================
# compare_art_experiments.R
# Planned cross-experiment comparisons -- statistical questions that combine
# already-fit experiments, as opposed to the systematic per-experiment fitting
# in 2_fit_models.R. Deliberately unnumbered: unlike the numbered pipeline
# stages, this file has no fixed position -- it's a growing collection you add
# a new block to whenever a new cross-experiment question comes up. It only
# needs to run sometime after 1_loading_cleaning.R.
#
# Run this script when a new planned comparison is added, or when the
# underlying data changes.
# Output: scripts/intermediate_outputs/experiment_comparisons.RData
#
# Run from the project root directory (where data_analysis.Rproj lives).
# =============================================================================

library(here)
library(lme4)
library(tidyverse)
library(emmeans)

setwd(rprojroot::find_rstudio_root_file())

source("scripts/1_loading_cleaning.R")

choices$decision     <- as.numeric(choices$decision)
choices$manipulation <- relevel(choices$manipulation, ref = "test")

# -----------------------------------------------------------------------------
# Q: perpendicular/parallel vs. oblique stimuli -- does discrimination
# performance depend on stimulus type, or is any artificial pattern equally
# discriminable? perpPara_170725 and thinOb_150823 also have control-phase
# trials in `choices`; filtering to manipulation == "test" keeps the
# comparison to discrimination performance only, matching how mTest/mTc are
# split in 2_fit_models.R.
# Produces: mArtCompare, emmArtCompare, artContrastLink, artContrastResp
# -----------------------------------------------------------------------------

artCompare <- choices %>%
  filter(experiment %in% c("perpPara_170725", "thinOb_150823", "thickOb_140823"),
         manipulation == "test")

mArtCompare <- glmer(
  decision ~ rank_trial * experiment + sideAlt_trial + reward_side + (1 | individual),
  data = artCompare, family = binomial
)

# Predicted P_corr (link scale) per experiment at trial 46, the max test-phase
# trial common to all three experiments (perpPara: 47, thickOb: 46, thinOb: 66)
emmArtCompare <- emmeans(mArtCompare, ~ experiment, at = list(rank_trial = 46))
emmArtCompare <- emmeans(mArtCompare, ~ experiment, at = list(rank_trial = 46), type = "response")

# Confirm factor level order before trusting the contrast weight vector below
levels(droplevels(artCompare$experiment))
# [1] "perpPara_170725" "thickOb_140823"  "thinOb_150823"

# Planned contrast: perpendicular/parallel vs. the average of the two oblique
# conditions -- this is the actual biological question (symmetric vs.
# asymmetric-but-matched optic flow), not three separate pairwise tests.
artContrast <- contrast(
  emmArtCompare,
  method = list("perpPara - oblique_avg" = c(1, -0.5, -0.5))
)

artContrastLink <- summary(artContrast, infer = c(TRUE, TRUE))                     # log-odds-ratio scale, for the p-value/CI
artContrastResp <- summary(artContrast, type = "response", infer = c(TRUE, TRUE))  # back-transformed odds ratio, for reporting


# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(
  mArtCompare,
  emmArtCompare,
  artContrastLink,
  artContrastResp,
  file = "scripts/intermediate_outputs/experiment_comparisons.RData"
)

message("experiment_comparisons.RData saved.")
