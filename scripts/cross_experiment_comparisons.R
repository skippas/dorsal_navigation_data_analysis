# =============================================================================
# cross_experiment_comparisons.R
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

# Same core-experiment exclusion used in 2_fit_models.R (supplementary/control
# experiments handled separately in 4_run_supp_analysis.R).
choices <- choices %>% filter(
  ! experiment %in% c("thick_oblique_apis", "thickObDiff_210924",
                      "perpPara_240723", "perpParaPostNatcan_090925")
)

# -----------------------------------------------------------------------------
# Q: perpendicular/parallel vs. oblique stimuli -- does discrimination
# performance depend on stimulus type, or is any artificial pattern equally
# discriminable? perpPara_170725 and thinOb_150823 also have control-phase
# trials in `choices`; filtering to manipulation == "test" keeps the
# comparison to discrimination performance only, matching how mTest/mTc are
# split in 2_fit_models.R.
# Produces: mArtCompare, emmArtCompare, artContrastLink, artContrastResp
# -----------------------------------------------------------------------------

# trial offset so 0 = the first trial, as in the nat/art model below. This
# makes the `experiment` main effect the difference at trial 1 rather than at
# trial 0 (outside the data), and keeps the slope in per-trial units.
#
# reward_side interacts with experiment because the side bias runs in opposite
# directions in the two experiments (own models: perpPara right = -0.84,
# thinOb right = +2.19 log-odds; raw P_corr thinOb 45% on left-reward vs 88%
# on right-reward trials). A single shared reward_side coefficient fits
# neither and pushes the misfit into the experiment-specific intercept/slope,
# which is why the earlier version of this model put thinOb at 75% at trial
# 47 while its own within-experiment model gives 80%. With the interaction the
# two agree (80.6 vs 80.7%); LRT vs the shared-side model: chi2(1) = 102.
# bobyqa clears lme4's max|grad| warning; estimates are unchanged.
artCompare <- choices %>%
  filter(experiment %in% c("perpPara_170725", "thinOb_150823"),
         manipulation == "test") %>%
  mutate(trial_m1 = rank_trial - 1)

mArtCompare <- glmer(
  decision ~ trial_m1 * experiment + sideAlt_trial + reward_side * experiment +
    (1 | individual),
  data = artCompare, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# Predicted P_corr per experiment at trial 47 (on the offset scale, 46), the
# max test-phase trial common to both experiments (perpPara: 47, thinOb: 66)
emmArtCompare <- emmeans(mArtCompare, ~ experiment, at = list(trial_m1 = 47 - 1),
                         type = "response")

# Planned contrast: perpendicular/parallel vs. thin oblique -- with only two
# experiment levels this is a plain pairwise comparison, no custom weights needed.
artContrast <- contrast(emmArtCompare, method = "pairwise", type = "response")

artContrastLink <- summary(artContrast, infer = c(TRUE, TRUE))                     # log-odds-ratio scale, for the p-value/CI
artContrastResp <- summary(artContrast, type = "response", infer = c(TRUE, TRUE))  # back-transformed odds ratio, for reporting

# -----------------------------------------------------------------------------
# Q: artificial vs. naturalistic stimuli -- does discrimination performance
# depend on stimulus category? Pools all core experiments (test phase only).
# Unlike the comparison above, this is well-posed as a genuine fixed effect:
# each category (4 artificial, 7 naturalistic experiments) has real
# within-category replication, so (1 | experiment) can separate the category
# effect from ordinary experiment-to-experiment variation, with
# (1 | experiment:individual) accounting for individuals being nested within
# (not shared across) experiments.
#
# reward_side also gets a random slope by experiment: side biases differ in
# direction among experiments (see the Eq. 3 note above and Table S3), so one
# shared reward_side coefficient is a compromise that fits none of them.
# A fixed reward_side x experiment term is not an option here -- experiment is
# nested in nat_or_art, so the design is rank-deficient. The random-slope fit
# converges and is not singular; the experiment-level intercept/slope
# correlation is strongly negative (~ -0.97), which is the same fact restated:
# experiments that do well on left-reward trials do worse on right. Versus
# the shared-side model: delta-AIC = -157; category contrast at trial 26 moves
# from OR 2.45 to 2.74. bobyqa clears lme4's max|grad| warning.
#
# trial is offset so that 0 = the first trial (trial_m1 = rank_trial - 1).
# This makes the `nat_or_art` main effect the artificial-vs-naturalistic
# difference at trial 1 -- a real, in-data reference point -- rather than at
# trial 0, which lies outside the data. It also keeps the trial slope in
# per-trial units, directly comparable to the within-experiment models.
#
# NOTE: this fit emits lme4's "nearly unidentifiable: very large eigenvalue"
# warning. It is benign and purely a numerical-conditioning artefact of trial
# spanning 1-131 while every other predictor is a 0/1 dummy (eigenvalue ratio
# ~1.3e5 vs ~1e2 when trial is standardised). Verified: raw, trial-1,
# mean-centred and standardised codings all give an identical trial slope
# and an identical odds ratio at trial 26 (verified on the shared-side
# version of this model; bobyqa leaves this note but no gradient warning). Standardising
# silences the warning but puts the slope in per-SD (27.1-trial) units, which
# is worse for the reported coefficient table.
# Produces: mNatArtCompare, emmNatArtCompare, natArtContrastLink, natArtContrastResp
# -----------------------------------------------------------------------------

natArtCompare <- choices %>%
  filter(manipulation == "test") %>%
  mutate(trial_m1 = rank_trial - 1)

mNatArtCompare <- glmer(
  decision ~ trial_m1 * nat_or_art + sideAlt_trial + reward_side +
    (1 + reward_side | experiment) + (1 | experiment:individual),
  data = natArtCompare, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# Trial 26 (the max test-phase trial shared by all core experiments), on the
# offset scale, matching the convention already used in fig-q3-combined.
emmNatArtCompare <- emmeans(mNatArtCompare, ~ nat_or_art, at = list(trial_m1 = 26 - 1))

# Planned contrast: artificial vs. naturalistic -- two-level factor, plain
# pairwise comparison.
natArtContrast <- contrast(emmNatArtCompare, method = "pairwise")

natArtContrastLink <- summary(natArtContrast, infer = c(TRUE, TRUE))
natArtContrastResp <- summary(natArtContrast, type = "response", infer = c(TRUE, TRUE))

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(
  mArtCompare,
  emmArtCompare,
  artContrastLink,
  artContrastResp,
  mNatArtCompare,
  emmNatArtCompare,
  natArtContrastLink,
  natArtContrastResp,
  file = "scripts/intermediate_outputs/experiment_comparisons.RData"
)

message("experiment_comparisons.RData saved.")
