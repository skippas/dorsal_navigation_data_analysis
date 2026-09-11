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
library(ggtext)

# knitr::kable()/kableExtra normally auto-detect "latex" as the output format
# from the live Quarto rendering session, via knitr::is_latex_output()
# (which checks opts_knit$get("rmarkdown.pandoc.to")). This script isn't run
# inside one (it's a plain Rscript), so without this, tables silently fall
# back to a generic format that doesn't escape LaTeX special characters
# (%, _) -- a real correctness bug, not just a cosmetic one. Note: pass
# format = "latex" explicitly to kable() and this breaks in a different way
# (bypasses kableExtra's LaTeX-aware code path entirely) -- auto-detection
# via this option is the one that actually works.
knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")

load("scripts/intermediate_outputs/analysis_results.RData")
load("scripts/intermediate_outputs/experiment_comparisons.RData")

source("functions/format_numeric_cols.R")
source("functions/make_confint_col.R")
source("functions/format_tables.R")
source("scripts/plotting/plot_label_lookups.R")   # -> artLabels, natcanLabels, natcanImgLabels, artImgLabels

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

source("scripts/format_results_helpers/format_contTc.R")   # -> contTcRef, contTcTbl

contrastsRef <- contTcRef
contrastsTbl <- contTcTbl

# -----------------------------------------------------------------------------
# Format the perpendicular/parallel vs. oblique planned comparison
# Produces: artCompareRef
# -----------------------------------------------------------------------------

source("scripts/format_results_helpers/format_artCompare.R")   # -> artCompareRef
source("scripts/format_results_helpers/format_natArtCompare.R")   # -> natArtCompareRef
source("scripts/format_results_helpers/format_sideBias.R")      # -> sideBiasRef

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
# Format the overview and sample-size tables
# Produces: tblOverviewStr, tblSampleSizesStr
# -----------------------------------------------------------------------------

source("scripts/format_results_helpers/format_overview_and_samplesizes.R")
source("scripts/format_results_helpers/format_tbl_model_coefs.R")   # -> tblModelCoefsStr
source("scripts/format_results_helpers/format_tbl_cross_coefs.R")   # -> tblCrossCoefsStr

# Write each table body out as a standalone .tex fragment. These are copied
# into the manuscript's tables/ directory by render_figures.sh and pulled in
# with \input{} from results_supplementary.tex, which supplies the table
# environment, caption, label and landscape wrapper. This keeps the values
# and column headers single-sourced from R -- the supplementary section is
# otherwise hand-maintained (sync_results_prose.sh stops at the Supplementary
# heading), so pasted tables there would silently drift.
dir.create("scripts/intermediate_outputs/tables", showWarnings = FALSE, recursive = TRUE)
writeLines(tblOverviewStr,    "scripts/intermediate_outputs/tables/tbl_overview.tex")
writeLines(tblSampleSizesStr, "scripts/intermediate_outputs/tables/tbl_sample_sizes.tex")
writeLines(tblModelCoefsStr,  "scripts/intermediate_outputs/tables/tbl_model_coefs.tex")
writeLines(tblCrossCoefsStr,  "scripts/intermediate_outputs/tables/tbl_cross_coefs.tex")

# -----------------------------------------------------------------------------
# Naturalistic vs. artificial end-of-test summary stats (used inline in the
# "Performance differences among naturalistic and artificial stimuli" prose)
# Produces: natArtMean, q3NatBest, q3ArtWorst
# -----------------------------------------------------------------------------

q3NatArtEndPts <- emmPts %>%
  filter(trial_pos == "last_test") %>%
  mutate(
    nat_or_art = factor(nat_or_art, levels = c("naturalistic", "artificial"))
  )

natArtMean <- q3NatArtEndPts %>%
  group_by(nat_or_art) %>%
  summarise(
    mean_prob = mean(prob),
    min_prob  = min(prob),
    max_prob  = max(prob),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = nat_or_art,
    values_from = c(mean_prob, min_prob, max_prob),
    names_glue  = "{nat_or_art}_{.value}"
  ) %>%
  mutate(diff_mean_prob = artificial_mean_prob - naturalistic_mean_prob) %>%
  mutate(across(ends_with("_prob"), ~as.character(as.integer(round(. * 100)))))

q3NatBest <- q3NatArtEndPts %>%
  filter(nat_or_art == "naturalistic") %>%
  slice_max(order_by = prob, n = 1, with_ties = FALSE) %>%
  mutate(prob  = as.character(as.integer(round(prob * 100))),
         label = unname(natcanLabels[experiment]))

q3ArtWorst <- q3NatArtEndPts %>%
  filter(nat_or_art == "artificial") %>%
  slice_min(order_by = prob, n = 1, with_ties = FALSE) %>%
  mutate(prob  = as.character(as.integer(round(prob * 100))),
         label = unname(artLabels[experiment]))

# -----------------------------------------------------------------------------
# Build figures
# Produces: fig_supp_canopyG1, fig_q2, fig_q3_tc, fig_q3_combined
# -----------------------------------------------------------------------------

library(patchwork)

source("scripts/plotting/add_row_letters.R")   # -> add_row_letters(): per-row A/B/C tags
source("scripts/plotting/make_fig_supp_canopyG1.R")
fig_supp_canopyG1 <- make_fig_supp_canopyG1(
  emmAll, emmPts, choices_rolling, nLabels, natcanLabels, natcanImgLabels
)

source("scripts/plotting/make_fig_q2.R")
fig_q2 <- make_fig_q2(emmAll, emmPts, choices_rolling, nLabels, artLabels, artImgLabels)

source("scripts/plotting/make_fig_q3_tc.R")
fig_q3_tc <- make_fig_q3_tc(emmAll, emmPts, choices_rolling, nLabels, natcanLabels, natcanImgLabels)

source("scripts/plotting/make_fig_q3_combined.R")
fig_q3_combined <- make_fig_q3_combined(emmAll, artLabels, natcanLabels)

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
  artCompareGrp,
  natArtCompareRef,
  natArtCompareGrp,
  sideBiasRef,
  q2ArtTableOrder,
  choices_rolling,
  nLabels,
  nSummary,
  fig_supp_canopyG1,
  fig_q2,
  fig_q3_tc,
  fig_q3_combined,
  natArtMean,
  q3NatBest,
  q3ArtWorst,
  tblOverviewStr,
  tblSampleSizesStr,
  tblModelCoefsStr,
  tblCrossCoefsStr,
  plotStyle,
  file = "scripts/intermediate_outputs/formatted_results.RData"
)

message("formatted_results.RData saved.")
