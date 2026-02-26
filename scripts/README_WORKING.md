# Working README

Last updated: 2026-02-26
Primary script: `glm_analysis.qmd`

## Where I left off
- Standardized naming so inline-format objects use the `InLine` suffix (replacing `Disp`).
- Renamed TC contrast objects for consistency (`contTcLn`, `contTcRsp`, `contTcLnInLine`).
- Split cleanup so each master formatting chunk removes only its own temporary objects.
- Isolated plotting styles into a dedicated environment and retained only one style object via `plotStyle$themeTextEnlarged`.
- Added master manuscript tables:
  - `testRes$emmPtsMsTab` (test start/mid/end points)
  - `tcRes$emmPtsMsTab` (test-end vs control-start points)
- Refactored Q3 tables to consume master `MsTab` objects:
  - `q3TablePts` now uses `testRes$emmPtsMsTab`
  - `q3TableTcPts` now uses `tcRes$emmPtsMsTab`

## Current output objects to rely on
- `testRes`
  - `emmAll`, `emmPts`, `emmPtsInLine`, `emmPtsMsTab`
  - `mCoefs`, `mCoefsInLine`, `trialSlopeInLine`
- `tcRes`
  - `emmAll`, `emmPts`, `emmPtsInLine`, `emmPtsMsTab`
  - `mCoefsInLine`, `contrastsInLine`, `trialSlopeInLine`

## TODO (single source of truth)
When an item is completed:
1. check it off here
2. remove its TODO comment from the source script

### Active in `glm_analysis/glm_analysis.qmd`
- [x] `[MEDIUM]` Remove custom macro usage and use `$P_{\mathrm{corr}}$` text/math directly.
- [ ] `[MEDIUM]` Remove odd dash formatting in Q1 text/results.
- [ ] `[MEDIUM]` Remove top-of-document results block that should not be in manuscript flow.
- [ ] `[MEDIUM]` Fill missing statistics in results text.
- [ ] `[HARD]` Run overall naturalistic vs artificial comparison using all data.
- [ ] `[MEDIUM]` Merge Q3 test points and TC tables.
- [ ] `[HARD]` Evaluate/use TC-only model strategy throughout.
- [ ] `[MEDIUM]` Add end-trial estimates + CIs to all remaining Q2/Q3 figures.
- [ ] `[EASY]` Finalize axis labels for all figures.
- [ ] `[MEDIUM]` Make table headers + row names publication-ready.
- [ ] `[MEDIUM]` Replace experiment IDs with publication-ready names.
- [ ] `[MEDIUM]` Add diffuser experiment data to supplementary.
- [ ] `[HARD]` Add naturalistic -> artificial transition experiment analysis.

### Supplementary formatting TODOs (`glm_analysis/glm_analysis.qmd`)
- [ ] `[EASY]` Verify S1 formatting target (2 decimals + combined `mid, end trial #`) and remove stale TODO if done.
- [ ] `[MEDIUM]` Improve S2 layout (split by experiment, order by `nat_or_art`).
- [ ] `[EASY]` Keep supplementary tables consistently ordered by `nat_or_art`.
- [ ] `[EASY]` Standardize CI separator style (`,` vs `;`) across supplementary tables.
- [ ] `[MEDIUM]` Decide whether to merge `logOdds` and `confint` into one display column.
- [ ] `[MEDIUM]` Decide whether to merge TC contrast and TC points tables.
- [ ] `[MEDIUM]` Clarify/report control coefficient presence in TC model results.
- [ ] `[EASY]` Rename display label `testEndTrial` -> `TrEnd`.
- [ ] `[MEDIUM]` Reduce repeated experiment labels in model-result tables.
- [ ] `[EASY]` Finalize supplementary captions and S-numbering.

### Formatting-file TODOs
- [ ] `[EASY]` Decide and enforce final significant digits in test points formatter.
  Source: `glm_analysis/formatting_tables/test/format_emmTestPts.R`
- [ ] `[MEDIUM]` Finish TC formatter refactor pass.
  Source: `glm_analysis/formatting_tables/test/format_emmTestPts.R`
- [ ] `[MEDIUM]` Ensure caption strategy is consistent across formatter outputs.
  Source: `glm_analysis/formatting_tables/test/format_emmTestPts.R`
- [ ] `[MEDIUM]` Investigate missing `manipulation` term in TC results.
  Source: `glm_analysis/formatting_tables/test/format_emmTestPts.R`
- [ ] `[MEDIUM]` Complete remaining Q3 writeup follow-through.
  Source: `glm_analysis/formatting_tables/test/format_emmTestPts.R`

### Legacy TODOs (outside current `glm_analysis` flow)
- [ ] `[MEDIUM]` Ensure control trials are handled separately in old binning analysis.
  Source: `analysis.Rmd`
- [ ] `[EASY]` Decide whether low-`n` bin filtering should be removed in old workflow.
  Source: `analysis.Rmd`
- [ ] `[HARD]` Enter/plot previous experiments and include first control with training-protocol notes.
  Source: `analysis.Rmd`
- [ ] `[MEDIUM]` Build cohort tile plot with day/experiment transition annotations.
  Source: `analysis.Rmd`
- [ ] `[MEDIUM]` Replace ambiguous individual numbers with stable codes.
  Source: `loading_cleaning.R`

## Fast Monday restart checklist
1. `git status`
2. `git log --oneline -n 10`
3. Open `glm_analysis.qmd` and jump to these chunks:
   - `formatTestTables`
   - `formatTcDataStructures`
   - `q3TablePts`
   - `q3TableTcPts`
4. Render once and fix only errors/warnings first.

## Notes
- Quarto rendering is currently done in your RStudio session on your machine.
- This file is intentionally short; update it at the end of each work session.
