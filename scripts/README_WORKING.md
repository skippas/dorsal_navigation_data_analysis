# Working README

Last updated: 2026-02-23
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

## Next actions (suggested)
1. Render `glm_analysis.qmd` and confirm Q3 tables are formatted as intended.
2. Decide whether to keep/remove `middleTrial` in `testRes$emmPtsMsTab` where not needed.
3. Add final manuscript-facing column labels/captions for all `MsTab` tables.
4. Trim or remove obsolete exploratory text blocks in Q2/Q3.

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
