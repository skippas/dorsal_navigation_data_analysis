# CLAUDE.md — Dorsal Snapshot Discrimination Analysis

## Project overview

PhD analysis project (Andres Cheepers, Lund University). Wasp Y-maze behavioural
experiments testing whether wasps can use dorsal (overhead) visual patterns to
navigate to a reward. Experiments vary the stimulus type: artificial patterns
(perpendicular/parallel stripes, oblique stripes, brightness difference) and
naturalistic canopy photographs.

The analysis document is `scripts/glm_analysis/glm_analysis.qmd`, structured
around three research questions:
- **Q1**: Can wasps discriminate a simple artificial dorsal pattern?
- **Q2**: What information are wasps using (which artificial features)?
- **Q3**: Can wasps use naturalistic canopy patterns?

---

## Pipeline — three-script architecture

Run in order from the project root (`data_analysis.Rproj`):

1. **`scripts/glm_analysis/run_analysis.R`** — fits all models, computes emmeans,
   saves `scripts/glm_analysis/analysis_results.RData`
2. **`scripts/glm_analysis/format_results.R`** — loads the RData, runs formatting
   scripts, saves `scripts/glm_analysis/formatted_results.RData`
3. **`scripts/glm_analysis/glm_analysis.qmd`** — loads `formatted_results.RData`
   only; contains no modelling or formatting code

The QMD will fail to render if the two R scripts have not been run first.

---

## Key data objects (available in QMD after loading formatted_results.RData)

### Prediction curves
- **`emmAll`** — one row per `(experiment, manipulation, rank_trial)`. The full
  predicted probability curve from the model. Used for `geom_line()` and
  `geom_ribbon()`. `manipulation` is a factor with levels `c("test", "control")`
  — this ordering is set at source in `run_analysis.R` and must not be
  overridden without good reason.

### Predicted key points
- **`emmPts`** — long format, one row per `(experiment, trial_pos)`. Four
  `trial_pos` values: `first`, `middle`, `last_test`, `ctrl_start`.
  `ctrl_start` rows are absent for test-only experiments. Used for
  `geom_point()` + `geom_errorbar()` on figures.
- **`emmPtsInLine`** — wide format, one row per experiment (rownames = experiment
  ID). Columns named `{trial_pos}_{prob/confint/rank_trial}`, e.g.
  `last_test_prob`, `ctrl_start_confint`. Used for inline `r` text references.
- **`emmPtsMsTab`** — manuscript-ready wide table with pre-formatted string
  columns (`lastTestPcorr95CI`, `ctrlStartPcorr95CI`, `lastTestTrial`, etc.).
  Used for `knitr::kable()` tables.

### Rolling means (raw data summary)
- **`choices_rolling`** — 3-trial rolling mean of `decision` per
  `(individual, experiment, nat_or_art, manipulation)`, then averaged across
  individuals. Used for the grey background points in figures (`geom_point`,
  `alpha = 0.1`).

### Model results — test-only experiments (`testRes`)
- `testRes$mCoefs` — raw tidy output
- `testRes$mCoefsInLine` — wide, rownames = experiment
- `testRes$mCoefsMsTab` — manuscript table
- `testRes$trialSlopeInLine` — `rank_trial` coefficient only, rownames = experiment
- `testRes$trialSlopeMsTab` — manuscript table for slopes

### Model results — test+control experiments (`tcRes`)
- `tcRes$mCoefsInLine` — wide, rownames = experiment
- `tcRes$mCoefsMsTab` — manuscript table
- `tcRes$contrastsInLine` — test-end vs control-start contrast, rownames = experiment
- `tcRes$contrastsMsTab` — manuscript table for contrasts
- `tcRes$trialSlopeInLine` — rownames = experiment
- `tcRes$trialSlopeMsTab` — manuscript table for slopes

### Plotting style
- **`plotStyle$themeTextEnlarged`** — custom ggplot2 theme, use instead of
  `theme_bw()` in Q1 and Q2 figures

---

## Formatting scripts (`scripts/glm_analysis/formatting_tables/`)

| File | Produces |
|---|---|
| `format_emmPts.R` | `emmPtsInLine`, `emmPtsMsTab` |
| `format_mTestTidy.R` | `testRes` components |
| `format_mTcTidy.R` | `tcRes` model coef components |
| `format_contTcLn.R` | `tcRes` contrast components |

---

## Helper functions (`functions/`)

- `format_numeric_cols.R` — `fmtNumCols()`: rounds and formats numeric columns
- `make_confint_col.R` — `make_confint_col()`: combines lower/upper CI into `[x, y]` string
- `format_tables.R` — shared table utilities

---

## Figure conventions

All faceted figures use:
- `geom_ribbon` + `geom_line` from `emmAll` for the prediction curve
- `geom_point(alpha = 0.1)` from `choices_rolling` for grey rolling-mean points
- `geom_errorbar` + `geom_point` from `emmPts` (filtered to relevant `trial_pos`) for black predicted endpoint markers
- `panel.spacing.y = unit(1.5, "lines")` in `theme()` for vertical panel spacing
- `facet_grid(experiment ~ manipulation, scales = "free_x")` for multi-experiment TC figures

For TC figures, filter `choices_rolling` to only experiments with both phases:
```r
expTestAndCtrl <- choices_rolling %>%
  group_by(experiment) %>%
  filter(n_distinct(manipulation) > 1) %>%
  pull(experiment) %>%
  unique()
```

---

## Model structure

Two model classes fitted in `run_analysis.R`:

**`mTest`** — test-phase only, for experiments without a control:
```r
glmer(decision ~ rank_trial + reward_side + (1 | individual),
      data = .x, family = binomial)
```

**`mTc`** — test + control, for experiments with both phases:
```r
glmer(decision ~ rank_trial * manipulation + reward_side + (1 | individual),
      data = .x, family = binomial)
```

Key predictors: `rank_trial` (learning slope), `manipulation` (test vs control),
`reward_side` (side bias). Day, reward duration and `sideAlt_trial` (side
alternation) were considered but excluded — `sideAlt_trial` had a negligible
effect in every model and was dropped (see `misc/plan_drop_sideAlt.md`). The
variable is still computed in `1_loading_cleaning.R` so the sensitivity check
stays a one-line refit; only the model terms were removed.

---

## Experiment IDs and groupings

- **Artificial** (`nat_or_art == "artificial"`): `perpPara_170725`,
  `thinOb_150823`, `brightDiff_250725`
- **Naturalistic** (`nat_or_art == "naturalistic"`): multiple `natcan_*` experiments
- Experiments excluded from this analysis (filtered in `run_analysis.R`):
  `thick_oblique_apis`, `thickOb_140823`, `thickObDiff_210924`,
  `perpPara_240723`, `perpParaPostNatcan_090925`

Q2 display order: `c("thinOb_150823", "brightDiff_250725")`
Q2 table order: `c("perpPara_170725", "thinOb_150823", "brightDiff_250725")`

---

## Deadline focus

Andres is under submission pressure. When we work together, prioritise ruthlessly:

- **Call out rabbit holes directly** — if a session drifts into formatting, LaTeX tweaks, or tooling tinkering that isn't blocking submission, say so plainly and redirect to prose or content work.
- **Submission over perfection** — good enough figures and tables now beat perfect ones after the deadline.
- **Prompt task transitions** — if we've been on one thing for a while, ask whether it's time to move on.
- **Keep focus on content** — the priority order is: (1) complete missing prose/sections, (2) fix content errors, (3) fix figures, (4) polish formatting.

---

## Workflow notes

- Always run `run_analysis.R` then `format_results.R` before rendering the QMD
- Git branch for current work: `refactor-plotting-code`
- `manipulation` factor levels are set once in `run_analysis.R` — do not re-specify in figure chunks unless overriding for a specific reason
- `emmPts` "Long" suffix (e.g. `emmNatTcPts`) = tidy format for plotting; no suffix or "InLine" = wide format for inline text

## Git workflow
- Always commit all current changes before beginning any new modifications.
