# Plan — drop `sideAlt_trial` from all models

Status: **done** (2026-09-11), except the prose re-sync noted at the bottom.
Steps 1-7 and the Methods edits in step 8 are complete.

## Outcome of the sensitivity check (step 2)

Every conclusion survived. Refitting all models with and without the term:

| Result | With | Without | Verdict |
|---|---|---|---|
| perpPara trial x phase | p = 0.089 | p = 0.096 | still non-significant |
| Canopy G test-end vs ctrl-start | p = 0.021 | p = 0.018 | still significant |
| Eq. 3 perp/para vs thin oblique | p = 0.044 | p = 0.045 | still significant |
| Eq. 4 artificial vs naturalistic | p < 0.001 | p < 0.001 | unchanged |
| "three of the six" canopies above chance | E, G sig.; I borderline | same | unchanged |

Two test-end CI lower bounds cross 50% by ~0.2 percentage points, in
opposite directions: brightDiff 50.03% -> 49.87% and the Canopy G pilot
49.96% -> 50.18%. Neither changes a stated claim — the brightDiff prose
already hedges ("only borderline significantly higher than 50%") and both
still display as 50% once rounded to whole percent; the Canopy G pilot is
supplementary and carries no significance claim. The real lesson is that
those two above-chance calls were never robust either way.

The "nearly unidentifiable: very large eigenvalue" warning on Eq. 4 is
**pre-existing** — it appears identically with and without the term.

---

Original plan follows.

## Rationale

`sideAlt_trial` (rewarded side differs from the previous trial) is a nuisance
term with no explanatory power anywhere in the analysis. In `tbl_model_coefs`
all 11 within-experiment models give |log-odds| < 0.39 with 95% CIs spanning
zero and p between 0.18 and 0.79; both cross-experiment models (Eq. 3, Eq. 4)
are the same (p = 0.78 and 0.15). It costs a df, a table row in every
experiment block of two supplementary tables, and a sentence of methods prose
that a reviewer may ask about — for nothing.

## Decisions to make before starting

1. **Keep the variable, drop the term.** Leave `identify_sideAlt_trials()`
   (`functions/transition_trials.R`) and its call in `1_loading_cleaning.R` in
   place. It is cheap, and keeping `sideAlt_trial` in `choices` means the
   sensitivity check below is a one-line refit rather than a pipeline change.
   Only the model formulas change.
2. **How to justify it in Methods.** Recommended: one sentence saying the term
   was fitted initially, had a negligible effect in every model, and was
   dropped. This is more defensible than silent removal — the experimental
   design (randomised reward side, no runs > 3 trials) already explains why it
   was considered in the first place, and that text is already in
   `methods_procedure.tex`.
3. **Do it before or after re-running everything?** Do the sensitivity
   comparison (step 2) *first*, so the decision is evidence-backed and the
   refit isn't wasted work if something moves.

## Step 1 — commit current state

The pipeline outputs are the baseline for the comparison in step 2. Commit
`scripts/5_results_report.qmd` (currently modified) before touching anything.

## Step 2 — sensitivity check (do this first)

Before editing any formula, refit with and without the term and confirm nothing
material moves. Write a throwaway script (not part of the pipeline) that:

- refits `mTest` / `mTc` per experiment with and without `sideAlt_trial`
- compares, per experiment: the `rank_trial` slope, the test-end predicted
  P_corr and its 95% CI lower bound (the above-chance criterion), and the
  test-end vs. control-start contrast estimate and p-value
- does the same for `mArtCompare` and `mNatArtCompare`

**Watch these specifically — they are the results that could flip:**

| Result | Current value | Why it's at risk |
|---|---|---|
| perpPara `rank_trial:manipulation` | p = 0.089 | already marginal |
| Canopy F2 test-end vs ctrl-start | p = 0.021 | only significant natcan contrast |
| Any test-end CI lower bound near 50% | — | above-chance calls are CI-based |
| nat vs. art category contrast (Eq. 4) | — | headline Q3 result |

If a conclusion flips, stop and reconsider — that turns this from a
simplification into a substantive analysis change.

## Step 3 — edit the model formulas

Seven formulas, all a straight deletion of `+ sideAlt_trial`:

| File | Line (approx) | Object |
|---|---|---|
| `scripts/2_fit_models.R` | 53 | `mTest` |
| `scripts/2_fit_models.R` | 97 | `mTc` |
| `scripts/cross_experiment_comparisons.R` | 65 | `mArtCompare` (Eq. 3) |
| `scripts/cross_experiment_comparisons.R` | 127 | `mNatArtCompare` (Eq. 4) |
| `scripts/4_run_supp_analysis.R` | 54 | `mPpPostNatcan` |
| `scripts/4_run_supp_analysis.R` | 177 | `mThickObDiff` |
| `scripts/4_run_supp_analysis.R` | 183 | `mThickObDiffTest` |

Note `mArtCompare` keeps `reward_side * experiment` — only the `sideAlt_trial`
term goes.

## Step 4 — formatting layer

- `scripts/format_results_helpers/coef_term_labels.R`: remove the
  `"sideAlt_trialTRUE" = "Side alternation"` entry from `coefTermLabels` and
  `"Side alternation"` from `coefTermOrder`. Both `labelCoefTerms()` and
  `orderCoefTerms()` pass unknown terms through, so a missed model would show
  up as a raw `sideAlt_trialTRUE` row rather than disappearing silently — that
  is the intended safety net, leave it.
- No other formatter references the term. `format_sideBias.R` is about
  `reward_side`, not alternation — **do not touch it**.

## Step 5 — re-run the pipeline

```
Rscript scripts/0_run_all.R
```
then render the report and push figures/tables to the manuscript:
```
writing/overleaf_manuscript/wasp_dorsal_snapshot_discrimination/scripts/render_figures.sh
```

`tbl_model_coefs.tex` and `tbl_cross_coefs.tex` are generated, so the
"Side alternation" rows disappear from the supplementary tables automatically.
No hand-editing of those two files.

## Step 6 — report and repo docs

- `scripts/5_results_report.qmd` line ~325: the `tbl-cap` for the coefficient
  table lists "trial number, side alternation, reward side, ..." — drop
  "side alternation". This caption is what ends up in `tbl_model_coefs.tex`.
- `CLAUDE.md` lines 127, 133, 138: the two documented model formulas and the
  "Key predictors" sentence. (This file also still describes the old
  `scripts/glm_analysis/` layout — worth fixing in the same pass, but that is
  a separate concern.)
- `TODO.md`: the bullet asking for an explanation of `sideAlt_trial` in the
  results/methods becomes moot; replace it with the Methods sentence from
  decision 2.

## Step 7 — verify

- Confirm no `sideAlt` rows survive: `grep -c "Side alternation" tables/*.tex`
  should be 0, and `grep -rn "sideAlt" scripts/ functions/` should return only
  `transition_trials.R` and the `1_loading_cleaning.R` call.
- Re-read the inline numbers in the rendered report against the step-2
  comparison — every inline `r` value in the manuscript comes from the refitted
  models, so they all change in the last decimal even where conclusions don't.

---

## Step 8 — manuscript prose (AFTER the steps above are done)

The generated tables take care of themselves; **the prose does not.** Nothing
below should be edited until the refit above has actually been run, because the
Methods wording depends on what the sensitivity check showed.

Repo: `writing/overleaf_manuscript/wasp_dorsal_snapshot_discrimination/`
(separate from this git repo — not version-controlled alongside the analysis).

### `sections/methods_procedure.tex` — five edits

1. **Eq. 1 (`eq:mtest`, ~line 195)** — remove `+ \mathrm{sideAlt}`.
2. **Eq. 2 (`eq:mtc`, ~line 202)** — remove `+ \mathrm{sideAlt}`.
3. **The "where decision is..." paragraph (~line 206)** — remove the
   `\textit{sideAlt} flags trials where the rewarded side differed from the
   preceding trial` clause, and the `and the side-alternation term quantifies
   the extent to which wasps prefer the previously rewarded side` clause. Keep
   the reward-side and trial-number explanations intact. Add the one-sentence
   justification from decision 2 here.
4. **Eq. 3 (`eq:martcompare`, ~line 223)** — remove `+ \mathrm{sideAlt}`.
5. **Eq. 4 (`eq:natartcompare`, ~line 231)** — remove `+ \mathrm{sideAlt}`.

### `sections/results_body.tex` — nothing to remove

Checked: no mention of side alternation. The inline numbers do change, but they
come through the render pipeline, not by hand.

### Do NOT bother with `sections/results.tex`

It contains a stale hardcoded copy of the supplementary coefficient tables with
raw term names (`sideAlt\_trialTRUE`, `rank\_trial`, ...), but `main.tex` does
not `\input` it — the live supplementary comes from `results_supplementary.tex`,
which `\input`s the generated `tables/*.tex`. That file is dead. Deleting it
outright is the right move, but it is a separate cleanup, not part of this task.

### OUTSTANDING — the prose re-sync

`sections/results_body.tex` was deliberately **not** re-synced. It is generated
from `5_results_report.qmd`, and the manuscript's current copy was synced from
the *uncommitted* working-copy version of that file, which is ahead of what is
committed on this branch. Re-syncing from this branch's render would have
reverted that newer prose, so the file was left untouched — its inline numbers
are still the pre-refit ones while the tables are post-refit.

To finish, from the main checkout once this branch is merged:

```
writing/overleaf_manuscript/wasp_dorsal_snapshot_discrimination/scripts/render_figures.sh
writing/overleaf_manuscript/wasp_dorsal_snapshot_discrimination/scripts/sync_results_prose.sh
```

That regenerates the prose from the working-copy qmd against the refitted
models. Expect small shifts only: e.g. the brightness trial p-value
0.356 -> 0.381, and the Eq. 3 odds ratio 2.512 -> 2.484 (p 0.044 -> 0.045).

### Numbers to re-check by eye after the refit

Every inline statistic in Results and the Abstract comes from the refitted
models. Focus the read-through on the four at-risk results listed in step 2 —
if the sensitivity check was clean, the rest will only move in the last
decimal, but the Abstract's headline numbers should still be confirmed against
the regenerated report.
