# Methods — Statistical Analysis
*Draft written by Claude based on the analysis pipeline in `run_analysis.R` and `loading_cleaning.R`. Review all claims carefully before using.*

---

## Data preparation

Only the first decision recorded per trial per individual was retained for analysis. Trials in which the recorded decision was ambiguous or non-numeric were excluded. Within each experiment and manipulation phase, trials were ranked sequentially per individual (`rank_trial`), so that trial 1 always refers to the first trial an individual experienced in that phase regardless of calendar date.

A side-alternation indicator (`sideAlt_trial`) was computed for each trial, flagging cases where the rewarded side differed from the previous trial. This predictor was included to account for spontaneous side-switching behaviour that is independent of the stimulus.

## Excluded experiments

The following experiments were excluded from the primary analysis: `thickOb_140823`, `thickObDiff_210924`, `perpPara_240723`, `thick_oblique_apis`, and `perpParaPostNatcan_090925`. One trial (trial 34) was removed from `perpPara_170725` due to a procedural error during data collection. For `natcan1_170923`, data collected on the final date (20 September 2023) were excluded because those trials were run after the control phase had already begun, which may have influenced subsequent performance.

## Model structure

We fitted separate generalised linear mixed models (GLMMs) for each experiment using the `lme4` package (Bates et al., 2015) in R. The binary response variable was whether the individual chose the rewarded arm on each trial (1 = correct, 0 = incorrect), modelled with a binomial error distribution and a logit link function.

Two model structures were used depending on whether an experiment included both a test phase and a control phase:

**Test-only model** (experiments without a control phase):

```
decision ~ rank_trial + sideAlt_trial + reward_side + (1 | individual)
```

**Test + control model** (experiments with both phases):

```
decision ~ rank_trial × manipulation + sideAlt_trial + reward_side + (1 | individual)
```

In both models, `rank_trial` captures the within-phase learning trajectory (the slope of improvement over trials). In the test + control model, the `rank_trial × manipulation` interaction allows the slope to differ between the test and control phases, with `manipulation` coded as a factor with levels `test` (reference) and `control`.

`reward_side` was included as a fixed effect to account for any systematic side bias (left vs. right preference). Individual identity was included as a random intercept to account for repeated measures within individuals and baseline differences in performance.

Day of testing and reward duration were considered as additional predictors but were not included in the final models.

## Inference

Significance of the effect of trial number on performance was assessed from the Wald z-test for the `rank_trial` coefficient in each model. Model coefficients and 95% confidence intervals were extracted using the `broom.mixed` package.

For experiments with a control phase, we additionally tested whether performance at the end of the test phase differed significantly from performance at the start of the control phase. This contrast was computed on the log-odds scale using the `emmeans` package (Lenth, 2023), comparing the model-predicted log-odds at the last observed test trial against the model-predicted log-odds at the first control trial.

## Predicted probabilities

Marginal predicted probabilities of a correct choice were obtained from each fitted model using `emmeans`, marginalising over the observed trial range while holding nuisance predictors (`sideAlt_trial`, `reward_side`) at their reference values. These predicted probability curves and their 95% confidence intervals are shown in the figures. Specific predicted values are reported at three trial positions within the test phase: the first trial, the midpoint trial (ceiling of the midpoint between first and last), and the final trial. For experiments with a control phase, the predicted probability at the first control trial is also reported.

## Software

All analyses were conducted in R (version ≥ 4.0). Key packages: `lme4` for model fitting, `emmeans` for marginal predictions and contrasts, `broom.mixed` for tidying model output, `tidyverse` for data processing.
