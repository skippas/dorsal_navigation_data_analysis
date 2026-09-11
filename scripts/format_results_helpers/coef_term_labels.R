# Publication-ready display names for model coefficient terms.
#
# Shared by format_mAllCoefs.R (Table S3, within-experiment models) and
# format_tbl_cross_coefs.R (Table S4, cross-experiment models) so the two
# tables cannot drift apart.
#
# Raw lme4 term names are variable names, not English -- and for factor
# terms lme4 concatenates the variable and the level with no separator
# (e.g. "nat_or_artnaturalistic"). The labels below also make the contrast
# level explicit in parentheses, so each row states its own reference
# comparison rather than relying on the caption to explain it.
#
# The random-effect SD rows ("Individual (SD)" etc.) are already relabelled
# upstream by their respective formatters, so they are passed through
# unchanged.

coefTermLabels <- c(
  "(Intercept)"                        = "Intercept",

  # trial: rank_trial in the within-experiment models, trial_m1 (offset so
  # 0 = first trial) in the cross-experiment models. Both are per-trial.
  "rank_trial"                         = "Trial",
  "trial_m1"                           = "Trial",

  # nuisance predictors, common to every model
  "sideAlt_trialTRUE"                  = "Side alternation",
  "reward_sideR"                       = "Reward side (right)",

  # test/control phase (within-experiment models only)
  "manipulationcontrol"                = "Phase (control)",
  "rank_trial:manipulationcontrol"     = "Trial × Phase",

  # stimulus pair (Eq. 3; reference = perpendicular/parallel)
  "experimentthinOb_150823"            = "Stimulus pair (thin oblique)",
  "trial_m1:experimentthinOb_150823"   = "Trial × Stimulus pair",
  "experimentthinOb_150823:reward_sideR" = "Reward side × Stimulus pair",

  # stimulus category (Eq. 4; reference = artificial)
  "nat_or_artnaturalistic"             = "Category (naturalistic)",
  "trial_m1:nat_or_artnaturalistic"    = "Trial × Category"
)

# Display order: fixed effects in a logical sequence (intercept, trial, the
# stimulus contrast and its interaction, then nuisance predictors), with
# random-effect SDs last. Without this the tables sort alphabetically on the
# display label, which puts "Individual (SD)" above the intercept.
coefTermOrder <- c(
  "Intercept",
  "Trial",
  "Phase (control)",
  "Trial × Phase",
  "Stimulus pair (thin oblique)",
  "Trial × Stimulus pair",
  "Category (naturalistic)",
  "Trial × Category",
  "Side alternation",
  "Reward side (right)",
  "Reward side × Stimulus pair",
  "Individual (SD)",
  "Individual within experiment (SD)",
  "Experiment (SD)",
  "Experiment: reward side (SD)"
)

# Order a relabelled term column. Anything unrecognised sorts to the end
# rather than becoming NA, so a newly added term stays visible.
orderCoefTerms <- function(term) {
  factor(term, levels = union(coefTermOrder, sort(unique(term))))
}

# Relabel where a mapping exists, pass anything else through untouched, so a
# newly added term shows up as its raw name rather than silently becoming NA.
labelCoefTerms <- function(term) {
  ifelse(term %in% names(coefTermLabels), unname(coefTermLabels[term]), term)
}
