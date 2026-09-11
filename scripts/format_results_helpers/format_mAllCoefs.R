# objects created:
# mAllCoefsFmt, mAllCoefsRef, mAllCoefsTbl,
# allTrialSlopeRef, allTrialSlopeTbl

source("scripts/format_results_helpers/coef_term_labels.R")   # -> labelCoefTerms()

mAllCoefsFmt <- mAllCoefs %>%
  rename(logOdds = estimate) %>%
  mutate(
    term           = if_else(term == "sd__(Intercept)", "Individual (SD)", term),
    pFmt           = pvalue(p.value, accuracy = 0.001),
    oddsMultiplier = exp(logOdds)
  ) %>%
  fmtNumCols() %>%
  make_confint_col(prefix = "[", suffix = "]") %>%
  arrange(nat_or_art, experiment)

mAllCoefsRef <- mAllCoefsFmt %>%
  select(term, logOdds, p.value, pFmt, confint, experiment) %>%
  pivot_wider(
    names_from  = "term",
    values_from = c("logOdds", "p.value", "pFmt", "confint"),
    names_glue  = "{term}_{.value}"
  ) %>%
  tibble::column_to_rownames("experiment")

mAllCoefsTbl <- mAllCoefsFmt %>%
  transmute(
    experiment, nat_or_art, term, logOdds,
    logOdds95CI = sprintf("%s %s", logOdds, confint),
    pFmt
  ) %>%
  mutate(
    logOdds95CI = if_else(term == "Individual (SD)", logOdds, logOdds95CI),
    pFmt        = if_else(term == "Individual (SD)", "—", pFmt),
    # Display names applied last: mAllCoefsFmt keeps the raw lme4 term names,
    # which allTrialSlopeRef filters on and mAllCoefsRef glues into its column
    # names (and which the qmd looks up as e.g. "rank_trial_pFmt").
    term        = labelCoefTerms(term)
  ) %>%
  select(-logOdds)

allTrialSlopeRef <- mAllCoefsFmt %>%
  filter(term == "rank_trial") %>%
  select(experiment, nat_or_art, logOdds, confint, pFmt, oddsMultiplier) %>%
  tibble::column_to_rownames("experiment")

allTrialSlopeTbl <- allTrialSlopeRef %>%
  tibble::rownames_to_column("experiment") %>%
  transmute(
    experiment, nat_or_art,
    trialLogOdds95CI = sprintf("%s %s", logOdds, confint),
    pFmt,
    oddsMultiplier
  )
