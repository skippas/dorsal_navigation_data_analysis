# Builds the cross-experiment model coefficients table (tbl-cross-coefs).
# Mirrors format_tbl_model_coefs.R (Table S3) in columns, styling and row
# grouping -- the difference is that groups are the two joint models rather
# than individual experiments.
#
# mArtCompare and mNatArtCompare must exist in the environment (produced by
# cross_experiment_comparisons.R).
#
# Objects created:
# tblCrossCoefsStr (raw kableExtra output string, for knitr::asis_output()
# in the qmd / \input{} from results_supplementary.tex)

# Random-effect SD rows arrive from broom.mixed as "sd__(Intercept)" with the
# grouping factor in `group`. mNatArtCompare has two of them, so they must be
# labelled distinctly or the table shows two identical row names. Its random
# slope for reward side arrives as "sd__reward_sideR" (group = experiment) plus
# a "cor__(Intercept).reward_sideR" row; the correlation is dropped from the
# table and described in the caption instead.
reLabels <- c(
  individual              = "Individual (SD)",
  experiment              = "Experiment (SD)",
  `individual:experiment` = "Individual within experiment (SD)",
  `experiment:individual` = "Individual within experiment (SD)"
)

tidyCross <- function(model, model_label) {
  broom.mixed::tidy(model, conf.int = TRUE) %>%
    filter(!grepl("^cor__", term)) %>%
    mutate(
      term = case_when(
        term == "sd__(Intercept)"  ~ unname(reLabels[group]),
        term == "sd__reward_sideR" ~ "Experiment: reward side (SD)",
        TRUE                       ~ term
      ),
      model = model_label
    ) %>%
    select(model, term, estimate, conf.low, conf.high, p.value)
}

crossCoefs <- bind_rows(
  tidyCross(mArtCompare,    "Perpendicular/parallel vs.\\ thin oblique (Eq.~3)"),
  tidyCross(mNatArtCompare, "Artificial vs.\\ naturalistic (Eq.~4)")
) %>%
  # Fix the model order as Eq. 3 then Eq. 4, matching the Methods; otherwise
  # the later arrange() sorts the groups alphabetically and flips them.
  mutate(model = factor(model, levels = unique(model)))

crossCoefsFmt <- crossCoefs %>%
  rename(logOdds = estimate) %>%
  mutate(pFmt = pvalue(p.value, accuracy = 0.001)) %>%
  fmtNumCols() %>%
  make_confint_col(prefix = "[", suffix = "]") %>%
  mutate(
    # Random-effect SDs have no CI or p-value; show the SD alone, matching
    # how Table S3 presents "Individual (SD)".
    isRandom     = grepl("\\(SD\\)$", term),
    logOdds95CI  = if_else(isRandom, logOdds, sprintf("%s %s", logOdds, confint)),
    pFmt         = if_else(isRandom, "—", pFmt)
  )

source("scripts/format_results_helpers/coef_term_labels.R")   # -> labelCoefTerms()

crossCoefsFmt <- crossCoefsFmt %>%
  mutate(term = labelCoefTerms(term)) %>%
  arrange(model, orderCoefTerms(term))

crossCoefsTable <- crossCoefsFmt %>% select(term, logOdds95CI, pFmt)

# Group anchors for pack_rows, in the order the models appear above
crossGroups <- crossCoefsFmt %>%
  mutate(model = factor(model, levels = unique(model))) %>%
  group_by(model) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(end = cumsum(n), start = end - n + 1, label = as.character(model))

kx <- knitr::kable(
  crossCoefsTable,
  col.names = c("Term", "Log-odds [95% CI]", "P"),
  caption = paste0(
    "\\label{tbl-cross-coefs}Coefficients for the two cross-experiment models ",
    "(Eq.~3 and Eq.~4). Terms in parentheses give the level being contrasted ",
    "against the reference level, which is the perpendicular-parallel pair ",
    "(Eq.~3) and artificial stimuli (Eq.~4). Trial is coded so that 0 is the ",
    "first trial, so the intercept and stimulus terms are evaluated at trial 1 ",
    "and the trial slope is per trial. Because side biases differed in ",
    "direction between experiments (Table~\\ref{tbl-model-coefs}), reward side ",
    "interacts with stimulus pair in Eq.~3 and varies as a random slope across ",
    "experiments in Eq.~4 (the experiment-level intercept and reward-side ",
    "effect were allowed to correlate; the correlation is not shown). ",
    "Consequently, in Eq.~3 the stimulus-pair and reward-side terms are ",
    "evaluated at the reference level of the other (left-reward trials and ",
    "the perpendicular-parallel pair, respectively). ",
    "Random-effect standard deviations are reported without confidence ",
    "intervals or p-values."
  ),
  booktabs = TRUE, escape = FALSE
)
for (i in seq_len(nrow(crossGroups))) {
  kx <- kableExtra::pack_rows(kx,
    crossGroups$label[i], crossGroups$start[i], crossGroups$end[i],
    escape = FALSE
  )
}
kx <- kableExtra::kable_styling(kx, font_size = 9, latex_options = "hold_position")

# See format_tbl_model_coefs.R for why the × is swapped here rather than
# written as $\times$ in the label itself.
tblCrossCoefsStr <- gsub("×", "$\\\\times$",
                    gsub("<U\\+2014>", "---",
                    gsub("< ", "$<$ ",
                         paste(as.character(kx), collapse = "\n"))))
