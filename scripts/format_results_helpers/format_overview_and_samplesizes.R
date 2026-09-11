# Builds the two per-experiment overview tables (tbl-overview and
# tbl-sample-sizes), which share the same underlying `allExpOverview` /
# n_art / n_nat prep -- computed once here rather than twice.
# Objects created:
# tblOverviewStr, tblSampleSizesStr (raw kableExtra output strings, for
# knitr::asis_output() in the qmd)

expOrder <- c(q2ArtTableOrder, names(natcanLabels))

# Prediction at the maximum shared test trial across all experiments
sharedTrial <- emmAll %>%
  filter(manipulation == "test") %>%
  group_by(experiment) %>%
  summarise(max_trial = max(rank_trial), .groups = "drop") %>%
  pull(max_trial) %>%
  min()

sharedPtTbl <- emmAll %>%
  filter(manipulation == "test", rank_trial == sharedTrial) %>%
  # No per-cell % (the column group is headed "(%)"), and the value/CI order
  # matches the other P_corr columns -- previously this one alone rendered as
  # "81% [74, 87]" while the rest were "62 [49, 73]%".
  mutate(sharedPcorr95CI = paste0(
    as.integer(round(prob * 100)), " [",
    as.integer(round(asymp.LCL * 100)), ", ",
    as.integer(round(asymp.UCL * 100)), "]"
  )) %>%
  select(experiment, sharedPcorr95CI)

allExpOverview <- emmPtsTbl %>%
  left_join(sharedPtTbl, by = "experiment") %>%
  left_join(
    contrastsTbl %>% select(experiment, oddsRatio95CI, pFmt),
    by = "experiment"
  ) %>%
  left_join(nSummary, by = "experiment") %>%
  mutate(experiment = factor(experiment, levels = expOrder)) %>%
  arrange(experiment) %>%
  mutate(experiment = dplyr::recode(as.character(experiment), !!!c(artLabels, natcanLabels)))

n_art <- sum(allExpOverview$nat_or_art == "artificial")
n_nat <- sum(allExpOverview$nat_or_art == "naturalistic")

# -----------------------------------------------------------------------------
# tbl-overview
# -----------------------------------------------------------------------------

tblOverview <- allExpOverview %>%
  select(experiment, firstPcorr95CI, midPcorr95CI, lastTestPcorr95CI,
         sharedPcorr95CI, ctrlStartPcorr95CI, oddsRatio95CI, pFmt) %>%
  mutate(across(where(is.character), ~ if_else(is.na(.), "---", .))) %>%
  knitr::kable(
    col.names = c("Experiment", "Test start", "Test middle", "Test end",
                  paste0("Trial ", sharedTrial), "Control start",
                  "Odds ratio [95% CI]", "P"),
    caption = paste0(
      "\\label{tbl-overview}Predicted \\(P_{\\mathrm{corr}}\\) at key ",
      "trial positions and test--control contrasts for all experiments. ",
      "Control start and contrast columns are blank for test-only experiments."
    ),
    booktabs = TRUE, escape = FALSE
  ) %>%
  kableExtra::add_header_above(
    c(" " = 1, "$P_{\\\\mathrm{corr}}$ (\\\\%) [95\\\\% CI]" = 5, " " = 2),
    escape = FALSE
  ) %>%
  kableExtra::pack_rows("Artificial", 1, n_art) %>%
  kableExtra::pack_rows("Naturalistic", n_art + 1, n_art + n_nat) %>%
  kableExtra::kable_styling(font_size = 9, latex_options = "hold_position")

# knitr::kable converts non-ASCII chars to <U+XXXX> sequences; fix them here
# after all kableExtra processing (which would re-escape LaTeX backslashes)
tblOverviewStr <- gsub("<U\\+00B0>", "$^{\\\\circ}$",
                  gsub("<U\\+00B1>", "$\\\\pm$",
                  gsub("<U\\+2014>", "---",
                  gsub("< ", "$<$ ",
                       paste(as.character(tblOverview), collapse = "\n")))))

# -----------------------------------------------------------------------------
# tbl-sample-sizes
# -----------------------------------------------------------------------------

tblSampleSizes <- allExpOverview %>%
  select(experiment, lastTestTrial,
         n_wasps_test, n_choices_test, n_wasps_control, n_choices_control) %>%
  knitr::kable(
    col.names = c("Experiment", "Last trial",
                  "Wasps", "Choices", "Wasps", "Choices"),
    caption = paste0(
      "\\label{tbl-sample-sizes}Per-experiment sample sizes and ",
      "test-phase duration. Wasps and choices are split by phase (test and ",
      "control). Last trial is the highest trial number reached in the test ",
      "phase. Control columns are blank for test-only experiments."
    ),
    booktabs = TRUE, escape = FALSE
  ) %>%
  kableExtra::add_header_above(
    c(" " = 2, "Test" = 2, "Control" = 2),
    escape = FALSE
  ) %>%
  kableExtra::pack_rows("Artificial", 1, n_art) %>%
  kableExtra::pack_rows("Naturalistic", n_art + 1, n_art + n_nat) %>%
  kableExtra::kable_styling(font_size = 9, latex_options = "hold_position")

tblSampleSizesStr <- gsub("<U\\+2014>", "---",
                          paste(as.character(tblSampleSizes), collapse = "\n"))
