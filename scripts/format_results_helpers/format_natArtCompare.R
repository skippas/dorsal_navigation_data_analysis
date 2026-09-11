# Objects created:
# natArtCompareRef  -- the contrast (odds ratio, CI, p), single row
# natArtCompareGrp  -- per-category predicted P_corr with CIs, row names are
#                      "artificial" / "naturalistic"
#
# natArtContrastResp and emmNatArtCompare must exist in the environment
# (produced by cross_experiment_comparisons.R).

natArtCompareRef <- natArtContrastResp %>%
  rename(oddsRatio = odds.ratio) %>%
  mutate(pFmt = pvalue(p.value)) %>%
  fmtNumCols() %>%
  make_confint_col(lower_col = "asymp.LCL", upper_col = "asymp.UCL",
                    out_col = "confint", prefix = "[", suffix = "]")

# Per-category predicted probabilities at the contrast trial, as whole
# percentages, matching how P_corr is reported elsewhere in the manuscript.
natArtCompareGrp <- summary(emmNatArtCompare, type = "response") %>%
  as.data.frame() %>%
  remove_rownames() %>%
  transmute(
    nat_or_art,
    prob    = as.character(as.integer(round(prob * 100))),
    confint = sprintf("[%d, %d]",
                      as.integer(round(asymp.LCL * 100)),
                      as.integer(round(asymp.UCL * 100)))
  ) %>%
  column_to_rownames("nat_or_art")
