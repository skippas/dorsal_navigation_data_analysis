# Objects created:
# artCompareRef
#
# artContrastResp must exist in the environment (produced by
# compare_art_experiments.R). Single-row data frame -- referenced in the qmd by
# column (e.g. artCompareRef$oddsRatio), not by row name.

artCompareRef <- artContrastResp %>%
  rename(oddsRatio = odds.ratio) %>%
  mutate(pFmt = pvalue(p.value)) %>%
  fmtNumCols() %>%
  make_confint_col(lower_col = "asymp.LCL", upper_col = "asymp.UCL",
                    out_col = "confint", prefix = "[", suffix = "]")
