# Raw test-phase P_corr split by rewarded side, for the two experiments in the
# perpendicular/parallel vs. thin oblique comparison (Eq. 3). Reported in the
# Results to show why reward side is allowed to differ between experiments in
# that model: the side bias is large and runs in opposite directions.
#
# Objects created:
# sideBiasRef -- whole-percentage strings, e.g. sideBiasRef["thinOb_150823", "L"]
#
# `choices` must exist in the environment (from analysis_results.RData).

sideBiasRef <- choices %>%
  filter(experiment %in% c("perpPara_170725", "thinOb_150823"),
         manipulation == "test") %>%
  group_by(experiment, reward_side) %>%
  summarise(pcorr = mean(as.numeric(decision)), .groups = "drop") %>%
  mutate(pcorr = as.character(as.integer(round(pcorr * 100)))) %>%
  pivot_wider(names_from = reward_side, values_from = pcorr) %>%
  column_to_rownames("experiment")
