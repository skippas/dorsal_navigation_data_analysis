# export_last_test_pcorrect.R
# ---------------------------
# Standalone export of the model-predicted asymptotic P(correct) at the last
# test trial of each experiment, for use by the Python simulations in
# ../view_simulation_analysis/ (orientation_vs_behaviour.py).
#
# Run from the analysis project root (where data_analysis.Rproj lives):
#     Rscript scripts/export_last_test_pcorrect.R
#
# Requires scripts/intermediate_outputs/formatted_results.RData, produced by
# the numbered pipeline scripts (0_run_all.R). Does not modify the pipeline.

load("scripts/intermediate_outputs/formatted_results.RData")

out <- emmPts[emmPts$trial_pos == "last_test", ]
out <- data.frame(
  experiment = out$experiment,
  nat_or_art = out$nat_or_art,
  rank_trial = out$rank_trial,
  prob       = out$prob,
  LCL        = out$asymp.LCL,
  UCL        = out$asymp.UCL
)
out <- out[order(out$nat_or_art, out$experiment), ]

out_dir <- file.path("..", "view_simulation_analysis", "inputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, "last_test_pcorrect.csv")
write.csv(out, out_path, row.names = FALSE)
cat(sprintf("Wrote %d rows to %s\n", nrow(out), out_path))
