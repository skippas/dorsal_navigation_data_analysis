# =============================================================================
# run_all.R
# Runs the analysis pipeline end to end (steps 1-4). Comment out any step
# below and re-run to reuse its cached output in scripts/intermediate_outputs/
# instead of regenerating it.
#
# Step 5 (the report) is rendered separately, not by this script — see
# 5_results_report.qmd, or run render_figures.sh in the manuscript repo.
#
# Run from the project root directory (where data_analysis.Rproj lives).
# =============================================================================

setwd(rprojroot::find_rstudio_root_file())

source("scripts/1_loading_cleaning.R")
source("scripts/2_fit_models.R")
source("scripts/3_format_results.R")
source("scripts/4_run_supp_analysis.R") # steps 3 and 4 don't rely on each other (although the 3->4 numbering might make one think so)

message("Steps 1-4 complete. Render scripts/5_results_report.qmd for the report.")
