# =============================================================================
# 03_run_all.R  -  runs all five outcomes Y1-Y5 in one go (~30-40 min)
# Results: outputs/<outcome>/ for every outcome
# Run from the project root: open "final project.Rproj" in RStudio, open this file, click Source.
# =============================================================================

Y_TO_RUN <- c("y1_employed", "y2_unemployed", "y3_log_wage", "y4_hours", "y5_poor")
source("code/03_run_models.R")
