# =============================================================================
# 03b_run_y3_y4_aiym.R  -  launcher for Aiym
# Runs code/03_run_models.R for: "y3_log_wage", "y4_hours"
# Results: outputs/<outcome>/  (run_log.txt, models_*.csv, lasso_selected_*.csv, gaps*.csv, tree_residuals_*.png)
# Run from the project root: open "final project.Rproj" in RStudio, open this file, click Source.
# =============================================================================

Y_TO_RUN <- c("y3_log_wage", "y4_hours")
source("code/03_run_models.R")
