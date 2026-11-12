# =============================================================================
# 03c_run_y5_aibek.R  -  launcher for Aibek
# Runs code/03_run_models.R for: "y5_poor"
# Results: outputs/<outcome>/  (run_log.txt, models_*.csv, lasso_selected_*.csv, gaps*.csv, tree_residuals_*.png)
# Run from the project root: open "final project.Rproj" in RStudio, open this file, click Source.
# =============================================================================

Y_TO_RUN <- c("y5_poor")
source("code/03_run_models.R")
