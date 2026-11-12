# =============================================================================
# 03a_run_y1_y2_ailana.R  -  launcher for Ailana
# Runs code/03_run_models.R for: "y1_employed", "y2_unemployed"
# Results: outputs/<outcome>/  (run_log.txt, models_*.csv, lasso_selected_*.csv, gaps*.csv, tree_residuals_*.png)
# Run from the project root: open "final project.Rproj" in RStudio, open this file, click Source.
# =============================================================================

Y_TO_RUN <- c("y1_employed", "y2_unemployed")
source("code/03_run_models.R")
