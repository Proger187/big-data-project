# =============================================================================
# 03_run_models.R
# Counterfactual analysis for the outcomes Y1-Y5 (see docs/PROJECT.md, docs/PLAN.md).
#
# For every outcome in Y_TO_RUN:
#   Step A (benchmark):      train on 2016 -> predict 2019 -> "normal" gap
#   Step B (counterfactual): train on 2019 -> predict 2022 -> "covid" gap
#   Excess gap = covid gap - normal gap
#
# Input : data/processed/acs_2016.rds, acs_2019.rds, acs_2022.rds (from 01_prepare_data.R)
# Output: outputs/<outcome>/   - see the list at the end of this file
# Run from the project root (open "final project.Rproj" in RStudio, then Source).
# =============================================================================

source("code/02_model_functions.R")

# Which outcome(s) to run - write one or several names inside c(...):
#   "y1_employed", "y2_unemployed"   -> Ailana
#   "y3_log_wage", "y4_hours"        -> Aiym
#   "y5_poor"                        -> Aibek
# Example for two outcomes: Y_TO_RUN <- c("y1_employed", "y2_unemployed")
# (the personal launchers code/03a_..., 03b_..., 03c_... set Y_TO_RUN themselves)
if (!exists("Y_TO_RUN")) Y_TO_RUN <- c("y5_poor")

for (y in Y_TO_RUN) {
  out_dir <- file.path("outputs", y)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  sink(file.path(out_dir, "run_log.txt"), split = TRUE)   # everything printed also goes to the log
  tryCatch({
    cat("Outcome:", y, "\nStarted:", format(Sys.time()), "\n")

    gaps_a <- run_pair(2016, 2019, y, out_dir)   # Step A: benchmark
    gaps_b <- run_pair(2019, 2022, y, out_dir)   # Step B: counterfactual

    # Final table: both steps side by side + excess gap
    setnames(gaps_a, c("n", "actual", "predicted", "gap"),
             c("n_2019", "actual_2019", "pred_2019_from_2016", "gap_norm"))
    setnames(gaps_b, c("n", "actual", "predicted", "gap"),
             c("n_2022", "actual_2022", "pred_2022_from_2019", "gap_covid"))
    gaps <- merge(gaps_a, gaps_b, by = c("group_var", "group"), all = TRUE, sort = FALSE)
    gaps[, gap_excess := gap_covid - gap_norm]
    fwrite(gaps, file.path(out_dir, "gaps.csv"))

    cat("\n================================================================\n")
    cat("FINAL: excess gap = (actual 2022 - predicted 2022) - (actual 2019 - predicted 2019)\n")
    cat("================================================================\n")
    print(gaps[, .(group_var, group, actual_2019, actual_2022, gap_norm, gap_covid, gap_excess)])
    cat("\nFinished:", format(Sys.time()), "\n")
  }, error = function(e) cat("\nERROR for", y, ":", conditionMessage(e), "\n"),
  finally = sink())
}

# Files in outputs/<outcome>/ after the run:
#   run_log.txt               everything that was printed (send this first)
#   models_2016.csv           model comparison on the 2016 test set
#   models_2019.csv           model comparison on the 2019 test set
#   lasso_selected_2016.csv   features kept by lasso (2016)
#   lasso_selected_2019.csv   features kept by lasso (2019)
#   gaps_2016_to_2019.csv     actual vs predicted 2019 (benchmark), overall and by groups
#   gaps_2019_to_2022.csv     actual vs predicted 2022 (counterfactual), overall and by groups
#   gaps.csv                  final table with the excess gap
#   tree_residuals_2019.png   where the 2016 model misses 2019 most
#   tree_residuals_2022.png   where the 2019 model misses 2022 most
