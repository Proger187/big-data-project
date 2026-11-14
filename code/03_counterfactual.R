# 03_counterfactual.R — main analysis: benchmark 2016 -> 2019, counterfactual 2019 -> 2022, Y1-Y5.
# Documentation: code/README.md

source("code/02_model_functions.R")

if (!exists("Y_TO_RUN")) Y_TO_RUN <- c("y1_employed", "y2_unemployed", "y3_log_wage", "y4_hours", "y5_poor")
OUT_ROOT <- "outputs/01_counterfactual"

for (y in Y_TO_RUN) {
  out_dir <- file.path(OUT_ROOT, y)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  sink(file.path(out_dir, "run_log.txt"), split = TRUE)
  tryCatch({
    cat("Outcome:", y, "\nStarted:", format(Sys.time()), "\n")
    gaps <- run_outcome(y, out_dir)
    cat("\n================================================================\n")
    cat("FINAL: excess gap = (actual 2022 - predicted 2022) - (actual 2019 - predicted 2019)\n")
    cat("================================================================\n")
    print(gaps[, .(group_var, group, actual_2019, actual_2022, gap_norm, gap_covid, gap_excess)])
    cat("\nFinished:", format(Sys.time()), "\n")
  }, error = function(e) cat("\nERROR for", y, ":", conditionMessage(e), "\n"),
  finally = sink())
}
