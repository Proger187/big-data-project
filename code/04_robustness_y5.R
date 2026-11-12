# =============================================================================
# 04_robustness_y5.R
# NOTE (12.11): this check was run when race was still in the main model.
# Result: dropping race changed the excess gap (1.70 -> 1.94 pp), dropping children did not
# -> race was removed from all models (D15). Outputs: outputs/y5_poor_robustness/.
#
# Robustness check for Y5 (poverty): does the result change if we drop
# features that may not be comparable between years?
#   - children: the "female_na" group (no answer) grows from year to year
#   - race:     race coding changed in 2020 ("other" grew a lot in 2022)
#
# Variants (the main run without changes is in outputs/y5_poor/):
#   no_children          - without `children`
#   no_race              - without `race` (and its interaction with education)
#   no_children_no_race  - without both
#
# Input : data/processed/acs_*.rds, outputs/y5_poor/gaps.csv (main run)
# Output: outputs/y5_poor_robustness/<variant>/   - same files as the main run
#         outputs/y5_poor_robustness/summary.csv  - all variants side by side
#         outputs/y5_poor_robustness/run_log.txt
# Run from the project root, after 03_run_models.R was run for y5_poor.
# =============================================================================

source("code/02_model_functions.R")

Y        <- "y5_poor"
OUT_ROOT <- "outputs/y5_poor_robustness"
VARIANTS <- list(
  no_children         = c("children"),
  no_race             = c("race"),
  no_children_no_race = c("children", "race")
)

# Removes every term that uses one of `vars` (also interactions like educ:race)
remove_from <- function(f, vars) {
  labels <- attr(terms(f), "term.labels")
  uses   <- sapply(labels, function(t) any(all.vars(as.formula(paste("~", t))) %in% vars))
  reformulate(labels[!uses])
}

# Keep the original formulas to restore them for every variant
ORIGINAL <- list(FULL = FORMULA_FULL, SPLINE = FORMULA_SPLINE, TREE = FORMULA_TREE, SMALL = FORMULA_SMALL)

dir.create(OUT_ROOT, showWarnings = FALSE, recursive = TRUE)
sink(file.path(OUT_ROOT, "run_log.txt"), split = TRUE)
tryCatch({
  cat("Robustness check for", Y, "\nStarted:", format(Sys.time()), "\n")
  all_gaps <- list()

  # main run (from 03_run_models.R) for comparison
  main <- fread("outputs/y5_poor/gaps.csv")
  main[, variant := "main"]
  all_gaps[["main"]] <- main

  for (v in names(VARIANTS)) {
    drop <- VARIANTS[[v]]
    cat("\n\n############## Variant:", v, "- dropped:", paste(drop, collapse = ", "), "##############\n")

    # change the formulas used by the model functions
    FORMULA_FULL   <- remove_from(ORIGINAL$FULL,   drop)
    FORMULA_SPLINE <- remove_from(ORIGINAL$SPLINE, drop)
    FORMULA_TREE   <- remove_from(ORIGINAL$TREE,   drop)
    FORMULA_SMALL  <- remove_from(ORIGINAL$SMALL,  drop)
    cat("Features:", deparse(FORMULA_FULL, width.cutoff = 500), "\n")

    out_dir <- file.path(OUT_ROOT, v)
    dir.create(out_dir, showWarnings = FALSE)

    gaps_a <- run_pair(2016, 2019, Y, out_dir)
    gaps_b <- run_pair(2019, 2022, Y, out_dir)
    setnames(gaps_a, c("n", "actual", "predicted", "gap"),
             c("n_2019", "actual_2019", "pred_2019_from_2016", "gap_norm"))
    setnames(gaps_b, c("n", "actual", "predicted", "gap"),
             c("n_2022", "actual_2022", "pred_2022_from_2019", "gap_covid"))
    gaps <- merge(gaps_a, gaps_b, by = c("group_var", "group"), all = TRUE, sort = FALSE)
    gaps[, gap_excess := gap_covid - gap_norm]
    fwrite(gaps, file.path(out_dir, "gaps.csv"))
    gaps[, variant := v]
    all_gaps[[v]] <- gaps
  }

  # restore the original formulas
  FORMULA_FULL <- ORIGINAL$FULL; FORMULA_SPLINE <- ORIGINAL$SPLINE
  FORMULA_TREE <- ORIGINAL$TREE; FORMULA_SMALL  <- ORIGINAL$SMALL

  # Summary: gaps in percentage points for every variant
  summary <- rbindlist(all_gaps, fill = TRUE)[
    , .(variant, group_var, group,
        gap_norm_pp   = round(100 * gap_norm, 2),
        gap_covid_pp  = round(100 * gap_covid, 2),
        gap_excess_pp = round(100 * gap_excess, 2))]
  fwrite(summary, file.path(OUT_ROOT, "summary.csv"))

  cat("\n\n================================================================\n")
  cat("SUMMARY: excess gap (percentage points) by variant\n")
  cat("================================================================\n")
  wide <- dcast(summary[group_var %in% c("all", "educ", "sex", "hispanic_origin", "age_group")],
                group_var + group ~ variant, value.var = "gap_excess_pp")
  print(wide)
  cat("\nFinished:", format(Sys.time()), "\n")
}, error = function(e) cat("\nERROR:", conditionMessage(e), "\n"),
finally = sink())
