# 04_robustness_race_children.R — does Y5 (poverty) change if race is added or children removed?
# Run after 03_counterfactual.R. Documentation: code/README.md

source("code/02_model_functions.R")

Y        <- "y5_poor"
OUT_ROOT <- "outputs/02_robustness/race_children"
SAVE_PREDICTIONS <- FALSE   # keep the main predictions used by the cluster analysis

remove_terms <- function(f, vars) {
  labels <- attr(terms(f), "term.labels")
  uses   <- sapply(labels, function(t) any(all.vars(as.formula(paste("~", t))) %in% vars))
  reformulate(labels[!uses])
}
add_terms <- function(f, terms_text) update(f, as.formula(paste("~ . +", terms_text)))

ORIGINAL <- list(FULL = FORMULA_FULL, SPLINE = FORMULA_SPLINE, TREE = FORMULA_TREE, SMALL = FORMULA_SMALL)

VARIANTS <- list(
  with_race = function(f, kind) add_terms(f, if (kind == "FULL") "race + educ:race" else "race"),
  no_children = function(f, kind) remove_terms(f, "children")
)

dir.create(OUT_ROOT, showWarnings = FALSE, recursive = TRUE)
sink(file.path(OUT_ROOT, "run_log.txt"), split = TRUE)
tryCatch({
  cat("Robustness check for", Y, "\nStarted:", format(Sys.time()), "\n")
  all_gaps <- list()

  main <- fread(file.path("outputs/01_counterfactual", Y, "gaps.csv"))
  main[, variant := "main"]
  all_gaps[["main"]] <- main

  for (v in names(VARIANTS)) {
    cat("\n\n############## Variant:", v, "##############\n")
    FORMULA_FULL   <- VARIANTS[[v]](ORIGINAL$FULL,   "FULL")
    FORMULA_SPLINE <- VARIANTS[[v]](ORIGINAL$SPLINE, "SPLINE")
    FORMULA_TREE   <- VARIANTS[[v]](ORIGINAL$TREE,   "TREE")
    FORMULA_SMALL  <- VARIANTS[[v]](ORIGINAL$SMALL,  "SMALL")
    cat("Features:", deparse(FORMULA_FULL, width.cutoff = 500), "\n")

    out_dir <- file.path(OUT_ROOT, v)
    dir.create(out_dir, showWarnings = FALSE)
    gaps <- run_outcome(Y, out_dir)
    gaps[, variant := v]
    all_gaps[[v]] <- gaps
  }
  FORMULA_FULL <- ORIGINAL$FULL; FORMULA_SPLINE <- ORIGINAL$SPLINE
  FORMULA_TREE <- ORIGINAL$TREE; FORMULA_SMALL  <- ORIGINAL$SMALL

  summary <- rbindlist(all_gaps, fill = TRUE)[
    , .(variant, group_var, group,
        gap_norm_pp   = round(100 * gap_norm, 2),
        gap_covid_pp  = round(100 * gap_covid, 2),
        gap_excess_pp = round(100 * gap_excess, 2))]
  fwrite(summary, file.path(OUT_ROOT, "summary.csv"))

  cat("\n\n================================================================\n")
  cat("SUMMARY: excess gap (percentage points) by variant\n")
  cat("================================================================\n")
  print(dcast(summary, group_var + group ~ variant, value.var = "gap_excess_pp"))

  # The reason race was dropped: its coding changed in 2020
  race_share <- rbindlist(lapply(c(2016, 2019, 2022), function(yr) {
    d <- readRDS(file.path("data/processed", paste0("acs_", yr, ".rds")))
    d[, .(share = round(sum(PWGTP) / d[, sum(PWGTP)], 4)), by = race][, year := yr]
  }))
  race_share <- dcast(race_share, race ~ year, value.var = "share")
  fwrite(race_share, file.path(OUT_ROOT, "race_shares_by_year.csv"))
  cat("\nWeighted share of each race category by year:\n"); print(race_share)
  cat("\nFinished:", format(Sys.time()), "\n")
}, error = function(e) cat("\nERROR:", conditionMessage(e), "\n"),
finally = sink())
