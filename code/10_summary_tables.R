# 10_summary_tables.R — collects the results of 03-07 into summary tables and figures.
# Reads only outputs/, runs in seconds. Documentation: code/README.md

library(data.table)

OUT_DIR <- "outputs/04_summary"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

OUTCOMES <- data.table(
  outcome = c("y1_employed", "y2_unemployed", "y3_log_wage", "y4_hours", "y5_poor"),
  label   = c("Employment rate", "Unemployment rate", "Real wage", "Weekly hours", "Poverty rate"),
  unit    = c("pp", "pp", "% (log points x 100)", "hours", "pp"),
  scale   = c(100, 100, 100, 1, 100)
)
main_dir <- function(y) file.path("outputs/01_counterfactual", y)

# ---- Model choice --------------------------------------------------------------------------

models <- rbindlist(lapply(OUTCOMES$outcome, function(y) rbindlist(lapply(c(2016, 2019), function(yr) {
  f <- file.path(main_dir(y), sprintf("models_%d.csv", yr))
  if (!file.exists(f)) return(NULL)
  m <- fread(f)
  m[, `:=`(outcome = y, train_year = yr, rank = seq_len(.N))]
}), fill = TRUE)), fill = TRUE)
setcolorder(models, c("outcome", "train_year", "rank"))
fwrite(models, file.path(OUT_DIR, "model_comparison.csv"))

best <- models[used_for_counterfactual == TRUE, .SD[1], by = .(outcome, train_year)]
best[, metric := fifelse(is.na(auc), sprintf("test MSE %.3f, R2 %.3f", test_mse, r2), sprintf("AUC %.3f", auc))]
best_wide <- dcast(best, outcome ~ train_year, value.var = c("model", "metric"))

# ---- Main result -----------------------------------------------------------------------------

main <- rbindlist(lapply(OUTCOMES$outcome, function(y) {
  f <- file.path(main_dir(y), "gaps.csv")
  if (!file.exists(f)) return(NULL)
  fread(f)[, outcome := y]
}))
main <- merge(main, OUTCOMES, by = "outcome")
for (v in c("gap_norm", "gap_covid", "gap_excess")) main[, (paste0(v, "_scaled")) := round(get(v) * scale, 2)]

overall <- main[group_var == "all", .(outcome, label, unit,
                                       actual_2019 = round(actual_2019, 4), actual_2022 = round(actual_2022, 4),
                                       gap_norm = gap_norm_scaled, gap_covid = gap_covid_scaled,
                                       gap_excess = gap_excess_scaled)]
overall <- merge(overall, best_wide, by = "outcome", all.x = TRUE)
fwrite(overall, file.path(OUT_DIR, "main_results.csv"))

by_group <- dcast(main[group_var != "all"], group_var + group ~ outcome, value.var = "gap_excess_scaled")
fwrite(by_group, file.path(OUT_DIR, "excess_gap_by_group.csv"))

# ---- Robustness: overall excess gap under every check ------------------------------------------

rob <- overall[, .(outcome, check = "main model", gap_excess)]

age <- "outputs/02_robustness/age_shape/summary.csv"
if (file.exists(age)) {
  a <- merge(fread(age), OUTCOMES, by = "outcome")
  rob <- rbind(rob, a[, .(outcome, check = paste("age shape chosen by CV:", chosen_shape),
                          gap_excess = round(excess_chosen * scale, 2))])
}
rc <- "outputs/02_robustness/race_children/summary.csv"
if (file.exists(rc)) {
  r <- fread(rc)[group_var == "all" & variant != "main"]
  rob <- rbind(rob, r[, .(outcome = "y5_poor", check = paste("variant:", variant), gap_excess = gap_excess_pp)])
}
alt <- list(y3_log_wage = "outputs/02_robustness/wage_alt/wage_gap_results.csv",
            y4_hours    = "outputs/02_robustness/hours_alt/hours_gap_results.csv")
for (y in names(alt)) if (file.exists(alt[[y]])) {
  g <- fread(alt[[y]])
  sc <- OUTCOMES[outcome == y, scale]
  rob <- rbind(rob, data.table(outcome = y, check = "alternative script (70/30 split, own CV)",
                               gap_excess = round(-g$adjusted_prediction_minus_actual * sc, 2)))
}
rob <- merge(rob, OUTCOMES[, .(outcome, unit)], by = "outcome")[order(outcome)]
fwrite(rob, file.path(OUT_DIR, "robustness_overall.csv"))

# ---- Clusters of people: excess gap by cluster ---------------------------------------------------

cg <- "outputs/03_clusters/people/cluster_gaps.csv"
if (file.exists(cg)) {
  c1 <- merge(fread(cg), OUTCOMES, by = "outcome")
  c1[, gap_excess_scaled := round(gap_excess * scale, 2)]
  fwrite(dcast(c1, cluster ~ outcome, value.var = "gap_excess_scaled"),
         file.path(OUT_DIR, "excess_gap_by_cluster.csv"))
}

# ---- Figures ---------------------------------------------------------------------------------------

png(file.path(OUT_DIR, "excess_gap_overall.png"), width = 1200, height = 500)
par(mfrow = c(1, 5), mar = c(4, 4, 3, 1))
for (i in seq_len(nrow(overall))) {
  r <- overall[i]
  barplot(c(r$gap_norm, r$gap_covid, r$gap_excess), names.arg = c("normal", "covid", "excess"),
          col = c("grey70", "grey40", "steelblue"), main = r$label, ylab = r$unit)
  abline(h = 0)
}
dev.off()

png(file.path(OUT_DIR, "excess_gap_by_education.png"), width = 1400, height = 500)
par(mfrow = c(1, 5), mar = c(6, 4, 3, 1))
lev <- c("lt_hs", "hs", "some_college", "ba", "grad")
for (y in OUTCOMES$outcome) {
  g <- main[outcome == y & group_var == "educ"][match(lev, group)]
  if (nrow(g) == 0) next
  barplot(g$gap_excess_scaled, names.arg = lev, las = 2, col = "steelblue",
          main = OUTCOMES[outcome == y, label], ylab = OUTCOMES[outcome == y, unit])
  abline(h = 0)
}
dev.off()

cat("Main result (excess gap = covid gap - normal gap):\n"); print(overall)
cat("\nRobustness:\n"); print(rob)
