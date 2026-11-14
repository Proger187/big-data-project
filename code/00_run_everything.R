# 00_run_everything.R — reproduces the whole project in order, each step in a fresh R process.
# Run from the project root (open "final project.Rproj"). Documentation: code/README.md

steps <- c(
  "code/01_prepare_data.R",            # needs the raw ACS files in data/
  "code/03_counterfactual.R",
  "code/04_robustness_race_children.R",
  "code/05_robustness_age_shape.R",
  "code/06_robustness_wage_alt.R",
  "code/07_robustness_hours_alt.R",
  "code/08_clusters_people.R",
  "code/09_clusters_workers.R",
  "code/10_summary_tables.R"
)

rscript <- file.path(R.home("bin"), "Rscript")
for (s in steps) {
  cat("\n#####", s, "-", format(Sys.time()), "#####\n")
  status <- system2(rscript, s)
  if (status != 0) stop(s, " failed (exit code ", status, ")")
}
