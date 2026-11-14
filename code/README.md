# Code — what every script does

All scripts are R. Run them **from the project root** (open `final project.Rproj` in RStudio, then open a script and click *Source*). Paths are relative, the seed is fixed (`set.seed(1)`), so every run gives the same numbers.

`code/00_run_everything.R` runs all steps in order, each in a fresh R process.

## Order and dependencies

```
01_prepare_data ──► data/processed/acs_2016/2019/2022.rds
        │
        ├─► 03_counterfactual ──► outputs/01_counterfactual/  + data/processed/predictions/
        │          ├─► 04_robustness_race_children
        │          ├─► 05_robustness_age_shape
        │          └─► 08_clusters_people   (needs the saved predictions)
        ├─► 06_robustness_wage_alt, 07_robustness_hours_alt   (independent)
        └─► 09_clusters_workers                               (independent)
                                   all of them ──► 10_summary_tables
```

`02_model_functions.R` is not run by itself: 03, 04 and 05 load it with `source()`.

## Scripts

| Script | What it does | Methods (course) | Output | Time* |
|---|---|---|---|---|
| `00_run_everything.R` | runs 01 and 03–10 in order | — | everything below | hours |
| `01_prepare_data.R` | reads the raw ACS 1-year PUMS person files (two halves `a`/`b` per year), keeps people 18–64, builds features X and outcomes Y1–Y5, converts money to 2022 dollars (ADJINC × CPI), checks the files | data cleaning | `data/processed/acs_<year>.rds` (not in git), `outputs/00_data_checks/` | ~5 min |
| `02_model_functions.R` | settings (seed, sample sizes, formulas) and all model functions: fit, compare on the test set, pick the best, predict, compute gaps | — | — | — |
| `03_counterfactual.R` | **main analysis.** For every Y: train on 2016 → predict 2019 (normal gap); train on 2019 → predict 2022 (covid gap); excess gap = covid − normal, overall and by group | validation set (100k train / 50k test), linear & logistic regression, lasso & ridge (λ by 5-fold CV), natural spline, LDA, KNN, classification/regression tree pruned by CV, regression tree on residuals | `outputs/01_counterfactual/<y>/`, `data/processed/predictions/` | ~15–40 min |
| `04_robustness_race_children.R` | Y5 again with race added and with children removed, compared with the main model; race shares by year | same as 03 | `outputs/02_robustness/race_children/` | ~15 min |
| `05_robustness_age_shape.R` | which shape of age fits best: polynomial (degree 1–4), step function (4/6/8 intervals), natural spline (df 3–8), chosen by 5-fold CV; excess gap recomputed with the chosen shape | polynomial regression, step functions, splines, k-fold CV | `outputs/02_robustness/age_shape/` | ~15–30 min |
| `06_robustness_wage_alt.R` | independent second implementation for Y3 (wage): 70/30 split, own 5-fold CV and tuning grids | linear regression, splines, ridge, lasso, tree, k-fold CV | `outputs/02_robustness/wage_alt/` | long |
| `07_robustness_hours_alt.R` | the same for Y4 (hours) | same | `outputs/02_robustness/hours_alt/` | long |
| `08_clusters_people.R` | "types of people" from pre-COVID features; excess gap of Y1–Y5 inside every type | PCA, k-means (K by elbow + silhouette) | `outputs/03_clusters/people/` | ~1 min |
| `09_clusters_workers.R` | "types of workers" from wage, hours, work from home, self-employment, commute, education, age; shares of the types in 2019 vs 2022 (descriptive) | PCA, k-means | `outputs/03_clusters/workers/` | ~1 min |
| `10_summary_tables.R` | collects everything into summary tables and two figures | — | `outputs/04_summary/` | seconds |

\* on a laptop with the full data.

## Key definitions (used everywhere)

- **Outcomes:** Y1 employed (25–64), Y2 unemployed (25–64 in the labour force), Y3 log real annual wage (employed 25–64 with wage > 0), Y4 usual weekly hours (employed 25–64), Y5 poor = income below the poverty line (18–64).
- **Features X:** age, sex, education (5 groups), Hispanic origin, foreign-born, citizen, marital status, state, years in the US, world region of birth, STEM degree, field of bachelor's degree, own children (women). Only characteristics that COVID did not change. **Race is not used** (Census changed its coding in 2020 — checked in script 04).
- **Model choice:** classification (Y1, Y2, Y5) — highest test AUC; regression (Y3, Y4) — lowest test MSE. KNN is shown only for comparison (too slow to predict millions of people).
- **Averages:** weighted by the person weight `PWGTP`; for classification the average of predicted probabilities (not of 0/1 classes).
- **Gaps:** gap = weighted mean(actual) − weighted mean(predicted). Normal gap: 2016 model on 2019 people. Covid gap: 2019 model on 2022 people. **Excess gap = covid gap − normal gap.**

## Settings you might change

| Where | Setting | Default |
|---|---|---|
| `01_prepare_data.R` | `RAW_FILES` — names of the raw csv files in `data/` | as downloaded from Census |
| `02_model_functions.R` | `TRAIN_N`, `TEST_N` — sample sizes | 100 000 / 50 000 |
| `03_counterfactual.R`, `05_...` | `Y_TO_RUN` — set it before `source()` to run only some outcomes | all five |
| `08_clusters_people.R`, `09_clusters_workers.R` | `K` — number of clusters | 6 / 5 |

## Packages

`data.table`, `glmnet`, `MASS`, `class`, `rpart`, `splines`, `cluster` (+ `Matrix` for 06/07). Scripts install `data.table`, `glmnet` and `cluster` if they are missing.

## Old file names (before 14.11)

| Old | New |
|---|---|
| `03_run_models.R`, `03_run_all.R`, `03a/03b/03c_run_*.R` | `03_counterfactual.R` (runs all five outcomes) |
| `04_robustness_y5.R` | `04_robustness_race_children.R` |
| `07_nonlinear_age.R` | `05_robustness_age_shape.R` |
| `y3_wage_script..R`, `y4_hours_script.R` | `06_robustness_wage_alt.R`, `07_robustness_hours_alt.R` |
| `05_clusters_types.R`, `06_clusters_workers.R` | `08_clusters_people.R`, `09_clusters_workers.R` |
