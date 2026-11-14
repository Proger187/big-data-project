# Outputs — what every file means, and the results

Everything here is produced by the scripts in `code/` (see `code/README.md`). Folder numbers follow the order of the analysis. **Start with `04_summary/`** — it collects the main numbers in a few tables.

How to read the numbers:
- **gap** = weighted mean of the actual outcome − weighted mean of the model prediction.
- **normal gap** (`gap_norm`): model trained on 2016, applied to people of 2019 (three years without COVID).
- **covid gap** (`gap_covid`): model trained on 2019, applied to people of 2022.
- **excess gap** (`gap_excess`) = covid gap − normal gap: how 2019→2022 differed from a normal three-year period, for people with the same characteristics.
- Units: Y1, Y2, Y5 — shares (×100 = percentage points, pp); Y3 — log real wage (×100 ≈ %); Y4 — hours per week. Files in `04_summary/` are already converted (pp, %, hours); the other files keep the raw units.

## Folder map

| Folder | Script | What is inside |
|---|---|---|
| `00_data_checks/` | `01_prepare_data.R` | `prep_checks.csv` — rows, duplicates, sample sizes per outcome, missing values; `eda_means_by_year.csv` — weighted means of Y1–Y5 in 2016, 2019, 2022 |
| `01_counterfactual/<outcome>/` | `03_counterfactual.R` | main analysis, one folder per outcome (files below) |
| `02_robustness/race_children/` | `04_robustness_race_children.R` | Y5 with race added / children removed; `summary.csv`, `race_shares_by_year.csv`, one subfolder per variant |
| `02_robustness/age_shape/` | `05_robustness_age_shape.R` | shape of age chosen by CV; `cv_<y>.csv/.png`, `test_<y>.csv`, `gaps_<y>.csv`, `summary.csv` |
| `02_robustness/wage_alt/`, `hours_alt/` | `06_…`, `07_…` | independent second implementation for Y3 and Y4 (own README.txt inside) |
| `03_clusters/people/` | `08_clusters_people.R` | types of people and the excess gap in every type |
| `03_clusters/workers/` | `09_clusters_workers.R` | types of workers and their shares in 2019 vs 2022 |
| `04_summary/` | `10_summary_tables.R` | summary tables and figures for the report and slides |

### Files in `01_counterfactual/<outcome>/`

| File | Meaning |
|---|---|
| `models_2016.csv`, `models_2019.csv` | all models compared on the test set of that year (AUC / test error, or test MSE / R²); first row = chosen model; `used_for_counterfactual = FALSE` for KNN |
| `lasso_selected_<year>.csv` | features kept by lasso (λ.1se) and their coefficients |
| `gaps_2016_to_2019.csv` | actual vs predicted 2019 (benchmark), overall and by group |
| `gaps_2019_to_2022.csv` | actual vs predicted 2022 (counterfactual), overall and by group |
| `gaps.csv` | both side by side + `gap_excess` — **the main table** |
| `tree_residuals_<year>.png` | regression tree on the residuals: where the prediction misses most (missing when the tree has no splits) |
| `run_log.txt` | everything printed during the run |

### Files in `03_clusters/*/`

`pca_variance.csv` (variance explained by each component), `pca_loadings.csv` (which variables make each component), `k_choice.csv` + `elbow.png` (within-SS and silhouette for k = 2…10), `pca_clusters.png` (people on PC1–PC2 coloured by cluster), `cluster_profiles.csv` (what each cluster looks like and its share), `cluster_gaps.csv` + `.png` (people only: gaps by cluster, cluster 0 = everyone), `cluster_profiles_by_year.csv` + `cluster_shares.png` (workers only).

### Files in `04_summary/`

| File | Meaning |
|---|---|
| `main_results.csv` | one row per outcome: actual 2019 and 2022, normal / covid / excess gap, chosen model and its test metric |
| `excess_gap_by_group.csv` | excess gap by age group, education, sex, Hispanic origin, region (columns = outcomes) |
| `excess_gap_by_cluster.csv` | excess gap by cluster of people (cluster 0 = everyone) |
| `robustness_overall.csv` | overall excess gap under every robustness check |
| `model_comparison.csv` | all model comparison tables in one file |
| `excess_gap_overall.png`, `excess_gap_by_education.png` | figures for the slides |

---

## Results

### 1. Raw averages (`00_data_checks/eda_means_by_year.csv`)

| | 2016 | 2019 | 2022 |
|---|---|---|---|
| Employment rate, 25–64 | 73.8% | 75.7% | 76.5% |
| Unemployment rate, 25–64 | 4.7% | 3.7% | 3.5% |
| Median real wage (2022 $) | 49 145 | 52 035 | 52 116 |
| Mean weekly hours | 40.9 | 40.8 | 40.8 |
| Poverty rate, 18–64 | 13.2% | 11.5% | 11.7% |

By 2022 the averages were back at (or slightly above) the 2019 level. 2016→2019 improved much more than 2019→2022.

### 2. Model choice (`04_summary/main_results.csv`, `model_comparison.csv`)

| Outcome | Chosen model (2016 / 2019) | Test metric 2016 / 2019 |
|---|---|---|
| Y1 employed | logit / logit | AUC 0.717 / 0.723 |
| Y2 unemployed | logit / logit | AUC 0.663 / 0.674 |
| Y3 log wage | OLS / OLS | MSE 0.697 / 0.695 (R² 0.25) |
| Y4 hours | spline OLS / spline OLS | MSE 117.4 / 120.7 (R² 0.08) |
| Y5 poor | logit / logit | AUC 0.769 / 0.768 |

Lasso and ridge are within ~0.005 AUC of logit (with 100 000 observations regularization changes little); LDA is close; KNN is lower (curse of dimensionality); trees are the weakest (for Y2 the pruned tree has no splits — unemployment is too rare). Mean predicted ≈ mean actual on the test sets, so the probabilities are well calibrated. Y4 is predicted poorly (R² ≈ 0.08), so results for hours are weak.

### 3. Main result (`04_summary/main_results.csv`)

| Outcome | Normal gap 2016→2019 | Covid gap 2019→2022 | **Excess gap** |
|---|---|---|---|
| Y1 employment | +2.19 pp | +1.04 pp | **−1.14 pp** |
| Y2 unemployment | −0.90 pp | −0.09 pp | **+0.81 pp** |
| Y3 real wage | +3.98% | +0.47% | **−3.51%** |
| Y4 hours | −0.02 h | −0.09 h | **−0.07 h (≈ 0)** |
| Y5 poverty | −1.67 pp | +0.28 pp | **+1.94 pp** |

Reading: in 2016→2019 people did better than the 2016 model expected (an improving economy). In 2019→2022 the outcomes are close to what the 2019 model predicts — the economy came back to its 2019 state, but the pre-COVID improvement stopped. The excess gap measures this lost improvement: about 1–2 pp for employment, unemployment and poverty, and 3.5% for real wages (2022 inflation is part of the wage result). Hours did not change.

### 4. Who was affected most (`04_summary/excess_gap_by_group.csv`)

Excess gap by education:

| Education | Y1 | Y2 | Y3 | Y4 | Y5 |
|---|---|---|---|---|---|
| no high school | −0.97 | +1.15 | −5.93% | −0.49 | +3.30 |
| high school | −1.44 | +1.49 | −5.53% | −0.24 | +2.44 |
| some college | −1.64 | +0.96 | −3.43% | −0.10 | +2.11 |
| bachelor | −0.54 | +0.41 | −1.25% | +0.14 | +1.02 |
| graduate | −0.37 | −0.14 | −2.81% | +0.22 | +0.74 |

- The less education, the larger the excess gap — for all outcomes except hours.
- Hispanic vs not Hispanic: Y2 +1.43 vs +0.67 pp, Y3 −5.1% vs −3.2%, Y5 +3.26 vs +1.65 pp.
- Sex: wages fell more for men (−5.2% vs −1.6%); hours fell for men (−0.31 h) and rose for women (+0.22 h).
- Region: employment fell most in the West (−1.93 pp).

### 5. Robustness (`04_summary/robustness_overall.csv`)

| Check | Result |
|---|---|
| Shape of age chosen by CV (`02_robustness/age_shape/`) | best shape: Y1 and Y4 polynomial of degree 4, Y2 and Y3 our `age + age²`, Y5 step function with 8 intervals. Test metric improves only slightly (Y1 AUC 0.7225 → 0.7238). Excess gap changes by less than 0.02 pp / 0.01 h. |
| Alternative implementation (`02_robustness/wage_alt/`, `hours_alt/`) | Y3 −3.22% (main −3.51%), Y4 −0.07 h (main −0.07 h) |
| Race added / children removed (`02_robustness/race_children/`) | Y5 with race: AUC 0.770 (main 0.768), excess gap +1.70 pp (main +1.94); for Hispanics +2.28 instead of +3.26. Without children: excess gap +1.91 pp, AUC drops to 0.761 → children are a useful feature and are kept. |

**Why race is not used.** Its coding changed in 2020: the weighted share of "other" race grows from 9.2% (2019) to 21.1% (2022), while "white" falls from 71.3% to 59.8% (`race_shares_by_year.csv`). In 2016 → 2019 the shares were stable. With race in the model, a 2022 person coded "other" is predicted with the 2019 coefficient of "other" — a different group of people — so race would distort the counterfactual. Removing race costs almost nothing in accuracy (AUC 0.770 → 0.768).

### 6. Clusters of people (`03_clusters/people/`)

PCA on 11 pre-COVID features (7 components = 84% of variance; PC1 = foreign-born / citizenship / years in the US, PC2 = education, family, children), k-means with K = 6. The silhouette has no clear maximum for K > 2 (K = 2 only splits immigrants and US-born), so K = 6 was chosen for interpretability. Cluster shares are the same in 2019 and 2022.

| Cluster | Who (from `cluster_profiles.csv`) | Share | Y1 | Y2 | Y3 | Y4 | Y5 |
|---|---|---|---|---|---|---|---|
| 1 | STEM graduates, mostly US-born | 9% | −0.35 | +0.07 | −1.16% | −0.01 | +0.82 |
| 2 | immigrants without citizenship | 9% | −0.90 | +0.56 | −4.77% | −0.28 | **+4.36** |
| 3 | young single US-born (~31 years) | 34% | −1.22 | +1.08 | −3.09% | −0.07 | +2.07 |
| 4 | naturalized immigrants, ~30 years in the US | 8% | −1.94 | +1.03 | **−6.97%** | −0.27 | +2.55 |
| 5 | mothers of children under 6 | 6% | **−2.14** | **+1.54** | −1.53% | +0.63 | +2.12 |
| 6 | older married US-born (~50 years) | 34% | −1.02 | +0.74 | −3.62% | −0.09 | +1.33 |
| 0 | everyone | 100% | −1.14 | +0.81 | −3.51% | −0.07 | +1.94 |

Immigrants and mothers of young children lost the most; STEM graduates almost nothing.

### 7. Clusters of workers (`03_clusters/workers/`)

PCA on 7 job features (PC1 = wage, hours, education; PC2 = work from home vs commuting), k-means with K = 5.

| Cluster | Type | Real wage (2022 $) | Share 2019 | Share 2022 | Change |
|---|---|---|---|---|---|
| 1 | young, low-wage, shorter hours | 25 591 | 31.0% | 27.4% | −3.6 pp |
| 2 | **work from home** | 70 263 | 4.0% | 14.5% | **+10.5 pp** |
| 3 | self-employed | 48 533 | 4.5% | 5.0% | +0.5 pp |
| 4 | older, middle-wage, commuting | 42 617 | 28.0% | 25.1% | −2.9 pp |
| 5 | high-wage, long hours, highly educated, commuting | 93 901 | 32.5% | 27.9% | −4.6 pp |

The share of remote workers more than tripled; the profiles inside every type barely changed (`cluster_profiles_by_year.csv`), so the change is in the composition of jobs, not in the types themselves.
