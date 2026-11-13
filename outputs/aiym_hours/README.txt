Aiym: Y4 working-hours analysis

Eligible sample: nonmissing cleaned y4_hours, ages 25-64.
70/30 split; five-fold CV within training data; final held-out test evaluation.
Linear, age spline (df 3-5), Ridge, Lasso and regression tree compared.
Primary model excludes race by design. Race sensitivity holds tuning fixed.
Model fitting and evaluation are unweighted; aggregate means use PWGTP.
Selected models are refitted on full source-year samples for cross-year forecasts.
2016-to-2019 mean forecast gap is assumed to carry forward to 2019-to-2022.
Final sign: adjusted prediction minus actual; units: hours per week.
This forecast gap does not identify the causal effect of COVID.
No raw data, fitted model files or person-level predictions are exported.
