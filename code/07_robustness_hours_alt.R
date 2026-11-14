# Aiym: working-hours analysis (Y4), ACS 2016 / 2019 / 2022
# Open this file in a fresh R session and run the complete script with Source.
# Edit DATA_DIR if the cleaned files are stored elsewhere.
# Models and prediction-error metrics are unweighted. Aggregate means use PWGTP.
# Primary specification excludes race; race is a reported sensitivity analysis.
# This is a predeclared primary specification, not a claim that race is unimportant.
# The adjusted forecast gap is not an identified causal effect of COVID.

DATA_DIR <- "data/processed"
RESULT_DIR <- "outputs/02_robustness/hours_alt"

needed <- c("data.table", "glmnet", "Matrix", "rpart", "splines")
for (package in needed) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install the missing package: install.packages(\"", package, "\")")
  }
}
library(data.table)

features <- c(
  "age", "female", "educ", "hispanic", "foreign_born", "citizen",
  "marital", "state", "years_in_us", "birth_region", "stem_degree",
  "degree_field", "children"
)
cp_grid <- c(.01, .003, .001, .0003, .0001, .00005, .00002, .00001, .000005, .000002, 0)
lambda_grid <- c(10^seq(2, -8, length.out = 100), 0)

# Load one year at a time and retain only the eligible hours sample.
load_hours <- function(year) {
  path <- file.path(DATA_DIR, paste0("acs_", year, ".rds"))
  if (!file.exists(path)) stop("Missing input file: ", path)
  d <- as.data.table(readRDS(path))
  columns <- unique(c(features, "race", "year", "SERIALNO", "SPORDER", "PWGTP", "y4_hours"))
  absent <- setdiff(columns, names(d))
  if (length(absent)) stop("Missing columns: ", paste(absent, collapse = ", "))
  if (anyNA(d$year) || any(d$year != year)) stop("Incorrect year in ", path)
  if (anyNA(d$SERIALNO) || anyNA(d$SPORDER) ||
      anyDuplicated(d, by = c("SERIALNO", "SPORDER"))) stop("Invalid person identifiers.")
  d <- d[!is.na(y4_hours), columns, with = FALSE]
  if (!nrow(d) || anyNA(d) || any(!is.finite(d$y4_hours)) ||
      any(d$age < 25 | d$age > 64) || any(!is.finite(d$PWGTP)) ||
      any(d$PWGTP <= 0)) stop("Invalid eligible hours sample for ", year)
  # Preserve cleaned predictor coding; treat race explicitly as categorical.
  d[, race := factor(race)]
  d
}

model_formula <- function(method, parameter = NA_real_, race = FALSE) {
  f <- reformulate(features, response = "y4_hours")
  if (method == "Spline") {
    f <- update(f, paste0(". ~ . - age + splines::ns(age, df = ", parameter, ")"))
  }
  if (race) f <- update(f, . ~ . + race)
  f
}

check_prediction <- function(x) {
  if (any(!is.finite(x))) stop("Missing or nonfinite predictions; inspect predictor levels.")
  invisible(TRUE)
}

fit_basic <- function(d, method, parameter, race = FALSE) {
  f <- model_formula(method, parameter, race)
  if (method == "Tree") {
    rpart::rpart(f, data = d, method = "anova", na.action = na.fail,
                 control = rpart::rpart.control(cp = parameter, minbucket = 200,
                                                maxdepth = 12, xval = 0, maxsurrogate = 0))
  } else {
    lm(f, data = d, na.action = na.fail, model = FALSE)
  }
}

# Matrix columns are defined by the training data and aligned by name.
make_matrix <- function(d, race = FALSE) {
  Matrix::sparse.model.matrix(
    reformulate(c(features, if (race) "race")), data = d
  )[, -1, drop = FALSE]
}

# Ridge/Lasso CV: one fold at a time, predictions in batches of 5,000 rows.
penalty_cv <- function(d, folds, race = FALSE, methods = c("Ridge", "Lasso")) {
  x <- make_matrix(d, race)
  y <- d$y4_hours
  results <- list()
  for (method in methods) {
    errors <- matrix(NA_real_, 5, length(lambda_grid))
    for (k in 1:5) {
      tr <- which(folds != k)
      va <- which(folds == k)
      fit <- glmnet::glmnet(x[tr, , drop = FALSE], y[tr], family = "gaussian",
                            alpha = if (method == "Ridge") 0 else 1,
                            lambda = lambda_grid, standardize = TRUE)
      if (fit$jerr != 0 || !isTRUE(all.equal(as.numeric(fit$lambda), lambda_grid))) {
        stop("Incomplete penalty path for ", method, "; do not use partial results.")
      }
      sse <- numeric(length(lambda_grid))
      for (start in seq.int(1L, length(va), by = 5000L)) {
        rows <- va[start:min(start + 4999L, length(va))]
        pred <- predict(fit, newx = x[rows, , drop = FALSE], s = lambda_grid)
        check_prediction(pred)
        sse <- sse + colSums(sweep(pred, 1, y[rows], "-")^2)
        rm(pred)
      }
      errors[k, ] <- sse / length(va)
      rm(fit)
      gc(verbose = FALSE)
      cat(method, "completed fold", k, "of 5.\n")
    }
    results[[method]] <- data.table(model = method, parameter = lambda_grid,
                                    cv_MSE = colMeans(errors))
  }
  rbindlist(results)
}

# Compare baseline, linear regression, splines, trees, Ridge and Lasso.
compare_hours <- function(d, folds) {
  specs <- data.table(model = c("Linear", rep("Spline", 3)), parameter = c(NA, 3:5))
  basic_errors <- matrix(NA_real_, 5, nrow(specs))
  tree_errors <- matrix(NA_real_, 5, length(cp_grid))
  baseline <- numeric(5)
  for (k in 1:5) {
    tr <- d[folds != k]
    va <- d[folds == k]
    baseline[k] <- mean((va$y4_hours - mean(tr$y4_hours))^2)
    for (j in seq_len(nrow(specs))) {
      fit <- fit_basic(tr, specs$model[j], specs$parameter[j])
      pred <- predict(fit, newdata = va)
      check_prediction(pred)
      basic_errors[k, j] <- mean((va$y4_hours - pred)^2)
      rm(fit, pred)
      gc(verbose = FALSE)
    }
    tree <- fit_basic(tr, "Tree", 0)
    for (j in seq_along(cp_grid)) {
      pred <- predict(rpart::prune(tree, cp = cp_grid[j]), newdata = va)
      check_prediction(pred)
      tree_errors[k, j] <- mean((va$y4_hours - pred)^2)
    }
    rm(tr, va, tree, pred)
    gc(verbose = FALSE)
    cat("Linear, spline and tree models: completed fold", k, "of 5.\n")
  }
  specs[, cv_MSE := colMeans(basic_errors)]
  tuning <- rbindlist(list(
    data.table(model = "Mean baseline", parameter = NA_real_, cv_MSE = mean(baseline)),
    specs,
    data.table(model = "Tree", parameter = cp_grid, cv_MSE = colMeans(tree_errors)),
    penalty_cv(d, folds)
  ))
  tuning[, cv_RMSE := sqrt(cv_MSE)]
  # Select each family's best setting, then choose the lowest CV MSE.
  best <- tuning[order(cv_MSE), .SD[1L], by = model]
  setorder(best, cv_MSE)
  selected <- best[model != "Mean baseline"][1L]
  list(comparison = best, tuning = tuning, selected = selected)
}

# Fit and predict a selected model; supports all candidate model families.
predict_selected <- function(train, target, selected, race = FALSE) {
  method <- selected$model[1]
  parameter <- selected$parameter[1]
  pred <- numeric(nrow(target))
  if (method %in% c("Ridge", "Lasso")) {
    x <- make_matrix(train, race)
    fit <- glmnet::glmnet(x, train$y4_hours, family = "gaussian",
                          alpha = if (method == "Ridge") 0 else 1,
                          lambda = lambda_grid, standardize = TRUE)
    if (fit$jerr != 0 || !isTRUE(all.equal(as.numeric(fit$lambda), lambda_grid))) {
      stop("Incomplete penalty path in selected model.")
    }
    column_names <- colnames(x)
    rm(x)
    gc(verbose = FALSE)
    for (start in seq.int(1L, nrow(target), by = 5000L)) {
      rows <- start:min(start + 4999L, nrow(target))
      batch <- copy(target[rows])
      # Reject unseen categories instead of silently recoding them.
      for (v in c(features, if (race) "race")) {
        if (is.factor(train[[v]]) || is.character(train[[v]])) {
          levels_train <- if (is.factor(train[[v]])) levels(train[[v]]) else sort(unique(train[[v]]))
          if (any(!as.character(batch[[v]]) %in% levels_train)) stop("Unseen category in ", v)
          set(batch, j = v, value = factor(batch[[v]], levels = levels_train,
                                           ordered = is.ordered(train[[v]])))
        }
      }
      xb <- make_matrix(batch, race)
      if (!setequal(colnames(xb), column_names)) stop("Predictor matrix columns differ.")
      pred[rows] <- as.numeric(predict(fit, newx = xb[, column_names, drop = FALSE], s = parameter))
    }
  } else {
    fit <- fit_basic(train, method, parameter, race)
    for (start in seq.int(1L, nrow(target), by = 20000L)) {
      rows <- start:min(start + 19999L, nrow(target))
      pred[rows] <- predict(fit, newdata = target[rows])
    }
  }
  check_prediction(pred)
  pred
}

# Race sensitivity holds the selected method and tuning parameter fixed.
compare_race <- function(train, folds, selected) {
  fold_mse <- numeric(5)
  for (k in 1:5) {
    pred <- predict_selected(train[folds != k], train[folds == k], selected, race = TRUE)
    fold_mse[k] <- mean((train$y4_hours[folds == k] - pred)^2)
    rm(pred)
    gc(verbose = FALSE)
    cat("Race sensitivity: completed fold", k, "of 5.\n")
  }
  data.table(
    specification = c("Without race", "With race"),
    model = selected$model, parameter = selected$parameter,
    cv_MSE = c(selected$cv_MSE, mean(fold_mse)),
    reduction_percent_vs_without_race = c(0, 100 * (1 - mean(fold_mse) / selected$cv_MSE))
  )[, cv_RMSE := sqrt(cv_MSE)]
}

# Execute the complete analysis. Only final project tables are written to disk.
run_hours_analysis <- function() {
  dir.create(RESULT_DIR, recursive = TRUE, showWarnings = FALSE)
  output <- list()
  save_table <- function(name, value) {
    fwrite(value, file.path(RESULT_DIR, paste0(name, ".csv")))
    print(value)
  }
  # Independent fixed seeds make each year reproducible when run on its own.
  split_seeds <- c("2016" = 2016L, "2019" = 2019L)
  fold_seeds <- c("2016" = 12016L, "2019" = 12019L)
  tests <- list()
  forecasts <- list()
  descriptions <- list()
  selections <- list()
  for (year in c(2016L, 2019L)) {
    cat("\nSTARTING HOURS ANALYSIS FOR", year, "\n")
    d <- load_hours(year)
    set.seed(split_seeds[as.character(year)])
    train_id <- sample.int(nrow(d), floor(.70 * nrow(d)))
    train <- d[train_id]
    test <- d[-train_id]
    set.seed(fold_seeds[as.character(year)])
    folds <- sample(rep(1:5, length.out = nrow(train)))
    cv <- compare_hours(train, folds)
    cv$comparison[, `:=`(year = year, outcome = "y4_hours", race_included = FALSE)]
    save_table(paste0("cv_results", year), cv$comparison)
    # Retain all tuning results in one table for reproducibility.
    save_table(paste0("tuning_results", year), cv$tuning)
    selected <- cv$selected
    race_results <- compare_race(train, folds, selected)
    save_table(paste0("race_comparison", year), race_results)
    cat("Primary specification remains without race; see the sensitivity table.\n")
    pred <- predict_selected(train, test, selected)
    err <- test$y4_hours - pred
    baseline <- mean((test$y4_hours - mean(train$y4_hours))^2)
    tests[[as.character(year)]] <- data.table(
      year = year, model = selected$model, parameter = selected$parameter,
      race_included = FALSE, training_rows = nrow(train), test_rows = nrow(test),
      test_MSE = mean(err^2), test_RMSE = sqrt(mean(err^2)), test_MAE = mean(abs(err)),
      test_R2 = 1 - sum(err^2) / sum((test$y4_hours - mean(test$y4_hours))^2),
      baseline_test_MSE = baseline, mse_reduction_percent = 100 * (1 - mean(err^2) / baseline),
      split_seed = split_seeds[as.character(year)], fold_seed = fold_seeds[as.character(year)]
    )
    save_table("hours_test_results", rbindlist(tests))
    selections[[as.character(year)]] <- selected
    descriptions[[as.character(year)]] <- data.table(year = year, observations = nrow(d),
                                                     weighted_mean_hours = weighted.mean(d$y4_hours, d$PWGTP))
    rm(train, test, pred, err, folds, train_id)
    gc(verbose = FALSE)
    target_year <- year + 3L
    target <- load_hours(target_year)
    cat("Refitting on the full", year, "sample and predicting", target_year, "\n")
    pred <- predict_selected(d, target, selected)
    forecasts[[as.character(year)]] <- data.table(
      training_year = year, target_year = target_year, target_rows = nrow(target),
      model = selected$model, parameter = selected$parameter,
      actual_mean_hours = weighted.mean(target$y4_hours, target$PWGTP),
      predicted_mean_hours = weighted.mean(pred, target$PWGTP),
      actual_minus_predicted = weighted.mean(target$y4_hours - pred, target$PWGTP)
    )
    save_table("hours_crossyear_results", rbindlist(forecasts))
    if (target_year == 2022L) {
      descriptions[["2022"]] <- data.table(year = 2022L, observations = nrow(target),
                                           weighted_mean_hours = weighted.mean(target$y4_hours, target$PWGTP))
    }
    output[[as.character(year)]] <- list(comparison = cv$comparison, race = race_results)
    rm(d, target, pred, cv)
    gc(verbose = FALSE)
  }
  crossyear <- rbindlist(forecasts)
  prepandemic_gap <- crossyear[target_year == 2019, actual_minus_predicted]
  predicted2022 <- crossyear[target_year == 2022, predicted_mean_hours]
  actual2022 <- crossyear[target_year == 2022, actual_mean_hours]
  gap <- data.table(
    outcome = "y4_hours", prepandemic_actual_minus_predicted = prepandemic_gap,
    predicted_mean_2022 = predicted2022,
    adjusted_predicted_mean_2022 = predicted2022 + prepandemic_gap,
    actual_mean_2022 = actual2022,
    adjusted_prediction_minus_actual = predicted2022 + prepandemic_gap - actual2022
  )
  # Positive = actual hours below the adjusted benchmark; units are hours/week.
  save_table("hours_gap_results", gap)
  save_table("hours_descriptive_stats", rbindlist(descriptions))
  writeLines(capture.output(sessionInfo()), file.path(RESULT_DIR, "sessionInfo.txt"))
  writeLines(c(
    "Aiym: Y4 working-hours analysis", "",
    "Eligible sample: nonmissing cleaned y4_hours, ages 25-64.",
    "70/30 split; five-fold CV within training data; final held-out test evaluation.",
    "Linear, age spline (df 3-5), Ridge, Lasso and regression tree compared.",
    "Primary model excludes race by design. Race sensitivity holds tuning fixed.",
    "Model fitting and evaluation are unweighted; aggregate means use PWGTP.",
    "Selected models are refitted on full source-year samples for cross-year forecasts.",
    "2016-to-2019 mean forecast gap is assumed to carry forward to 2019-to-2022.",
    "Final sign: adjusted prediction minus actual; units: hours per week.",
    "This forecast gap does not identify the causal effect of COVID.",
    "No raw data, fitted model files or person-level predictions are exported."
  ), file.path(RESULT_DIR, "README.txt"))
  output$test_results <- rbindlist(tests)
  output$crossyear_results <- crossyear
  output$gap_results <- gap
  message("HOURS ANALYSIS COMPLETE. Results: ", RESULT_DIR)
  output
}

# Run once. Results are also available in hours_results in the Environment.
hours_results <- run_hours_analysis()
