# Aiym: ACS wage analysis (Y3), 2016 / 2019 / 2022
# Set DATA_DIR to the directory containing the three cleaned RDS files.
# Model fitting and prediction-error metrics are unweighted.
# Descriptive means and cross-year mean gaps use target-year PWGTP weights.
# The same 70/30 splits, five folds and tuning grids as the original analysis are retained.
# The final df = 5 specification without race reflects the completed CV/race comparisons.
# The adjusted forecast gap is a descriptive benchmark, not an identified causal effect.
# Hours are included only in sample checks and descriptive statistics; Y4 is not modeled.

# Configuration

# Directory containing the three cleaned datasets
DATA_DIR <- "C:/Users/43354/Desktop/cleandata"

# Directory for saving results
RESULT_DIR <- file.path(DATA_DIR, "outputs", "aiym_wages")

Y_TO_RUN <- c("y3_log_wage", "y4_hours")

# Check required packages

needed <- c(
  "data.table", "glmnet", "Matrix",
  "rpart", "splines"
)

for (package in needed) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package '", package,
      "' is not installed. Run: install.packages(\"", package, "\")"
    )
  }
}

library(data.table)

# Check that the input files exist

files <- file.path(
  DATA_DIR,
  c("acs_2016.rds", "acs_2019.rds", "acs_2022.rds")
)

file_check <- data.table(
  file = basename(files),
  found = file.exists(files)
)

print(file_check)

if (!all(file_check$found)) {
  stop(
    "Missing files: ",
    paste(file_check$file[!file_check$found], collapse = ", "),
    ". Check DATA_DIR."
  )
}

# Load the datasets into separate tables

acs2016 <- as.data.table(readRDS(files[1]))
acs2019 <- as.data.table(readRDS(files[2]))
acs2022 <- as.data.table(readRDS(files[3]))

# Define predictors for models excluding race

features <- c(
  "age", "female", "educ", "hispanic",
  "foreign_born", "citizen", "marital", "state",
  "years_in_us", "birth_region", "stem_degree",
  "degree_field", "children"
)

required_columns <- c(
  features,
  "year", "SERIALNO", "SPORDER", "PWGTP", "region",
  "y3_log_wage", "y4_hours", "wage_real"
)

# Validate the datasets and create a summary table

# Apply the same checks to all three datasets
datasets <- list(
  "2016" = acs2016,
  "2019" = acs2019,
  "2022" = acs2022
)

summary_rows <- list()

for (yr in names(datasets)) {
  
  d <- datasets[[yr]]
  
  # Check that all required columns are present
  missing_columns <- setdiff(required_columns, names(d))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing columns in the ", yr, " dataset: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # Verify that the year variable matches the dataset year
  if (anyNA(d$year) || any(d$year != as.integer(yr))) {
    stop("Missing or incorrect year values in the ", yr, " dataset.")
  }
  
  # Verify that the full cleaned sample contains ages 18–64
  if (anyNA(d$age) || any(d$age < 18 | d$age > 64)) {
    stop(
      "Missing ages or ages outside the 18–64 range in the ",
      yr, " dataset."
    )
  }
  
  # Check for missing identifiers and duplicate person records
  if (anyNA(d$SERIALNO) || anyNA(d$SPORDER)) {
    stop("Missing person identifiers in the ", yr, " dataset.")
  }
  
  if (anyDuplicated(d, by = c("SERIALNO", "SPORDER")) > 0) {
    stop("Duplicate person records found in the ", yr, " dataset.")
  }
  
  for (outcome in Y_TO_RUN) {
    
    # Retain observations with a nonmissing outcome
    sample_data <- d[!is.na(get(outcome))]
    
    if (nrow(sample_data) == 0) {
      stop("No eligible observations for ", outcome, " in ", yr, ".")
    }
    
    # Under the current project specification, Y3 and Y4 cover ages 25–64
    if (any(sample_data$age < 25 | sample_data$age > 64)) {
      stop(
        "Ages outside the 25–64 range for ",
        outcome, " in ", yr, "."
      )
    }
    
    # Check that outcome values are finite
    if (any(!is.finite(sample_data[[outcome]]))) {
      stop("Nonfinite values found for ", outcome, " in ", yr, ".")
    }
    
    # Verify that person weights are finite and strictly positive
    if (any(!is.finite(sample_data$PWGTP)) ||
        any(sample_data$PWGTP <= 0)) {
      stop(
        "Missing, nonfinite, or nonpositive PWGTP weights for ",
        outcome, " in ", yr, "."
      )
    }
    
    # Check predictors for missing values
    missing_counts <- colSums(is.na(sample_data[, ..features]))
    
    if (any(missing_counts > 0)) {
      stop(
        "Missing predictor values for ", outcome, " in ", yr, ": ",
        paste(names(missing_counts)[missing_counts > 0], collapse = ", ")
      )
    }
    
    # Verify the precomputed log wage against the real wage variable
    if (outcome == "y3_log_wage") {
      
      if (any(!is.finite(sample_data$wage_real)) ||
          any(sample_data$wage_real <= 0)) {
        stop(
          "Missing, nonfinite, or nonpositive real wages in ",
          yr, "."
        )
      }
      
      difference <- abs(
        sample_data$y3_log_wage - log(sample_data$wage_real)
      )
      
      if (any(difference > 1e-8)) {
        stop(
          "y3_log_wage does not match log(wage_real) in ",
          yr, "."
        )
      }
    }
    
    # Add sample sizes and the weighted outcome mean to the summary
    summary_rows[[paste(yr, outcome)]] <- data.table(
      year = as.integer(yr),
      outcome = outcome,
      file_rows = nrow(d),
      eligible_rows = nrow(sample_data),
      weighted_mean = weighted.mean(
        sample_data[[outcome]],
        w = sample_data$PWGTP
      )
    )
  }
}

input_checks <- rbindlist(summary_rows)

print(input_checks)

message("All checks passed. The datasets are ready for the next step.")

# Create outcome-specific samples

# Wage samples: observations with a nonmissing log wage
wages2016 <- acs2016[!is.na(y3_log_wage)]
wages2019 <- acs2019[!is.na(y3_log_wage)]
wages2022 <- acs2022[!is.na(y3_log_wage)]

# Hours samples: observations with nonmissing working hours
hours2016 <- acs2016[!is.na(y4_hours)]
hours2019 <- acs2019[!is.na(y4_hours)]
hours2022 <- acs2022[!is.na(y4_hours)]

# Compare sample sizes across years

sample_sizes <- data.table(
  year = c(2016, 2019, 2022),
  wage_observations = c(
    nrow(wages2016),
    nrow(wages2019),
    nrow(wages2022)
  ),
  hours_observations = c(
    nrow(hours2016),
    nrow(hours2019),
    nrow(hours2022)
  )
)

print(sample_sizes)

# Calculate weighted descriptive statistics

# Person weights are used to calculate population-weighted means
descriptive_stats <- data.table(
  year = c(2016, 2019, 2022),
  
  mean_real_wage = c(
    weighted.mean(wages2016$wage_real, wages2016$PWGTP),
    weighted.mean(wages2019$wage_real, wages2019$PWGTP),
    weighted.mean(wages2022$wage_real, wages2022$PWGTP)
  ),
  
  mean_log_wage = c(
    weighted.mean(wages2016$y3_log_wage, wages2016$PWGTP),
    weighted.mean(wages2019$y3_log_wage, wages2019$PWGTP),
    weighted.mean(wages2022$y3_log_wage, wages2022$PWGTP)
  ),
  
  mean_weekly_hours = c(
    weighted.mean(hours2016$y4_hours, hours2016$PWGTP),
    weighted.mean(hours2019$y4_hours, hours2019$PWGTP),
    weighted.mean(hours2022$y4_hours, hours2022$PWGTP)
  )
)

print(descriptive_stats)

# Split wage samples into training and test sets

# Set a seed to make the random split reproducible
set.seed(1)

# Select 70% of the 2016 observations for training
train_id2016 <- sample.int(
  n = nrow(wages2016),
  size = floor(0.70 * nrow(wages2016))
)

train_wage2016 <- wages2016[train_id2016]
test_wage2016 <- wages2016[-train_id2016]

# Select 70% of the 2019 observations for training
train_id2019 <- sample.int(
  n = nrow(wages2019),
  size = floor(0.70 * nrow(wages2019))
)

train_wage2019 <- wages2019[train_id2019]
test_wage2019 <- wages2019[-train_id2019]

# Check training and test sample sizes

split_summary <- data.table(
  year = c(2016, 2019),
  
  training_rows = c(
    nrow(train_wage2016),
    nrow(train_wage2019)
  ),
  
  test_rows = c(
    nrow(test_wage2016),
    nrow(test_wage2019)
  )
)

split_summary[, training_share :=
                training_rows / (training_rows + test_rows)]

print(split_summary)

# Create five cross-validation folds within each training set

set.seed(2)

fold_wage2016 <- sample(
  rep(1:5, length.out = nrow(train_wage2016))
)

fold_wage2019 <- sample(
  rep(1:5, length.out = nrow(train_wage2019))
)

# Define the linear regression model without race

wage_formula <- y3_log_wage ~
  age + female + educ + hispanic +
  foreign_born + citizen + marital + state +
  years_in_us + birth_region + stem_degree +
  degree_field + children

# Evaluate linear regression using five-fold cross-validation

cv_lm2016 <- numeric(5)
cv_baseline2016 <- numeric(5)

for (k in 1:5) {
  
  # Use four folds for training and one fold for validation
  training_fold <- train_wage2016[fold_wage2016 != k]
  validation_fold <- train_wage2016[fold_wage2016 == k]
  
  # Fit linear regression on the four training folds
  fit_lm <- lm(
    formula = wage_formula,
    data = training_fold,
    na.action = na.fail
  )
  
  # Predict log wages for the validation fold
  predictions <- predict(
    fit_lm,
    newdata = validation_fold
  )
  
  actual <- validation_fold$y3_log_wage
  
  # Stop if any predictions are missing or nonfinite
  if (any(!is.finite(predictions))) {
    stop("Missing or nonfinite predictions in fold ", k, ".")
  }
  
  # Calculate the validation MSE for linear regression
  cv_lm2016[k] <- mean((actual - predictions)^2)
  
  # Calculate the baseline using only the training folds
  baseline_prediction <- mean(training_fold$y3_log_wage)
  
  cv_baseline2016[k] <- mean(
    (actual - baseline_prediction)^2
  )
  
  cat(
    "Fold", k,
    "| Linear regression MSE:", round(cv_lm2016[k], 6),
    "| Baseline MSE:", round(cv_baseline2016[k], 6),
    "\n"
  )
}

# Compare cross-validation results

cv_results2016 <- data.table(
  year = 2016,
  outcome = "y3_log_wage",
  model = c("Mean baseline", "Linear regression"),
  race_included = FALSE,
  cv_MSE = c(
    mean(cv_baseline2016),
    mean(cv_lm2016)
  )
)

cv_results2016[, cv_RMSE := sqrt(cv_MSE)]

print(cv_results2016)

# Percentage reduction in MSE relative to the mean baseline
mse_reduction2016 <- 100 * (
  1 - mean(cv_lm2016) / mean(cv_baseline2016)
)

cat(
  "MSE reduction relative to baseline:",
  round(mse_reduction2016, 2),
  "%\n"
)

# Compare age spline specifications using cross-validation

df_values <- c(3, 4, 5)

spline_results2016 <- data.table(
  df = df_values,
  cv_MSE = NA_real_
)

for (j in seq_along(df_values)) {
  
  # Replace linear age with a natural cubic spline
  spline_formula <- update(
    wage_formula,
    paste0(
      ". ~ . - age + splines::ns(age, df = ",
      df_values[j],
      ")"
    )
  )
  
  fold_mse <- numeric(5)
  
  for (k in 1:5) {
    
    # Use the same folds as in the linear regression comparison
    training_fold <- train_wage2016[fold_wage2016 != k]
    validation_fold <- train_wage2016[fold_wage2016 == k]
    
    # Estimate the spline model using only the training folds
    fit_spline <- lm(
      formula = spline_formula,
      data = training_fold,
      na.action = na.fail
    )
    
    # Apply the fitted spline transformation to validation data
    predictions <- predict(
      fit_spline,
      newdata = validation_fold
    )
    
    actual <- validation_fold$y3_log_wage
    
    if (any(!is.finite(predictions))) {
      stop(
        "Missing or nonfinite predictions for df = ",
        df_values[j], " in fold ", k, "."
      )
    }
    
    fold_mse[k] <- mean((actual - predictions)^2)
  }
  
  spline_results2016$cv_MSE[j] <- mean(fold_mse)
  
  cat(
    "Completed: df =", df_values[j],
    "| CV MSE =", round(mean(fold_mse), 6),
    "\n"
  )
}

spline_results2016[, cv_RMSE := sqrt(cv_MSE)]

print(spline_results2016)

# Select the spline specification with the lowest CV MSE

best_spline_row <- which.min(spline_results2016$cv_MSE)
best_df2016 <- spline_results2016$df[best_spline_row]

cat("Selected spline degrees of freedom:", best_df2016, "\n")

# Add the selected spline model to the comparison table

# Remove an earlier spline entry if this block is rerun
cv_results2016 <- cv_results2016[
  !grepl("^Age spline", model)
]

cv_results2016 <- rbindlist(list(
  cv_results2016,
  data.table(
    year = 2016,
    outcome = "y3_log_wage",
    model = paste0("Age spline (df = ", best_df2016, ")"),
    race_included = FALSE,
    cv_MSE = spline_results2016$cv_MSE[best_spline_row],
    cv_RMSE = spline_results2016$cv_RMSE[best_spline_row]
  )
))

print(cv_results2016)

# Prepare the predictor matrix for Ridge and Lasso

library(glmnet)

# Convert categorical predictors into dummy variables
# Remove the intercept column: glmnet estimates its own intercept
x_train2016 <- Matrix::sparse.model.matrix(
  wage_formula,
  data = train_wage2016
)[, -1, drop = FALSE]

y_train2016 <- train_wage2016$y3_log_wage

stopifnot(
  nrow(x_train2016) == length(y_train2016),
  length(y_train2016) == length(fold_wage2016)
)

# Include zero to allow a model without a penalty
lambda_grid <- c(
  10^seq(2, -8, length.out = 100),
  0
)

# Tune Ridge regression using five-fold cross-validation

ridge_cv2016 <- cv.glmnet(
  x = x_train2016,
  y = y_train2016,
  family = "gaussian",
  alpha = 0,
  lambda = lambda_grid,
  foldid = fold_wage2016,
  type.measure = "mse",
  standardize = TRUE
)

cat("Ridge cross-validation completed.\n")

# Tune Lasso regression using five-fold cross-validation

lasso_cv2016 <- cv.glmnet(
  x = x_train2016,
  y = y_train2016,
  family = "gaussian",
  alpha = 1,
  lambda = lambda_grid,
  foldid = fold_wage2016,
  type.measure = "mse",
  standardize = TRUE
)

cat("Lasso cross-validation completed.\n")

# Compare the selected penalties and validation errors

regularization_results2016 <- data.table(
  model = c("Ridge", "Lasso"),
  
  best_lambda = c(
    ridge_cv2016$lambda.min,
    lasso_cv2016$lambda.min
  ),
  
  cv_MSE = c(
    min(ridge_cv2016$cvm),
    min(lasso_cv2016$cvm)
  )
)

regularization_results2016[, cv_RMSE := sqrt(cv_MSE)]

print(regularization_results2016)

# Add Ridge and Lasso to the model comparison table

# Remove previous entries if this block is rerun
cv_results2016 <- cv_results2016[
  !model %in% c("Ridge", "Lasso")
]

cv_results2016 <- rbindlist(list(
  cv_results2016,
  data.table(
    year = 2016,
    outcome = "y3_log_wage",
    model = regularization_results2016$model,
    race_included = FALSE,
    cv_MSE = regularization_results2016$cv_MSE,
    cv_RMSE = regularization_results2016$cv_RMSE
  )
))

# Display models from lowest to highest validation MSE
print(cv_results2016[order(cv_MSE)])

# Define regression tree complexity values

library(rpart)

cp_values <- c(
  0.01, 0.003, 0.001, 0.0003,
  0.0001, 0.00005, 0.00002, 0.00001
)

tree_mse2016 <- matrix(
  NA_real_,
  nrow = 5,
  ncol = length(cp_values)
)

# Evaluate regression trees using the same five folds

for (k in 1:5) {
  
  training_fold <- train_wage2016[fold_wage2016 != k]
  validation_fold <- train_wage2016[fold_wage2016 == k]
  
  # Grow a tree at the smallest cp in the candidate grid
  tree_fit <- rpart(
    formula = wage_formula,
    data = training_fold,
    method = "anova",
    na.action = na.fail,
    control = rpart.control(
      cp = min(cp_values),
      minbucket = 200,
      maxdepth = 12,
      xval = 0,
      maxsurrogate = 0
    )
  )
  
  # Prune the fitted tree to each candidate complexity level
  for (j in seq_along(cp_values)) {
    
    smaller_tree <- prune(
      tree_fit,
      cp = cp_values[j]
    )
    
    predictions <- predict(
      smaller_tree,
      newdata = validation_fold
    )
    
    if (any(!is.finite(predictions))) {
      stop(
        "Missing or nonfinite predictions for cp = ",
        cp_values[j], " in fold ", k, "."
      )
    }
    
    tree_mse2016[k, j] <- mean(
      (validation_fold$y3_log_wage - predictions)^2
    )
  }
  
  cat("Completed fold", k, "of 5.\n")
}

# Compare tree complexity settings

tree_results2016 <- data.table(
  cp = cp_values,
  cv_MSE = colMeans(tree_mse2016)
)

tree_results2016[, cv_RMSE := sqrt(cv_MSE)]

print(tree_results2016)

best_tree_row2016 <- which.min(tree_results2016$cv_MSE)
best_cp2016 <- tree_results2016$cp[best_tree_row2016]

cat("Best cp in the tested grid:", best_cp2016, "\n")

# Add the best tree to the model comparison table

# Remove the previous tree entry if this block is rerun
cv_results2016 <- cv_results2016[
  model != "Regression tree"
]

cv_results2016 <- rbindlist(list(
  cv_results2016,
  data.table(
    year = 2016,
    outcome = "y3_log_wage",
    model = "Regression tree",
    race_included = FALSE,
    cv_MSE = tree_results2016$cv_MSE[best_tree_row2016],
    cv_RMSE = tree_results2016$cv_RMSE[best_tree_row2016]
  )
))

print(cv_results2016[order(cv_MSE)])

# Check smaller tree complexity values

cp_extra <- c(0.000005, 0.000002, 0)

tree_mse_extra2016 <- matrix(
  NA_real_,
  nrow = 5,
  ncol = length(cp_extra)
)

for (k in 1:5) {
  
  training_fold <- train_wage2016[fold_wage2016 != k]
  validation_fold <- train_wage2016[fold_wage2016 == k]
  
  # Grow a tree without cp-based stopping
  # Minimum leaf size and maximum depth still limit complexity
  tree_fit <- rpart(
    formula = wage_formula,
    data = training_fold,
    method = "anova",
    na.action = na.fail,
    control = rpart.control(
      cp = 0,
      minbucket = 200,
      maxdepth = 12,
      xval = 0,
      maxsurrogate = 0
    )
  )
  
  for (j in seq_along(cp_extra)) {
    
    smaller_tree <- prune(
      tree_fit,
      cp = cp_extra[j]
    )
    
    predictions <- predict(
      smaller_tree,
      newdata = validation_fold
    )
    
    if (any(!is.finite(predictions))) {
      stop(
        "Missing or nonfinite predictions for cp = ",
        cp_extra[j], " in fold ", k, "."
      )
    }
    
    tree_mse_extra2016[k, j] <- mean(
      (validation_fold$y3_log_wage - predictions)^2
    )
  }
  
  cat("Completed fold", k, "of 5.\n")
}

# Combine the original and additional tree results

tree_extra_results2016 <- data.table(
  cp = cp_extra,
  cv_MSE = colMeans(tree_mse_extra2016)
)

tree_extra_results2016[, cv_RMSE := sqrt(cv_MSE)]

# Remove matching entries to allow this block to be rerun
tree_results2016 <- tree_results2016[!cp %in% cp_extra]

tree_results2016 <- rbindlist(list(
  tree_results2016,
  tree_extra_results2016
))

setorder(tree_results2016, -cp)

print(tree_results2016)

best_tree_row2016 <- which.min(tree_results2016$cv_MSE)
best_cp2016 <- tree_results2016$cp[best_tree_row2016]

cat("Selected cp:", best_cp2016, "\n")

# Update the model comparison table

cv_results2016 <- cv_results2016[
  model != "Regression tree"
]

cv_results2016 <- rbindlist(list(
  cv_results2016,
  data.table(
    year = 2016,
    outcome = "y3_log_wage",
    model = "Regression tree",
    race_included = FALSE,
    cv_MSE = tree_results2016$cv_MSE[best_tree_row2016],
    cv_RMSE = tree_results2016$cv_RMSE[best_tree_row2016]
  )
))

print(cv_results2016[order(cv_MSE)])

# Check the race variable

if (!"race" %in% names(train_wage2016)) {
  stop("The race variable is missing from train_wage2016.")
}

if (anyNA(train_wage2016$race)) {
  stop("The race variable contains missing values.")
}

# Inspect the number of observations in each race category
print(table(train_wage2016$race, useNA = "ifany"))

# Define the selected spline model with and without race

spline_formula_no_race2016 <- update(
  wage_formula,
  paste0(
    ". ~ . - age + splines::ns(age, df = ",
    best_df2016,
    ")"
  )
)

# Treat race as a categorical predictor
spline_formula_with_race2016 <- update(
  spline_formula_no_race2016,
  . ~ . + factor(race)
)

# Compare both specifications using the same five folds

race_fold_results2016 <- data.table(
  fold = 1:5,
  mse_without_race = NA_real_,
  mse_with_race = NA_real_
)

for (k in 1:5) {
  
  training_fold <- train_wage2016[fold_wage2016 != k]
  validation_fold <- train_wage2016[fold_wage2016 == k]
  
  # Fit the selected spline model without race
  fit_without_race <- lm(
    formula = spline_formula_no_race2016,
    data = training_fold,
    na.action = na.fail
  )
  
  # Fit the same model with race added
  fit_with_race <- lm(
    formula = spline_formula_with_race2016,
    data = training_fold,
    na.action = na.fail
  )
  
  prediction_without_race <- predict(
    fit_without_race,
    newdata = validation_fold
  )
  
  prediction_with_race <- predict(
    fit_with_race,
    newdata = validation_fold
  )
  
  if (any(!is.finite(prediction_without_race)) ||
      any(!is.finite(prediction_with_race))) {
    stop("Missing or nonfinite predictions in fold ", k, ".")
  }
  
  actual <- validation_fold$y3_log_wage
  
  race_fold_results2016$mse_without_race[k] <- mean(
    (actual - prediction_without_race)^2
  )
  
  race_fold_results2016$mse_with_race[k] <- mean(
    (actual - prediction_with_race)^2
  )
  
  cat("Completed fold", k, "of 5.\n")
}

# Summarize the change in prediction error

# Positive values indicate lower MSE when race is included
race_fold_results2016[, mse_reduction :=
                        mse_without_race - mse_with_race]

race_comparison2016 <- data.table(
  specification = c("Without race", "With race"),
  cv_MSE = c(
    mean(race_fold_results2016$mse_without_race),
    mean(race_fold_results2016$mse_with_race)
  )
)

race_comparison2016[, cv_RMSE := sqrt(cv_MSE)]

race_improvement2016 <- data.table(
  mse_reduction = mean(race_fold_results2016$mse_reduction),
  
  reduction_percent = 100 * (
    1 -
      mean(race_fold_results2016$mse_with_race) /
      mean(race_fold_results2016$mse_without_race)
  )
)

print(race_fold_results2016)
print(race_comparison2016)
print(race_improvement2016)

# Fit the selected spline model without race

final_wage_model2016 <- lm(
  formula = spline_formula_no_race2016,
  data = train_wage2016,
  na.action = na.fail
)

# Predict log wages for the held-out test sample

test_predictions2016 <- predict(
  final_wage_model2016,
  newdata = test_wage2016
)

test_actual2016 <- test_wage2016$y3_log_wage

if (any(!is.finite(test_predictions2016))) {
  stop("Missing or nonfinite predictions in the test sample.")
}

# Calculate unweighted test performance metrics

test_mse2016 <- mean(
  (test_actual2016 - test_predictions2016)^2
)

test_rmse2016 <- sqrt(test_mse2016)

test_mae2016 <- mean(
  abs(test_actual2016 - test_predictions2016)
)

# Test R-squared uses the observed test-sample mean
test_r2_2016 <- 1 -
  sum((test_actual2016 - test_predictions2016)^2) /
  sum((test_actual2016 - mean(test_actual2016))^2)

# The baseline predicts the training-sample mean for every test observation
baseline_prediction2016 <- mean(train_wage2016$y3_log_wage)

baseline_test_mse2016 <- mean(
  (test_actual2016 - baseline_prediction2016)^2
)

# Create the test results table

test_results2016 <- data.table(
  year = 2016,
  outcome = "y3_log_wage",
  model = paste0("Age spline (df = ", best_df2016, ")"),
  race_included = FALSE,
  training_rows = nrow(train_wage2016),
  test_rows = nrow(test_wage2016),
  test_MSE = test_mse2016,
  test_RMSE = test_rmse2016,
  test_MAE = test_mae2016,
  test_R2 = test_r2_2016,
  baseline_test_MSE = baseline_test_mse2016,
  mse_reduction_percent = 100 * (
    1 - test_mse2016 / baseline_test_mse2016
  )
)

print(test_results2016)

# Wage model comparison: 2019

# Release temporary objects from previous calculations

objects_to_remove <- c(
  "training_fold", "validation_fold", "sample_data", "d",
  "fit", "fit_lm", "fit_spline", "tree_fit", "smaller_tree",
  "fit_without_race", "fit_with_race",
  "predictions", "prediction",
  "prediction_without_race", "prediction_with_race", "actual",
  "x_train2016", "y_train2016",
  "ridge_cv2016", "lasso_cv2016",
  "model_lm2016", "final_wage_model2016",
  "test_predictions2016", "test_actual2016"
)

rm(
  list = intersect(objects_to_remove, ls(envir = .GlobalEnv)),
  envir = .GlobalEnv
)

gc()

# Compare linear, spline and tree models using five-fold CV

compare_models <- function(training_data, outcome, fold_id, features) {
  
  stopifnot(
    nrow(training_data) == length(fold_id),
    !anyNA(fold_id),
    all(fold_id %in% 1:5),
    length(unique(fold_id)) == 5
  )
  
  # Define the linear model and three age-spline specifications
  linear_formula <- reformulate(features, response = outcome)
  
  formulas <- list(Linear = linear_formula)
  
  for (df in 3:5) {
    formulas[[paste0("Spline_df", df)]] <- update(
      linear_formula,
      paste0(". ~ . - age + splines::ns(age, df = ", df, ")")
    )
  }
  
  # Keep tree settings consistent with the 2016 analysis
  cp_grid <- c(
    0.01, 0.003, 0.001, 0.0003, 0.0001,
    0.00005, 0.00002, 0.00001, 0.000005, 0.000002, 0
  )
  
  formula_mse <- matrix(
    NA_real_,
    nrow = 5,
    ncol = length(formulas),
    dimnames = list(NULL, names(formulas))
  )
  
  tree_mse <- matrix(
    NA_real_,
    nrow = 5,
    ncol = length(cp_grid)
  )
  
  baseline_mse <- numeric(5)
  
  # Copy only the columns needed for model fitting
  model_columns <- unique(c(outcome, features))
  
  for (k in 1:5) {
    
    train <- training_data[
      fold_id != k, model_columns, with = FALSE
    ]
    
    validation <- training_data[
      fold_id == k, model_columns, with = FALSE
    ]
    
    actual <- validation[[outcome]]
    
    baseline_mse[k] <- mean(
      (actual - mean(train[[outcome]]))^2
    )
    
    # Evaluate linear regression and age splines
    for (j in seq_along(formulas)) {
      
      fit <- lm(
        formula = formulas[[j]],
        data = train,
        na.action = na.fail
      )
      
      prediction <- predict(fit, newdata = validation)
      
      if (any(!is.finite(prediction))) {
        stop(
          "Invalid predictions for ",
          names(formulas)[j],
          " in fold ", k, "."
        )
      }
      
      formula_mse[k, j] <- mean(
        (actual - prediction)^2
      )
      
      # Release each fitted model before fitting the next one
      rm(fit, prediction)
      gc(verbose = FALSE)
    }
    
    # Grow one tree per fold
    tree <- rpart::rpart(
      formula = linear_formula,
      data = train,
      method = "anova",
      na.action = na.fail,
      control = rpart::rpart.control(
        cp = 0,
        minbucket = 200,
        maxdepth = 12,
        xval = 0,
        maxsurrogate = 0
      )
    )
    
    # Evaluate each pruning level
    for (j in seq_along(cp_grid)) {
      
      pruned_tree <- rpart::prune(
        tree,
        cp = cp_grid[j]
      )
      
      prediction <- predict(
        pruned_tree,
        newdata = validation
      )
      
      if (any(!is.finite(prediction))) {
        stop("Invalid tree predictions in fold ", k, ".")
      }
      
      tree_mse[k, j] <- mean(
        (actual - prediction)^2
      )
      
      rm(pruned_tree, prediction)
    }
    
    # Release temporary objects before the next fold
    rm(train, validation, actual, tree)
    gc(verbose = FALSE)
    
    cat("Completed fold", k, "of 5.\n")
  }
  
  # Summarize tree tuning results
  tree_results <- data.table::data.table(
    cp = cp_grid,
    cv_MSE = colMeans(tree_mse)
  )
  
  best_tree <- which.min(tree_results$cv_MSE)
  
  # Compare the completed methods
  comparison <- data.table::data.table(
    model = c(
      "Mean baseline",
      names(formulas),
      "Regression tree"
    ),
    parameter = c(
      NA_real_,
      NA_real_,
      3:5,
      tree_results$cp[best_tree]
    ),
    cv_MSE = c(
      mean(baseline_mse),
      colMeans(formula_mse),
      tree_results$cv_MSE[best_tree]
    )
  )
  
  comparison[, cv_RMSE := sqrt(cv_MSE)]
  data.table::setorder(comparison, cv_MSE)
  
  # Return small result tables without retaining fitted models
  list(
    comparison = comparison,
    tree_results = tree_results,
    formula_fold_mse = formula_mse,
    tree_fold_mse = tree_mse,
    baseline_fold_mse = baseline_mse
  )
}

# Run the comparison: log wages, 2019, without race

wage_comparison2019 <- compare_models(
  training_data = train_wage2019,
  outcome = "y3_log_wage",
  fold_id = fold_wage2019,
  features = features
)

cv_results2019 <- data.table::copy(
  wage_comparison2019$comparison
)

cv_results2019[, `:=`(
  year = 2019,
  outcome = "y3_log_wage",
  race_included = FALSE
)]

print(cv_results2019)

gc()

# Compare Ridge and Lasso with controlled prediction memory use

library(glmnet)

# Use the same linear predictors as before
predictor_formula2019 <- reformulate(features)

x_wage2019 <- Matrix::sparse.model.matrix(
  predictor_formula2019,
  data = train_wage2019
)[, -1, drop = FALSE]

y_wage2019 <- train_wage2019$y3_log_wage

stopifnot(
  nrow(x_wage2019) == length(y_wage2019),
  length(y_wage2019) == length(fold_wage2019),
  all(is.finite(y_wage2019))
)

lambda_grid2019 <- c(
  10^seq(2, -8, length.out = 100),
  0
)

regularization_rows2019 <- list()

# Evaluate one method and one fold at a time

for (method in c("Ridge", "Lasso")) {
  
  alpha_value <- if (method == "Ridge") 0 else 1
  
  fold_mse <- matrix(
    NA_real_,
    nrow = 5,
    ncol = length(lambda_grid2019)
  )
  
  for (k in 1:5) {
    
    training_rows <- which(fold_wage2019 != k)
    validation_rows <- which(fold_wage2019 == k)
    
    # Fit the entire penalty path on four training folds
    fit_penalty <- glmnet::glmnet(
      x = x_wage2019[training_rows, , drop = FALSE],
      y = y_wage2019[training_rows],
      family = "gaussian",
      alpha = alpha_value,
      lambda = lambda_grid2019,
      standardize = TRUE
    )
    
    if (!is.null(fit_penalty$jerr) && fit_penalty$jerr != 0) {
      stop("Solver warning for ", method, " in fold ", k, ".")
    }
    
    # Confirm that the requested penalty path was fitted
    if (length(fit_penalty$lambda) != length(lambda_grid2019) ||
        !isTRUE(all.equal(
          as.numeric(fit_penalty$lambda),
          lambda_grid2019,
          tolerance = 1e-8
        ))) {
      stop("Incomplete penalty path for ", method, " in fold ", k, ".")
    }
    
    squared_error_sum <- numeric(length(lambda_grid2019))
    
    # Predict at most 5,000 validation observations at a time
    batch_starts <- seq.int(
      from = 1L,
      to = length(validation_rows),
      by = 5000L
    )
    
    for (batch_start in batch_starts) {
      
      batch_end <- min(
        batch_start + 4999L,
        length(validation_rows)
      )
      
      batch_rows <- validation_rows[batch_start:batch_end]
      
      predictions <- predict(
        fit_penalty,
        newx = x_wage2019[batch_rows, , drop = FALSE],
        s = lambda_grid2019,
        type = "response"
      )
      
      if (any(!is.finite(predictions))) {
        stop("Invalid predictions for ", method, " in fold ", k, ".")
      }
      
      # Each column corresponds to one lambda value
      errors <- sweep(
        predictions,
        MARGIN = 1,
        STATS = y_wage2019[batch_rows],
        FUN = "-"
      )
      
      squared_error_sum <- squared_error_sum +
        colSums(errors^2)
      
      rm(predictions, errors)
    }
    
    fold_mse[k, ] <- squared_error_sum / length(validation_rows)
    
    rm(fit_penalty)
    gc(verbose = FALSE)
    
    cat(method, ": completed fold", k, "of 5.\n")
  }
  
  # Average validation MSE across the same five folds
  mean_mse <- colMeans(fold_mse)
  best_index <- which.min(mean_mse)
  
  regularization_rows2019[[method]] <- data.table(
    model = method,
    best_lambda = lambda_grid2019[best_index],
    cv_MSE = mean_mse[best_index],
    cv_RMSE = sqrt(mean_mse[best_index])
  )
}

# Update the model comparison

regularization_results2019 <- rbindlist(
  regularization_rows2019
)

# Remove previous entries if this block is rerun
cv_results2019 <- cv_results2019[
  !model %in% c("Ridge", "Lasso")
]

cv_results2019 <- rbindlist(
  list(
    cv_results2019,
    data.table(
      model = regularization_results2019$model,
      parameter = regularization_results2019$best_lambda,
      cv_MSE = regularization_results2019$cv_MSE,
      cv_RMSE = regularization_results2019$cv_RMSE,
      year = 2019,
      outcome = "y3_log_wage",
      race_included = FALSE
    )
  ),
  use.names = TRUE
)

setorder(cv_results2019, cv_MSE)

print(regularization_results2019)
print(cv_results2019)

# Release the predictor matrix
rm(x_wage2019, y_wage2019)
gc()

# Compare the selected 2019 spline model with and without race

if (!"race" %in% names(train_wage2019)) {
  stop("The race variable is missing.")
}

if (anyNA(train_wage2019$race)) {
  stop("The race variable contains missing values.")
}

spline_formula_no_race2019 <- update(
  reformulate(features, response = "y3_log_wage"),
  . ~ . - age + splines::ns(age, df = 5)
)

spline_formula_with_race2019 <- update(
  spline_formula_no_race2019,
  . ~ . + factor(race)
)

# Reuse the saved validation errors for the model without race
race_fold_results2019 <- data.table(
  fold = 1:5,
  mse_without_race =
    wage_comparison2019$formula_fold_mse[, "Spline_df5"],
  mse_with_race = NA_real_
)

for (k in 1:5) {
  
  fit_race <- lm(
    formula = spline_formula_with_race2019,
    data = train_wage2019[fold_wage2019 != k],
    na.action = na.fail
  )
  
  prediction_race <- predict(
    fit_race,
    newdata = train_wage2019[fold_wage2019 == k]
  )
  
  actual_race <- train_wage2019$y3_log_wage[
    fold_wage2019 == k
  ]
  
  if (any(!is.finite(prediction_race))) {
    stop("Invalid predictions in fold ", k, ".")
  }
  
  race_fold_results2019$mse_with_race[k] <- mean(
    (actual_race - prediction_race)^2
  )
  
  rm(fit_race, prediction_race, actual_race)
  gc(verbose = FALSE)
  
  cat("Completed race comparison: fold", k, "of 5.\n")
}

# Positive reductions indicate an improvement from adding race
race_fold_results2019[, mse_reduction :=
                        mse_without_race - mse_with_race]

race_comparison2019 <- data.table(
  specification = c("Without race", "With race"),
  cv_MSE = c(
    mean(race_fold_results2019$mse_without_race),
    mean(race_fold_results2019$mse_with_race)
  )
)

race_comparison2019[, cv_RMSE := sqrt(cv_MSE)]

race_improvement2019 <- data.table(
  mse_reduction = mean(race_fold_results2019$mse_reduction),
  reduction_percent = 100 * (
    1 -
      mean(race_fold_results2019$mse_with_race) /
      mean(race_fold_results2019$mse_without_race)
  )
)

print(race_comparison2019)
print(race_improvement2019)

# Evaluate the selected 2019 wage model on the held-out test set

final_wage_model2019 <- lm(
  formula = spline_formula_no_race2019,
  data = train_wage2019,
  na.action = na.fail
)

test_predictions2019 <- predict(
  final_wage_model2019,
  newdata = test_wage2019
)

test_actual2019 <- test_wage2019$y3_log_wage

if (any(!is.finite(test_predictions2019))) {
  stop("Missing or nonfinite test predictions.")
}

# Calculate unweighted prediction errors
test_errors2019 <- test_actual2019 - test_predictions2019

test_mse2019 <- mean(test_errors2019^2)

# Predict the training-sample mean as a baseline
baseline_test_mse2019 <- mean(
  (test_actual2019 - mean(train_wage2019$y3_log_wage))^2
)

test_results2019 <- data.table(
  year = 2019,
  outcome = "y3_log_wage",
  model = "Age spline (df = 5)",
  race_included = FALSE,
  training_rows = nrow(train_wage2019),
  test_rows = nrow(test_wage2019),
  test_MSE = test_mse2019,
  test_RMSE = sqrt(test_mse2019),
  test_MAE = mean(abs(test_errors2019)),
  test_R2 = 1 -
    sum(test_errors2019^2) /
    sum((test_actual2019 - mean(test_actual2019))^2),
  baseline_test_MSE = baseline_test_mse2019,
  mse_reduction_percent = 100 * (
    1 - test_mse2019 / baseline_test_mse2019
  )
)

# Combine the final test results for both training years
wage_test_results <- rbindlist(
  list(test_results2016, test_results2019),
  use.names = TRUE
)

print(wage_test_results)

# Release the fitted model and temporary predictions
# Models for cross-year forecasts will be refitted on full samples
rm(
  final_wage_model2019,
  test_predictions2019,
  test_actual2019,
  test_errors2019
)

gc()

# Cross-year wage forecasts using the selected spline models

wage_forecast_formula <- update(
  reformulate(features, response = "y3_log_wage"),
  . ~ . - age + splines::ns(age, df = 5)
)

# Fit one model at a time and predict in batches to limit memory use
forecast_wages <- function(training_data, target_data,
                           training_year, target_year) {
  
  fit <- lm(
    formula = wage_forecast_formula,
    data = training_data,
    na.action = na.fail,
    model = FALSE
  )
  
  predictions <- numeric(nrow(target_data))
  
  batch_starts <- seq.int(
    from = 1L,
    to = nrow(target_data),
    by = 20000L
  )
  
  for (batch_start in batch_starts) {
    
    batch_end <- min(
      batch_start + 19999L,
      nrow(target_data)
    )
    
    rows <- batch_start:batch_end
    
    predictions[rows] <- predict(
      fit,
      newdata = target_data[rows]
    )
  }
  
  if (any(!is.finite(predictions))) {
    stop("Missing or nonfinite cross-year predictions.")
  }
  
  weights <- target_data$PWGTP
  actual <- target_data$y3_log_wage
  
  if (any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Invalid target-year person weights.")
  }
  
  # Use target-year person weights for both means
  result <- data.table(
    training_year = training_year,
    target_year = target_year,
    target_rows = nrow(target_data),
    actual_mean_log_wage = weighted.mean(actual, weights),
    predicted_mean_log_wage = weighted.mean(predictions, weights),
    actual_minus_predicted = weighted.mean(
      actual - predictions,
      weights
    )
  )
  
  rm(fit, predictions)
  gc(verbose = FALSE)
  
  return(result)
}

# Model A: train on all eligible 2016 observations, predict 2019

wage_forecast2019 <- forecast_wages(
  training_data = wages2016,
  target_data = wages2019,
  training_year = 2016,
  target_year = 2019
)

print(wage_forecast2019)

gc()

# Model B: train on all eligible 2019 observations, predict 2022

wage_forecast2022 <- forecast_wages(
  training_data = wages2019,
  target_data = wages2022,
  training_year = 2019,
  target_year = 2022
)

print(wage_forecast2022)

gc()

# Adjust the 2022 prediction using the pre-pandemic forecast gap

# Assumption: the 2016-to-2019 mean forecast gap carries forward
prepandemic_gap <- wage_forecast2019$actual_minus_predicted

adjusted_prediction2022 <-
  wage_forecast2022$predicted_mean_log_wage + prepandemic_gap

wage_gap_results <- data.table(
  outcome = "y3_log_wage",
  prepandemic_actual_minus_predicted = prepandemic_gap,
  predicted_mean_2022 =
    wage_forecast2022$predicted_mean_log_wage,
  adjusted_predicted_mean_2022 = adjusted_prediction2022,
  actual_mean_2022 =
    wage_forecast2022$actual_mean_log_wage,
  adjusted_prediction_minus_actual =
    adjusted_prediction2022 - wage_forecast2022$actual_mean_log_wage
)

wage_crossyear_results <- rbindlist(list(
  wage_forecast2019,
  wage_forecast2022
))

print(wage_crossyear_results)
print(wage_gap_results)

# Save the submission tables once

dir.create(RESULT_DIR, recursive = TRUE, showWarnings = FALSE)
submission_tables <- list(
  sample_sizes = sample_sizes,
  descriptive_stats = descriptive_stats,
  cv_results2016 = cv_results2016,
  cv_results2019 = cv_results2019,
  race_comparison2016 = race_comparison2016,
  race_comparison2019 = race_comparison2019,
  wage_test_results = wage_test_results,
  wage_crossyear_results = wage_crossyear_results,
  wage_gap_results = wage_gap_results
)

for (table_name in names(submission_tables)) {
  fwrite(
    submission_tables[[table_name]],
    file.path(RESULT_DIR, paste0(table_name, ".csv"))
  )
}

writeLines(capture.output(sessionInfo()),
           file.path(RESULT_DIR, "sessionInfo.txt"))
message("Analysis complete. Final tables saved to: ", RESULT_DIR)
