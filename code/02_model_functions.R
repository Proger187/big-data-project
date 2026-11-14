# 02_model_functions.R — settings and model functions used by 03-05 (sourced, runs nothing).
# Documentation: code/README.md

for (p in c("data.table", "glmnet")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(data.table)
library(glmnet)
library(MASS)
library(class)
library(rpart)
library(splines)

# ---- Settings ------------------------------------------------------------------------

SEED        <- 1
TRAIN_N     <- 100000
TEST_N      <- 50000
KNN_TRAIN_N <- 30000
KNN_TEST_N  <- 10000
MIN_LEVEL_N <- 50          # rarer categories are merged into "other"
SAVE_PREDICTIONS <- TRUE   # person-level predictions for the cluster analysis

PRED_DIR <- "data/processed/predictions"

FACTORS        <- c("educ", "race", "marital", "state", "birth_region", "degree_field", "children")
CLASSIFICATION <- c("y1_employed", "y2_unemployed", "y5_poor")

# Race is not used (coding changed in 2020, decision D15); see 04_robustness_race_children.R
FORMULA_FULL <- ~ age + I(age^2) + female + educ + hispanic + foreign_born + citizen +
  marital + state + years_in_us + birth_region + stem_degree + degree_field + children +
  educ:female + age:educ

FORMULA_SPLINE <- ~ ns(age, df = 5) + female + educ + hispanic + foreign_born + citizen +
  marital + state + years_in_us + birth_region + stem_degree + degree_field + children +
  educ:female

FORMULA_TREE <- ~ age + female + educ + hispanic + foreign_born + citizen + marital +
  state + years_in_us + birth_region + stem_degree + degree_field + children

FORMULA_SMALL <- ~ age + female + educ + hispanic + foreign_born + citizen + marital +
  years_in_us + birth_region + stem_degree + children

with_y <- function(formula, y) update(formula, as.formula(paste(y, "~ .")))

# ---- Data ----------------------------------------------------------------------------

load_y <- function(year, y) {
  dt <- readRDS(file.path("data/processed", paste0("acs_", year, ".rds")))
  dt <- dt[!is.na(get(y))]
  dt[, (FACTORS) := lapply(.SD, as.character), .SDcols = FACTORS]
  dt
}

split_train_test <- function(dt) {
  n_test <- min(TEST_N, round(0.3 * nrow(dt)))
  idx    <- sample(nrow(dt))
  test   <- dt[idx[1:n_test]]
  rest   <- idx[(n_test + 1):length(idx)]
  train  <- dt[rest[1:min(TRAIN_N, length(rest))]]
  list(train = train, test = test)
}

# Same factor levels in all tables; levels rare in the training sample -> "other"
align_factors <- function(train, others) {
  for (f in FACTORS) {
    counts <- table(train[[f]])
    keep   <- names(counts)[counts >= MIN_LEVEL_N]
    keep   <- keep[order(-counts[keep])]
    lev    <- unique(c(keep, "other"))
    fix    <- function(x) factor(ifelse(x %in% keep, x, "other"), levels = lev)
    train[, (f) := fix(get(f))]
    for (d in others) d[, (f) := fix(get(f))]
  }
  invisible(NULL)
}

# ---- Helpers -------------------------------------------------------------------------

wmean <- function(x, w) sum(x * w) / sum(w)

auc <- function(y, p) {
  r  <- rank(p)
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

predict_in_chunks <- function(predict_fun, data, chunk = 50000) {
  out <- numeric(nrow(data))
  for (start in seq(1, nrow(data), by = chunk)) {
    rows <- start:min(start + chunk - 1, nrow(data))
    out[rows] <- predict_fun(data[rows])
  }
  out
}

# ---- Models: each returns list(name, tuning, predict(newdata)) --------------------------

fit_ols_or_logit <- function(train, y, is_class) {
  f <- with_y(FORMULA_FULL, y)
  m <- if (is_class) glm(f, data = train, family = binomial) else lm(f, data = train)
  list(name = if (is_class) "logit" else "ols", tuning = "-",
       predict = function(nd) suppressWarnings(as.numeric(predict(m, newdata = nd, type = "response"))))
}

# alpha = 1 lasso, alpha = 0 ridge; lambda by 5-fold CV, lambda.1se is used
fit_glmnet <- function(train, y, is_class, alpha) {
  x  <- sparse.model.matrix(FORMULA_FULL, train)[, -1]
  cv <- cv.glmnet(x, train[[y]], alpha = alpha, nfolds = 5,
                  family = if (is_class) "binomial" else "gaussian")
  b  <- coef(cv, s = "lambda.1se")
  selected <- data.table(feature = rownames(b)[-1], coef = as.numeric(b[-1]))[coef != 0]
  list(name = if (alpha == 1) "lasso" else "ridge",
       tuning = sprintf("lambda.1se = %.5f (lambda.min = %.5f)", cv$lambda.1se, cv$lambda.min),
       selected = selected, n_candidates = nrow(b) - 1,
       predict = function(nd) as.numeric(predict(cv, newx = sparse.model.matrix(FORMULA_FULL, nd)[, -1],
                                                 s = "lambda.1se", type = "response")))
}

fit_spline <- function(train, y) {
  m <- lm(with_y(FORMULA_SPLINE, y), data = train)
  list(name = "spline_ols", tuning = "ns(age, df = 5)",
       predict = function(nd) suppressWarnings(as.numeric(predict(m, newdata = nd))))
}

# Columns constant inside one class are dropped (LDA cannot use them)
fit_lda <- function(train, y) {
  make_x <- function(d) model.matrix(FORMULA_SMALL, d)[, -1]
  x <- make_x(train)
  g <- train[[y]]
  varies <- apply(x, 2, function(col) sd(col[g == 0]) > 0 && sd(col[g == 1]) > 0)
  keep <- colnames(x)[varies]
  m <- lda(x[, keep], grouping = g)
  list(name = "lda", tuning = sprintf("small feature set, %d columns", length(keep)),
       predict = function(nd) as.numeric(predict(m, newdata = make_x(nd)[, keep, drop = FALSE])$posterior[, "1"]))
}

# Large tree pruned at the cp with the lowest 5-fold CV error
fit_tree <- function(train, y, is_class) {
  d <- copy(train)
  if (is_class) d[, (y) := factor(get(y))]
  m <- rpart(with_y(FORMULA_TREE, y), data = d, method = if (is_class) "class" else "anova",
             control = rpart.control(cp = 0.0005, xval = 5))
  best_cp <- m$cptable[which.min(m$cptable[, "xerror"]), "CP"]
  m <- prune(m, cp = best_cp)
  list(name = "tree", tuning = sprintf("cp = %.5f, %d leaves", best_cp, sum(m$frame$var == "<leaf>")),
       model = m,
       predict = function(nd) {
         if (is_class) as.numeric(predict(m, newdata = nd, type = "prob")[, "1"])
         else as.numeric(predict(m, newdata = nd))
       })
}

# k chosen on a validation part; comparison only (too slow for millions of predictions)
fit_knn <- function(train, y) {
  make_x <- function(d) model.matrix(FORMULA_SMALL, d)[, -1]
  tr <- train[sample(.N, min(.N, KNN_TRAIN_N))]
  x  <- make_x(tr)
  mu <- colMeans(x); sdv <- apply(x, 2, sd); sdv[sdv == 0] <- 1
  scale_x <- function(m) sweep(sweep(m, 2, mu), 2, sdv, "/")
  x  <- scale_x(x)
  val <- sample(nrow(x), round(0.2 * nrow(x)))
  errs <- sapply(c(10, 50, 200), function(k) {
    pred <- knn(x[-val, ], x[val, ], factor(tr[[y]][-val]), k = k)
    mean(pred != tr[[y]][val])
  })
  k <- c(10, 50, 200)[which.min(errs)]
  list(name = "knn", tuning = sprintf("k = %d (validation error %s)", k,
                                      paste(round(errs, 4), collapse = " / ")),
       only_comparison = TRUE,
       predict = function(nd) {
         pred <- knn(x, scale_x(make_x(nd)), factor(tr[[y]]), k = k, prob = TRUE)
         p <- attr(pred, "prob")
         ifelse(pred == "1", p, 1 - p)
       })
}

fit_all_models <- function(train, y) {
  is_class <- y %in% CLASSIFICATION
  tries <- if (is_class) {
    list(function() fit_ols_or_logit(train, y, TRUE),
         function() fit_glmnet(train, y, TRUE, alpha = 1),
         function() fit_glmnet(train, y, TRUE, alpha = 0),
         function() fit_lda(train, y),
         function() fit_knn(train, y),
         function() fit_tree(train, y, TRUE))
  } else {
    list(function() fit_ols_or_logit(train, y, FALSE),
         function() fit_glmnet(train, y, FALSE, alpha = 1),
         function() fit_glmnet(train, y, FALSE, alpha = 0),
         function() fit_spline(train, y),
         function() fit_tree(train, y, FALSE))
  }
  models <- list()
  for (f in tries) {
    m <- tryCatch(f(), error = function(e) { cat("  model failed:", conditionMessage(e), "\n"); NULL })
    if (!is.null(m)) { models[[m$name]] <- m; cat("  fitted:", m$name, "(", m$tuning, ")\n") }
  }
  models
}

# ---- Evaluation -----------------------------------------------------------------------

evaluate_models <- function(models, test, y) {
  is_class <- y %in% CLASSIFICATION
  rows <- list()
  for (m in models) {
    d <- if (isTRUE(m$only_comparison)) test[sample(.N, min(.N, KNN_TEST_N))] else test
    p <- m$predict(d)
    actual <- d[[y]]
    row <- data.table(model = m$name, tuning = m$tuning, n_test = nrow(d),
                      mean_actual = mean(actual), mean_predicted = mean(p))
    if (is_class) {
      row[, `:=`(test_error = mean((p > 0.5) != actual), auc = auc(actual, p))]
    } else {
      row[, `:=`(test_mse = mean((actual - p)^2),
                 r2 = 1 - mean((actual - p)^2) / mean((actual - mean(actual))^2))]
    }
    row[, used_for_counterfactual := !isTRUE(m$only_comparison)]
    rows[[m$name]] <- row
  }
  res <- rbindlist(rows, fill = TRUE)
  if (is_class) res[order(-auc)] else res[order(test_mse)]
}

# Best = highest AUC (classification) / lowest test MSE (regression); KNN excluded
pick_best <- function(results) results[used_for_counterfactual == TRUE][1, model]

# ---- Gaps: actual vs predicted, overall and by group ------------------------------------

GROUP_VARS <- c("all", "age_group", "educ", "sex", "hispanic_origin", "region")

add_groups <- function(dt) {
  dt[, age_group := cut(age, c(17, 24, 34, 44, 54, 64),
                        labels = c("18-24", "25-34", "35-44", "45-54", "55-64"))]
  dt[, sex := ifelse(female == 1, "female", "male")]
  dt[, hispanic_origin := ifelse(hispanic == 1, "hispanic", "not_hispanic")]
  dt[, all := "all"]
  invisible(dt)
}

gap_table <- function(target, y) {
  add_groups(target)
  out <- list()
  for (g in GROUP_VARS) {
    out[[g]] <- target[, .(n = .N,
                           actual    = wmean(get(y), PWGTP),
                           predicted = wmean(pred, PWGTP)), by = .(group = as.character(get(g)))]
    out[[g]][, group_var := g]
  }
  res <- rbindlist(out)[, gap := actual - predicted]
  setcolorder(res, c("group_var", "group", "n", "actual", "predicted", "gap"))
  res
}

# ---- One step: train on train_year, predict target_year ---------------------------------

run_pair <- function(train_year, target_year, y, out_dir) {
  cat("\n================================================================\n")
  cat(sprintf("%s: train on %d, predict %d   (%s)\n", y, train_year, target_year, format(Sys.time())))
  cat("================================================================\n")
  set.seed(SEED)

  data_train_year <- load_y(train_year, y)
  target          <- load_y(target_year, y)
  parts <- split_train_test(data_train_year)
  rm(data_train_year); gc()
  align_factors(parts$train, list(parts$test, target))
  cat(sprintf("Rows: train sample %d, test sample %d, target year %d\n",
              nrow(parts$train), nrow(parts$test), nrow(target)))

  models  <- fit_all_models(parts$train, y)
  results <- evaluate_models(models, parts$test, y)
  best    <- pick_best(results)
  fwrite(results, file.path(out_dir, sprintf("models_%d.csv", train_year)))
  cat("\nModel comparison on the test set (", train_year, "):\n", sep = ""); print(results)
  cat("\nBest model:", best, "\n")

  if (!is.null(models$lasso)) {
    sel <- models$lasso$selected[order(-abs(coef))]
    fwrite(sel, file.path(out_dir, sprintf("lasso_selected_%d.csv", train_year)))
    cat(sprintf("\nLasso kept %d of %d candidate features (lambda.1se). Top 15 by |coef|:\n",
                nrow(sel), models$lasso$n_candidates))
    print(head(sel, 15))
  }

  target[, pred := predict_in_chunks(models[[best]]$predict, target)]
  if (SAVE_PREDICTIONS) {
    dir.create(PRED_DIR, showWarnings = FALSE, recursive = TRUE)
    saveRDS(target[, .(SERIALNO, SPORDER, pred)],
            file.path(PRED_DIR, sprintf("%s_%d_from_%d.rds", y, target_year, train_year)))
  }
  gaps <- gap_table(target, y)
  fwrite(gaps, file.path(out_dir, sprintf("gaps_%d_to_%d.csv", train_year, target_year)))
  cat(sprintf("\nActual vs predicted in %d (model trained on %d):\n", target_year, train_year))
  print(gaps)

  # Regression tree on residuals: where the prediction misses most
  s <- target[sample(.N, min(.N, 200000))]
  s[, resid := get(y) - pred]
  rt <- rpart(update(FORMULA_TREE, resid ~ .), data = s, method = "anova",
              control = rpart.control(maxdepth = 3, cp = 0.001))
  if (nrow(rt$frame) > 1) {
    png(file.path(out_dir, sprintf("tree_residuals_%d.png", target_year)), width = 1400, height = 900)
    plot(rt, uniform = TRUE, margin = 0.05,
         main = sprintf("%s: where actual %d differs from prediction (residual = actual - predicted)",
                        y, target_year))
    text(rt, use.n = TRUE, cex = 0.8)
    dev.off()
  } else {
    cat("\nResidual tree has no splits: the gap is about the same for all groups.\n")
  }
  cat("\nResidual tree (", target_year, "):\n", sep = ""); suppressWarnings(print(rt))

  gaps
}

# Both steps for one outcome -> one table with gap_norm, gap_covid, gap_excess
run_outcome <- function(y, out_dir) {
  gaps_a <- run_pair(2016, 2019, y, out_dir)
  gaps_b <- run_pair(2019, 2022, y, out_dir)
  setnames(gaps_a, c("n", "actual", "predicted", "gap"),
           c("n_2019", "actual_2019", "pred_2019_from_2016", "gap_norm"))
  setnames(gaps_b, c("n", "actual", "predicted", "gap"),
           c("n_2022", "actual_2022", "pred_2022_from_2019", "gap_covid"))
  gaps <- merge(gaps_a, gaps_b, by = c("group_var", "group"), all = TRUE, sort = FALSE)
  gaps[, gap_excess := gap_covid - gap_norm]
  fwrite(gaps, file.path(out_dir, "gaps.csv"))
  gaps
}
