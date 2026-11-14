# =============================================================================
# 07_nonlinear_age.R  -  Which shape of age fits best? (lecture: polynomials, step functions, splines)
#
# In the main models age enters as  age + age^2  (a fixed choice). Here we let the data choose:
#   polynomial   poly(age, d),  d = 1..4         (d = 2 is the same as the main model)
#   step function  cut(age, k intervals), k = 4, 6, 8
#   natural spline ns(age, df), df = 3..8
# All other features are the same as in the main logit / OLS (FORMULA_FULL without the age terms).
#
# Steps for every outcome Y1-Y5:
#   1. 5-fold cross-validation (lecture 3) of every shape on a sample of the 2016 AND 2019
#      training data. Score: AUC (classification) or MSE (regression) - same rules as the main run.
#      ONE shape is chosen per outcome (best average over both years), so the benchmark
#      (2016 -> 2019) and the counterfactual (2019 -> 2022) use the same model.
#   2. The chosen shape and the main model (age + age^2) are fitted on the same training sample
#      as in 03_run_models.R and compared on the same test sample.
#   3. The chosen shape predicts 2019 and 2022 -> gaps and excess gap, compared with the main run.
#
# Input : data/processed/acs_*.rds, outputs/<y>/gaps.csv (main run, for the comparison)
# Output: outputs/nonlinear_age/   (see the list at the end of this file)
# Run from the project root. Takes about 20-40 minutes for all five outcomes.
# =============================================================================

source("code/02_model_functions.R")

if (!exists("Y_TO_RUN")) Y_TO_RUN <- c("y1_employed", "y2_unemployed", "y3_log_wage", "y4_hours", "y5_poor")
CV_N     <- 40000   # rows of each training year used for cross-validation
K_FOLDS  <- 5
OUT_DIR  <- "outputs/nonlinear_age"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Candidate shapes of age ----------------------------------------------------

# Everything except age (age:educ stays: it lets the age slope differ by education, as in the main model)
REST <- paste("female + educ + hispanic + foreign_born + citizen + marital + state + years_in_us +",
              "birth_region + stem_degree + degree_field + children + educ:female + age:educ")

step_breaks <- function(k) round(seq(17, 64, length.out = k + 1))   # fixed breaks 17..64 -> same in every year

SHAPES <- rbind(
  data.table(family = "polynomial", param = 1:4,
             term = c("age", sprintf("poly(age, %d)", 2:4))),
  data.table(family = "step", param = c(4, 6, 8),
             term = sapply(c(4, 6, 8), function(k)
               sprintf("cut(age, breaks = c(%s))", paste(step_breaks(k), collapse = ", ")))),
  data.table(family = "spline", param = 3:8, term = sprintf("ns(age, df = %d)", 3:8))
)
SHAPES[, shape := paste0(family, "_", param)]

make_formula <- function(term, y) as.formula(paste(y, "~", term, "+", REST))

fit_shape <- function(term, data, y, is_class) {
  f <- make_formula(term, y)
  if (is_class) suppressWarnings(glm(f, data = data, family = binomial)) else lm(f, data = data)
}
predict_shape <- function(m, nd) suppressWarnings(as.numeric(predict(m, newdata = nd, type = "response")))

score <- function(actual, p, is_class) if (is_class) auc(actual, p) else mean((actual - p)^2)

# ---- 2. Cross-validation of all shapes on one training year -----------------------------

cv_year <- function(year, y, is_class) {
  set.seed(SEED)
  d <- load_y(year, y)
  d <- d[sample(.N, min(.N, CV_N))]
  # rare categories -> "other"; a stricter threshold so every fold has every category
  old <- MIN_LEVEL_N; MIN_LEVEL_N <<- 200
  align_factors(d, list())
  MIN_LEVEL_N <<- old
  fold <- sample(rep(1:K_FOLDS, length.out = nrow(d)))

  res <- rbindlist(lapply(seq_len(nrow(SHAPES)), function(i) {
    s <- sapply(1:K_FOLDS, function(k) {
      m <- fit_shape(SHAPES$term[i], d[fold != k], y, is_class)
      score(d[fold == k][[y]], predict_shape(m, d[fold == k]), is_class)
    })
    data.table(year = year, shape = SHAPES$shape[i], cv_score = mean(s), cv_se = sd(s) / sqrt(K_FOLDS))
  }))
  res
}

# ---- 3. One step (train year -> target year) with the chosen shape -------------------------

run_step <- function(train_year, target_year, y, is_class, term) {
  set.seed(SEED)                                 # same seed -> same train / test samples as 03_run_models.R
  parts  <- split_train_test(load_y(train_year, y))
  target <- load_y(target_year, y)
  align_factors(parts$train, list(parts$test, target))

  m_new  <- fit_shape(term, parts$train, y, is_class)
  m_main <- fit_shape("age + I(age^2)", parts$train, y, is_class)   # = main logit / OLS
  test_cmp <- data.table(train_year = train_year,
                         model = c("main: age + age^2", paste("chosen:", term)),
                         test_score = c(score(parts$test[[y]], predict_shape(m_main, parts$test), is_class),
                                        score(parts$test[[y]], predict_shape(m_new,  parts$test), is_class)))

  target[, pred := predict_in_chunks(function(nd) predict_shape(m_new, nd), target)]
  list(test = test_cmp, gaps = gap_table(target, y))
}

# ---- 4. Main loop ---------------------------------------------------------------------------

sink(file.path(OUT_DIR, "run_log.txt"), split = TRUE)
summary_rows <- list()
for (y in Y_TO_RUN) {
  tryCatch({
    is_class <- y %in% CLASSIFICATION
    metric   <- if (is_class) "AUC (higher = better)" else "MSE (lower = better)"
    cat("\n================================================================\n")
    cat(y, "- cross-validation of age shapes,", metric, "  ", format(Sys.time()), "\n")
    cat("================================================================\n")

    cv <- rbind(cv_year(2016, y, is_class), cv_year(2019, y, is_class))
    cv_avg <- cv[, .(cv_score = mean(cv_score)), by = shape]
    cv_avg <- merge(SHAPES[, .(shape, family, param, term)], cv_avg, by = "shape")
    cv_avg <- if (is_class) cv_avg[order(-cv_score)] else cv_avg[order(cv_score)]
    best   <- cv_avg[1]
    fwrite(merge(cv, SHAPES[, .(shape, family, param)], by = "shape"), file.path(OUT_DIR, sprintf("cv_%s.csv", y)))
    cat("\nCV score by shape (average of 2016 and 2019, best first):\n")
    print(cv_avg[, .(shape, cv_score = round(cv_score, 5))])
    cat("\nChosen shape:", best$term, "\n")

    # picture: CV score against the parameter, one panel per family
    png(file.path(OUT_DIR, sprintf("cv_%s.png", y)), width = 1500, height = 500)
    par(mfrow = c(1, 3))
    for (fam in c("polynomial", "step", "spline")) {
      g <- cv_avg[family == fam][order(param)]
      xlab <- c(polynomial = "degree d", step = "number of intervals k", spline = "degrees of freedom df")[fam]
      plot(g$param, g$cv_score, type = "b", pch = 19, xlab = xlab, ylab = paste("CV", metric),
           main = paste(y, "-", fam), ylim = range(cv_avg$cv_score))
    }
    dev.off()

    a <- run_step(2016, 2019, y, is_class, best$term)
    b <- run_step(2019, 2022, y, is_class, best$term)
    test_cmp <- rbind(a$test, b$test)
    fwrite(test_cmp, file.path(OUT_DIR, sprintf("test_%s.csv", y)))
    cat("\nTest-set comparison with the main model (same samples as 03_run_models.R):\n"); print(test_cmp)

    ga <- a$gaps[, .(group_var, group, gap_norm = gap)]
    gb <- b$gaps[, .(group_var, group, gap_covid = gap)]
    gaps <- merge(ga, gb, by = c("group_var", "group"), sort = FALSE)
    gaps[, gap_excess := gap_covid - gap_norm]
    main_file <- file.path("outputs", y, "gaps.csv")
    if (file.exists(main_file)) {
      main <- fread(main_file)[, .(group_var, group, gap_excess_main = gap_excess)]
      gaps <- merge(gaps, main, by = c("group_var", "group"), all.x = TRUE, sort = FALSE)
    }
    fwrite(gaps, file.path(OUT_DIR, sprintf("gaps_%s.csv", y)))
    cat("\nExcess gap with the chosen shape (gap_excess) vs the main run (gap_excess_main):\n"); print(gaps)

    summary_rows[[y]] <- data.table(outcome = y, chosen_shape = best$term,
                                    test_main_2019 = test_cmp[train_year == 2019][1, test_score],
                                    test_chosen_2019 = test_cmp[train_year == 2019][2, test_score],
                                    excess_chosen = gaps[group_var == "all", gap_excess],
                                    excess_main = if ("gap_excess_main" %in% names(gaps)) gaps[group_var == "all", gap_excess_main] else NA)
  }, error = function(e) cat("\nERROR for", y, ":", conditionMessage(e), "\n"))
}
if (length(summary_rows)) {
  summ <- rbindlist(summary_rows)
  fwrite(summ, file.path(OUT_DIR, "summary.csv"))
  cat("\n================================================================\nSUMMARY\n"); print(summ)
}
cat("\nFinished:", format(Sys.time()), "\n")
sink()

# Files in outputs/nonlinear_age/:
#   run_log.txt          everything printed
#   cv_<y>.csv           CV score of every shape, separately for 2016 and 2019 (+ standard error)
#   cv_<y>.png           CV score against degree / intervals / df  (the picture for the report)
#   test_<y>.csv         test score: main model (age + age^2) vs chosen shape
#   gaps_<y>.csv         normal / covid / excess gap with the chosen shape, next to the main run
#   summary.csv          one line per outcome: chosen shape, test scores, excess gap new vs main
