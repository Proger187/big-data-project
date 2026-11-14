# =============================================================================
# 05_clusters_types.R  -  Clusters, variant 1: "types of people" for the counterfactual
#
# Idea: instead of groups we chose ourselves (education, sex, age), let the data find
# "types of people" from the same pre-COVID features X, then compute the excess gap
# of Y1-Y5 for every type.
#
# Steps (lectures 8 and 11):
#   1. Features: age, sex, education, Hispanic, foreign-born, citizen, married,
#      years in the US, STEM degree, young children - all standardized.
#   2. PCA on a 2019 sample -> keep the components that explain >= 80% of the variance.
#   3. K-means on the PC scores; elbow + silhouette for k = 2..10 (to choose K).
#   4. Everyone in 2019 and 2022 is assigned to the nearest cluster centre.
#   5. Excess gap by cluster, using the saved predictions of 03_run_models.R.
#
# Input : data/processed/acs_2019.rds, acs_2022.rds
#         data/processed/predictions/*.rds   (saved by 03_run_models.R / 03_run_all.R)
# Output: outputs/clusters_types/   (see the list at the end of this file)
# Run from the project root, AFTER code/03_run_all.R.
# =============================================================================

library(data.table)
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
library(cluster)   # silhouette

SEED     <- 1
K        <- 6        # number of clusters: look at elbow.png / silhouette, change if needed, run again
SAMPLE_N <- 50000    # 2019 sample used to fit PCA and k-means
OUT_DIR  <- "outputs/clusters_types"
PRED_DIR <- "data/processed/predictions"
Y_ALL    <- c("y1_employed", "y2_unemployed", "y3_log_wage", "y4_hours", "y5_poor")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(SEED)

# ---- 1. Features --------------------------------------------------------------

CLUSTER_VARS <- c("age", "female", "educ_num", "hispanic", "foreign_born", "citizen",
                  "married", "years_in_us", "stem_degree", "child_under6", "child_6_17")

make_cluster_vars <- function(dt) {
  dt[, educ_num := as.integer(factor(educ, levels = c("lt_hs", "hs", "some_college", "ba", "grad")))]
  dt[, married := as.integer(marital == "married")]
  dt[, child_under6 := as.integer(children %in% c("under6", "under6_and_6_17"))]
  dt[, child_6_17   := as.integer(children %in% c("age6_17", "under6_and_6_17"))]
  invisible(dt)
}

load_year <- function(year) {
  # copy(): a table read with readRDS() must be copied before adding columns by reference
  dt <- copy(readRDS(file.path("data/processed", paste0("acs_", year, ".rds"))))
  make_cluster_vars(dt)
  dt
}

# Assigns every row to the nearest cluster centre (in PC space), piece by piece
assign_clusters <- function(dt, pca, n_pc, centers) {
  out <- integer(nrow(dt))
  for (start in seq(1, nrow(dt), by = 100000)) {
    rows <- start:min(start + 99999, nrow(dt))
    x  <- as.matrix(dt[rows, ..CLUSTER_VARS])
    pc <- predict(pca, x)[, 1:n_pc, drop = FALSE]
    d  <- sapply(1:nrow(centers), function(k) rowSums(sweep(pc, 2, centers[k, ])^2))
    out[rows] <- max.col(-d)
  }
  out
}

wmean <- function(x, w) sum(x * w) / sum(w)

sink(file.path(OUT_DIR, "run_log.txt"), split = TRUE)
tryCatch({
  cat("Clusters, variant 1 (types of people). Started:", format(Sys.time()), "\n")

  d19 <- load_year(2019)
  d22 <- load_year(2022)

  # ---- 2. PCA on a 2019 sample ----------------------------------------------
  s   <- d19[sample(.N, min(.N, SAMPLE_N))]
  x   <- as.matrix(s[, ..CLUSTER_VARS])
  # prcomp standardizes itself (center + scale) and remembers the means/sds,
  # so predict(pca, new_data) later applies exactly the same standardization
  pca <- prcomp(x, center = TRUE, scale. = TRUE)
  var_share <- pca$sdev^2 / sum(pca$sdev^2)
  n_pc <- which(cumsum(var_share) >= 0.80)[1]
  pca_table <- data.table(pc = seq_along(var_share), variance_share = round(var_share, 4),
                          cumulative = round(cumsum(var_share), 4))
  fwrite(pca_table, file.path(OUT_DIR, "pca_variance.csv"))
  fwrite(data.table(variable = rownames(pca$rotation), round(pca$rotation[, 1:n_pc, drop = FALSE], 3)),
         file.path(OUT_DIR, "pca_loadings.csv"))
  cat("\nPCA: variance explained by each component:\n"); print(pca_table)
  cat("Keeping", n_pc, "components (>= 80% of variance)\n")
  cat("\nLoadings of the first components (which variables make each component):\n")
  print(round(pca$rotation[, 1:min(3, n_pc)], 2))
  scores <- pca$x[, 1:n_pc, drop = FALSE]

  # ---- 3. Choosing K: elbow + silhouette --------------------------------------
  ks <- 2:10
  sil_idx <- sample(nrow(scores), min(5000, nrow(scores)))
  choice <- rbindlist(lapply(ks, function(k) {
    km  <- kmeans(scores, centers = k, nstart = 10, iter.max = 50)
    sil <- silhouette(km$cluster[sil_idx], dist(scores[sil_idx, ]))
    data.table(k = k, within_ss = km$tot.withinss, avg_silhouette = mean(sil[, "sil_width"]))
  }))
  fwrite(choice, file.path(OUT_DIR, "k_choice.csv"))
  cat("\nChoosing K (lower within_ss = tighter clusters, higher silhouette = better separated):\n")
  print(choice)
  png(file.path(OUT_DIR, "elbow.png"), width = 1000, height = 500)
  par(mfrow = c(1, 2))
  plot(choice$k, choice$within_ss, type = "b", pch = 19, xlab = "k", ylab = "total within SS", main = "Elbow")
  abline(v = K, lty = 2)
  plot(choice$k, choice$avg_silhouette, type = "b", pch = 19, xlab = "k", ylab = "average silhouette",
       main = "Silhouette")
  abline(v = K, lty = 2)
  dev.off()

  km <- kmeans(scores, centers = K, nstart = 25, iter.max = 100)
  cat("\nUsing K =", K, "\n")

  # PCA picture: first two components, coloured by cluster
  png(file.path(OUT_DIR, "pca_clusters.png"), width = 900, height = 750)
  idx <- sample(nrow(scores), min(5000, nrow(scores)))
  plot(scores[idx, 1], scores[idx, 2], col = km$cluster[idx], pch = 20, cex = 0.6,
       xlab = sprintf("PC1 (%.0f%%)", 100 * var_share[1]), ylab = sprintf("PC2 (%.0f%%)", 100 * var_share[2]),
       main = sprintf("Types of people (k-means, K = %d) on the first two principal components", K))
  legend("topright", legend = paste("cluster", 1:K), col = 1:K, pch = 19)
  dev.off()

  # ---- 4. Assign everyone in 2019 and 2022 --------------------------------------
  d19[, cluster := assign_clusters(d19, pca, n_pc, km$centers)]
  d22[, cluster := assign_clusters(d22, pca, n_pc, km$centers)]

  # Profiles: weighted means of the features in every cluster (2019) + size in both years
  profile <- d19[, c(list(share_2019 = sum(PWGTP)), lapply(.SD, function(v) wmean(v, PWGTP))),
                 by = cluster, .SDcols = CLUSTER_VARS]
  profile[, share_2019 := round(share_2019 / sum(share_2019), 3)]
  size22 <- d22[, .(share_2022 = sum(PWGTP)), by = cluster][, share_2022 := round(share_2022 / sum(share_2022), 3)]
  profile <- merge(profile, size22, by = "cluster", all = TRUE)   # all = TRUE: keep a cluster even if it is empty in one year
  profile[is.na(share_2019), share_2019 := 0]; profile[is.na(share_2022), share_2022 := 0]
  setcolorder(profile, c("cluster", "share_2019", "share_2022"))
  profile <- profile[order(cluster)]
  num <- setdiff(names(profile), c("cluster", "share_2019", "share_2022"))
  profile[, (num) := lapply(.SD, round, 2), .SDcols = num]
  fwrite(profile, file.path(OUT_DIR, "cluster_profiles.csv"))
  cat("\nCluster profiles (2019 weighted means; educ_num: 1 = no high school ... 5 = graduate):\n")
  print(profile)

  # ---- 5. Excess gap by cluster for every outcome ---------------------------------
  gaps <- list()
  for (y in Y_ALL) {
    f19 <- file.path(PRED_DIR, sprintf("%s_2019_from_2016.rds", y))
    f22 <- file.path(PRED_DIR, sprintf("%s_2022_from_2019.rds", y))
    if (!file.exists(f19) || !file.exists(f22)) {
      cat("\nSkip", y, ": no saved predictions - run code/03_run_all.R first\n"); next
    }
    a <- merge(d19[!is.na(get(y)), .(SERIALNO, SPORDER, PWGTP, cluster, actual = get(y))],
               readRDS(f19), by = c("SERIALNO", "SPORDER"))
    b <- merge(d22[!is.na(get(y)), .(SERIALNO, SPORDER, PWGTP, cluster, actual = get(y))],
               readRDS(f22), by = c("SERIALNO", "SPORDER"))
    ga <- a[, .(n_2019 = .N, actual_2019 = wmean(actual, PWGTP), gap_norm = wmean(actual - pred, PWGTP)), by = cluster]
    gb <- b[, .(n_2022 = .N, actual_2022 = wmean(actual, PWGTP), gap_covid = wmean(actual - pred, PWGTP)), by = cluster]
    all_a <- a[, .(cluster = 0L, n_2019 = .N, actual_2019 = wmean(actual, PWGTP), gap_norm = wmean(actual - pred, PWGTP))]
    all_b <- b[, .(cluster = 0L, n_2022 = .N, actual_2022 = wmean(actual, PWGTP), gap_covid = wmean(actual - pred, PWGTP))]
    g <- merge(rbind(all_a, ga), rbind(all_b, gb), by = "cluster")
    g[, gap_excess := gap_covid - gap_norm]
    g[, outcome := y]
    gaps[[y]] <- g
  }
  if (length(gaps) == 0) stop("no saved predictions for any outcome - run code/03_run_all.R first, then this script again")
  gaps <- rbindlist(gaps)
  setcolorder(gaps, c("outcome", "cluster"))
  gaps <- gaps[order(outcome, cluster)]
  fwrite(gaps, file.path(OUT_DIR, "cluster_gaps.csv"))
  cat("\nExcess gap by cluster (cluster 0 = everyone):\n")
  print(gaps[, .(outcome, cluster, n_2022, actual_2022, gap_norm, gap_covid, gap_excess)])

  # one bar chart per outcome
  png(file.path(OUT_DIR, "cluster_gaps.png"), width = 1500, height = 900)
  outs <- unique(gaps$outcome)
  par(mfrow = c(2, 3))
  for (y in outs) {
    g <- gaps[outcome == y & cluster > 0]
    barplot(g$gap_excess, names.arg = paste0("c", g$cluster), main = y, ylab = "excess gap",
            col = ifelse(g$gap_excess < 0, "tomato", "steelblue"))
    abline(h = gaps[outcome == y & cluster == 0, gap_excess], lty = 2)
  }
  dev.off()

  cat("\nFinished:", format(Sys.time()), "\n")
}, error = function(e) cat("\nERROR:", conditionMessage(e), "\n"),
finally = sink())

# Files in outputs/clusters_types/:
#   run_log.txt            everything printed
#   pca_variance.csv       variance explained by every principal component
#   pca_loadings.csv       which variables make each component
#   k_choice.csv, elbow.png  elbow and silhouette for k = 2..10 (to choose K)
#   pca_clusters.png       people on PC1-PC2, coloured by cluster
#   cluster_profiles.csv   what every cluster looks like + its share in 2019 and 2022
#   cluster_gaps.csv       normal / covid / excess gap of Y1-Y5 by cluster (cluster 0 = all)
#   cluster_gaps.png       bar charts of the excess gap by cluster
