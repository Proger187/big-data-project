# 08_clusters_people.R — "types of people" from pre-COVID features (PCA + k-means)
# and the excess gap of Y1-Y5 in every type. Run after 03_counterfactual.R. Documentation: code/README.md

library(data.table)
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
library(cluster)

SEED     <- 1
K        <- 6
SAMPLE_N <- 50000
OUT_DIR  <- "outputs/03_clusters/people"
PRED_DIR <- "data/processed/predictions"
Y_ALL    <- c("y1_employed", "y2_unemployed", "y3_log_wage", "y4_hours", "y5_poor")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(SEED)

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
  dt <- copy(readRDS(file.path("data/processed", paste0("acs_", year, ".rds"))))
  make_cluster_vars(dt)
  dt
}

# Nearest cluster centre in PC space (predict() applies the PCA standardization)
assign_clusters <- function(dt, pca, n_pc, centers) {
  out <- integer(nrow(dt))
  for (start in seq(1, nrow(dt), by = 100000)) {
    rows <- start:min(start + 99999, nrow(dt))
    pc <- predict(pca, as.matrix(dt[rows, ..CLUSTER_VARS]))[, 1:n_pc, drop = FALSE]
    d  <- sapply(1:nrow(centers), function(k) rowSums(sweep(pc, 2, centers[k, ])^2))
    out[rows] <- max.col(-d)
  }
  out
}

wmean <- function(x, w) sum(x * w) / sum(w)

sink(file.path(OUT_DIR, "run_log.txt"), split = TRUE)
tryCatch({
  cat("Clusters of people. Started:", format(Sys.time()), "\n")
  d19 <- load_year(2019)
  d22 <- load_year(2022)

  # ---- PCA on a 2019 sample, keep components up to 80% of the variance ----
  s   <- d19[sample(.N, min(.N, SAMPLE_N))]
  x   <- as.matrix(s[, ..CLUSTER_VARS])
  pca <- prcomp(x, center = TRUE, scale. = TRUE)
  var_share <- pca$sdev^2 / sum(pca$sdev^2)
  n_pc <- which(cumsum(var_share) >= 0.80)[1]
  pca_table <- data.table(pc = seq_along(var_share), variance_share = round(var_share, 4),
                          cumulative = round(cumsum(var_share), 4))
  fwrite(pca_table, file.path(OUT_DIR, "pca_variance.csv"))
  fwrite(data.table(variable = rownames(pca$rotation), round(pca$rotation[, 1:n_pc, drop = FALSE], 3)),
         file.path(OUT_DIR, "pca_loadings.csv"))
  cat("\nPCA variance explained:\n"); print(pca_table)
  cat("Keeping", n_pc, "components (>= 80% of variance)\n")
  cat("\nLoadings of the first components:\n"); print(round(pca$rotation[, 1:min(3, n_pc)], 2))
  scores <- pca$x[, 1:n_pc, drop = FALSE]

  # ---- Choosing K: elbow + silhouette ----
  sil_idx <- sample(nrow(scores), min(5000, nrow(scores)))
  choice <- rbindlist(lapply(2:10, function(k) {
    km  <- kmeans(scores, centers = k, nstart = 10, iter.max = 50)
    sil <- silhouette(km$cluster[sil_idx], dist(scores[sil_idx, ]))
    data.table(k = k, within_ss = km$tot.withinss, avg_silhouette = mean(sil[, "sil_width"]))
  }))
  fwrite(choice, file.path(OUT_DIR, "k_choice.csv"))
  cat("\nChoosing K:\n"); print(choice)
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

  png(file.path(OUT_DIR, "pca_clusters.png"), width = 900, height = 750)
  idx <- sample(nrow(scores), min(5000, nrow(scores)))
  plot(scores[idx, 1], scores[idx, 2], col = km$cluster[idx], pch = 20, cex = 0.6,
       xlab = sprintf("PC1 (%.0f%%)", 100 * var_share[1]), ylab = sprintf("PC2 (%.0f%%)", 100 * var_share[2]),
       main = sprintf("Types of people (k-means, K = %d) on the first two principal components", K))
  legend("topright", legend = paste("cluster", 1:K), col = 1:K, pch = 19)
  dev.off()

  # ---- Assign everyone, describe the clusters ----
  d19[, cluster := assign_clusters(d19, pca, n_pc, km$centers)]
  d22[, cluster := assign_clusters(d22, pca, n_pc, km$centers)]

  profile <- d19[, c(list(share_2019 = sum(PWGTP)), lapply(.SD, function(v) wmean(v, PWGTP))),
                 by = cluster, .SDcols = CLUSTER_VARS]
  profile[, share_2019 := round(share_2019 / sum(share_2019), 3)]
  size22 <- d22[, .(share_2022 = sum(PWGTP)), by = cluster][, share_2022 := round(share_2022 / sum(share_2022), 3)]
  profile <- merge(profile, size22, by = "cluster", all = TRUE)
  profile[is.na(share_2019), share_2019 := 0]; profile[is.na(share_2022), share_2022 := 0]
  setcolorder(profile, c("cluster", "share_2019", "share_2022"))
  profile <- profile[order(cluster)]
  num <- setdiff(names(profile), c("cluster", "share_2019", "share_2022"))
  profile[, (num) := lapply(.SD, round, 2), .SDcols = num]
  fwrite(profile, file.path(OUT_DIR, "cluster_profiles.csv"))
  cat("\nCluster profiles (2019 weighted means; educ_num 1 = no high school ... 5 = graduate):\n")
  print(profile)

  # ---- Excess gap by cluster, from the saved predictions ----
  gaps <- list()
  for (y in Y_ALL) {
    f19 <- file.path(PRED_DIR, sprintf("%s_2019_from_2016.rds", y))
    f22 <- file.path(PRED_DIR, sprintf("%s_2022_from_2019.rds", y))
    if (!file.exists(f19) || !file.exists(f22)) {
      cat("\nSkip", y, ": no saved predictions - run code/03_counterfactual.R first\n"); next
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
  if (length(gaps) == 0) stop("no saved predictions - run code/03_counterfactual.R first")
  gaps <- rbindlist(gaps)
  setcolorder(gaps, c("outcome", "cluster"))
  gaps <- gaps[order(outcome, cluster)]
  fwrite(gaps, file.path(OUT_DIR, "cluster_gaps.csv"))
  cat("\nExcess gap by cluster (cluster 0 = everyone):\n")
  print(gaps[, .(outcome, cluster, n_2022, actual_2022, gap_norm, gap_covid, gap_excess)])

  png(file.path(OUT_DIR, "cluster_gaps.png"), width = 1500, height = 900)
  par(mfrow = c(2, 3))
  for (y in unique(gaps$outcome)) {
    g <- gaps[outcome == y & cluster > 0]
    barplot(g$gap_excess, names.arg = paste0("c", g$cluster), main = y, ylab = "excess gap",
            col = ifelse(g$gap_excess < 0, "tomato", "steelblue"))
    abline(h = gaps[outcome == y & cluster == 0, gap_excess], lty = 2)
  }
  dev.off()

  cat("\nFinished:", format(Sys.time()), "\n")
}, error = function(e) cat("\nERROR:", conditionMessage(e), "\n"),
finally = sink())
