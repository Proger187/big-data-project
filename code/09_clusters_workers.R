# 09_clusters_workers.R — "types of workers" in 2019 vs 2022 (PCA + k-means), descriptive.
# Independent from the models. Documentation: code/README.md

library(data.table)
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
library(cluster)

SEED     <- 1
K        <- 5
SAMPLE_N <- 30000
OUT_DIR  <- "outputs/03_clusters/workers"

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(SEED)

VARS <- c("log_wage", "hours", "wfh", "selfemp", "commute", "educ_num", "age")

load_workers <- function(year) {
  dt <- readRDS(file.path("data/processed", paste0("acs_", year, ".rds")))
  dt <- dt[!is.na(y3_log_wage) & !is.na(y4_hours) & !is.na(wfh)]
  dt[, `:=`(log_wage = y3_log_wage, hours = y4_hours,
            commute  = fifelse(wfh == 1, 0, fifelse(is.na(commute_min), 0, commute_min)),
            selfemp  = fifelse(is.na(selfemp), 0L, selfemp),
            educ_num = as.integer(factor(educ, levels = c("lt_hs", "hs", "some_college", "ba", "grad"))))]
  dt[, c("year", "PWGTP", VARS), with = FALSE]
}

wmean <- function(x, w) sum(x * w) / sum(w)

sink(file.path(OUT_DIR, "run_log.txt"), split = TRUE)
tryCatch({
  cat("Clusters of workers. Started:", format(Sys.time()), "\n")
  w <- rbind(load_workers(2019), load_workers(2022))
  cat("Workers: 2019 =", w[year == 2019, .N], ", 2022 =", w[year == 2022, .N], "\n")

  # ---- PCA on a pooled 2019 + 2022 sample ----
  s   <- w[, .SD[sample(.N, min(.N, SAMPLE_N))], by = year]
  x   <- as.matrix(s[, ..VARS])
  pca <- prcomp(x, center = TRUE, scale. = TRUE)
  var_share <- pca$sdev^2 / sum(pca$sdev^2)
  fwrite(data.table(pc = seq_along(var_share), variance_share = round(var_share, 4),
                    cumulative = round(cumsum(var_share), 4)), file.path(OUT_DIR, "pca_variance.csv"))
  fwrite(data.table(variable = rownames(pca$rotation), round(pca$rotation, 3)),
         file.path(OUT_DIR, "pca_loadings.csv"))
  cat("\nPCA variance explained:\n"); print(round(var_share, 3))
  cat("\nLoadings of PC1-PC3:\n"); print(round(pca$rotation[, 1:3], 2))
  scores <- pca$x

  # ---- Choosing K ----
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
       main = sprintf("Types of workers (k-means, K = %d)", K))
  legend("topright", legend = paste("cluster", 1:K), col = 1:K, pch = 19)
  dev.off()

  # ---- Assign all workers (nearest centre in PC space) ----
  assign <- function(d) {
    out <- integer(nrow(d))
    for (start in seq(1, nrow(d), by = 100000)) {
      rows <- start:min(start + 99999, nrow(d))
      pc <- predict(pca, as.matrix(d[rows, ..VARS]))
      dist2 <- sapply(1:K, function(k) rowSums(sweep(pc, 2, km$centers[k, ])^2))
      out[rows] <- max.col(-dist2)
    }
    out
  }
  w[, cluster := assign(w)]

  # ---- Profiles and shares by year ----
  profile <- w[, lapply(.SD, function(v) round(wmean(v, PWGTP), 2)), by = cluster, .SDcols = VARS]
  profile[, real_wage_2022usd := round(exp(log_wage))]
  shares <- dcast(w[, .(n = sum(PWGTP)), by = .(year, cluster)][, share := n / sum(n), by = year],
                  cluster ~ year, value.var = "share")
  setnames(shares, c("2019", "2022"), c("share_2019", "share_2022"))
  shares[, change_pp := round(100 * (share_2022 - share_2019), 2)]
  shares[, `:=`(share_2019 = round(share_2019, 3), share_2022 = round(share_2022, 3))]
  shares[is.na(share_2019), share_2019 := 0]; shares[is.na(share_2022), share_2022 := 0]
  profile <- merge(shares, profile, by = "cluster", all = TRUE)[order(cluster)]
  fwrite(profile, file.path(OUT_DIR, "cluster_profiles.csv"))
  cat("\nTypes of workers (weighted means, 2019 + 2022; wfh/selfemp = shares; educ_num 1-5):\n")
  print(profile)

  by_year <- w[, c(list(n = .N), lapply(.SD, function(v) round(wmean(v, PWGTP), 2))),
               by = .(cluster, year), .SDcols = VARS][order(cluster, year)]
  fwrite(by_year, file.path(OUT_DIR, "cluster_profiles_by_year.csv"))
  cat("\nProfiles inside every cluster, 2019 vs 2022:\n"); print(by_year)

  png(file.path(OUT_DIR, "cluster_shares.png"), width = 900, height = 600)
  barplot(t(as.matrix(profile[, .(share_2019, share_2022)])), beside = TRUE,
          names.arg = paste("cluster", profile$cluster), col = c("grey70", "steelblue"),
          legend.text = c("2019", "2022"), ylab = "share of workers (weighted)",
          main = "Types of workers: share in 2019 vs 2022")
  dev.off()

  cat("\nFinished:", format(Sys.time()), "\n")
}, error = function(e) cat("\nERROR:", conditionMessage(e), "\n"),
finally = sink())
