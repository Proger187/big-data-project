# 01_prepare_data.R — raw ACS PUMS person files (2016, 2019, 2022) -> data/processed/acs_<year>.rds
# Documentation: code/README.md

if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)

# ---- Settings ------------------------------------------------------------------------

# Raw person files (a + b) of each year
RAW_FILES <- list(
  "2016" = c("data/ss16pusa.csv",   "data/ss16pusb.csv"),
  "2019" = c("data/psam_pusa_.csv", "data/psam_pusb_.csv"),
  "2022" = c("data/psam_pusa.csv",  "data/psam_pusb.csv")
)

# ADJINC is constant within a year -> checks that files are not mixed up
ADJINC_OF_YEAR <- c("2016" = 1007588, "2019" = 1010145, "2022" = 1042311)

OUT_DATA <- "data/processed"
OUT_TAB  <- "outputs/00_data_checks"

# CPI-U annual averages (BLS), money in 2022 dollars
CPI      <- c("2016" = 240.007, "2019" = 255.657, "2022" = 292.655)
CPI_BASE <- CPI[["2022"]]

COLS_MAIN  <- c("SERIALNO", "SPORDER", "ST", "PWGTP", "ADJINC",
                "AGEP", "SEX", "SCHL", "RAC1P", "HISP", "NATIVITY", "CIT", "MAR",
                "ESR", "WAGP", "WKHP", "POVPIP",
                "YOEP", "WAOB", "SCIENGP", "FOD1P", "PAOC")
COLS_EXTRA <- c("JWTRNS", "JWTR", "COW", "JWMNP")   # JWTR = 2016 name of JWTRNS

KEEP <- c("year", "SERIALNO", "SPORDER", "PWGTP",
          "age", "female", "educ", "race", "hispanic", "foreign_born", "citizen",
          "marital", "state", "region",
          "years_in_us", "birth_region", "stem_degree", "degree_field", "children",
          "y1_employed", "y2_unemployed", "y3_log_wage", "y4_hours", "y5_poor",
          "wage_real", "wfh", "selfemp", "commute_min")

dir.create(OUT_DATA, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_TAB,  showWarnings = FALSE, recursive = TRUE)


# ---- Loading -------------------------------------------------------------------------

find_person_files <- function(year) {
  files <- RAW_FILES[[as.character(year)]]
  files[file.exists(files)]
}

read_person_file <- function(path) {
  header <- names(fread(path, nrows = 0))
  keep   <- header[toupper(header) %in% c(COLS_MAIN, COLS_EXTRA)]
  serial <- keep[toupper(keep) == "SERIALNO"]
  dt <- fread(path, select = keep, colClasses = list(character = serial), showProgress = FALSE)
  setnames(dt, toupper(names(dt)))
  dt
}

load_year <- function(year) {
  files <- find_person_files(year)
  if (length(files) != 2) {
    stop("Year ", year, ": expected 2 person files (a and b), found ", length(files))
  }
  dt <- rbindlist(lapply(files, read_person_file), use.names = TRUE, fill = TRUE)

  adj <- unique(dt$ADJINC)
  if (length(adj) != 1 || adj != ADJINC_OF_YEAR[[as.character(year)]]) {
    stop("Year ", year, ": ADJINC = ", paste(adj, collapse = ", "), " does not match this year. ",
         "Files are probably mixed up: check RAW_FILES.")
  }

  missing <- setdiff(COLS_MAIN, names(dt))
  if (length(missing) > 0) {
    stop("Year ", year, ": missing columns: ", paste(missing, collapse = ", "))
  }
  for (col in c("COW", "JWMNP")) {
    if (!col %in% names(dt)) dt[, (col) := NA_integer_]
  }
  dt[, year := year]
  dt
}


# ---- Features X ------------------------------------------------------------------------

make_x <- function(dt) {
  dt[, age    := AGEP]
  dt[, female := as.integer(SEX == 2)]

  dt[, educ := fcase(SCHL <= 15, "lt_hs",
                     SCHL <= 17, "hs",
                     SCHL <= 20, "some_college",
                     SCHL == 21, "ba",
                     SCHL >= 22, "grad")]
  dt[, educ := factor(educ, levels = c("lt_hs", "hs", "some_college", "ba", "grad"))]

  dt[, race := fcase(RAC1P == 1, "white",
                     RAC1P == 2, "black",
                     RAC1P == 6, "asian",
                     default = "other")]
  dt[, race := factor(race, levels = c("white", "black", "asian", "other"))]

  dt[, hispanic     := as.integer(HISP != 1)]
  dt[, foreign_born := as.integer(NATIVITY == 2)]
  dt[, citizen      := as.integer(CIT != 5)]
  dt[, marital := factor(MAR, levels = 1:5,
                         labels = c("married", "widowed", "divorced", "separated", "never_married"))]
  dt[, state  := factor(ST)]

  # 0 years for people born in the US
  dt[, years_in_us := fifelse(!is.na(YOEP), as.integer(pmax(year - YOEP, 0)), 0L)]

  dt[, birth_region := factor(WAOB, levels = 1:8,
                              labels = c("us", "pr_island_areas", "latin_america", "asia",
                                         "europe", "africa", "northern_america", "oceania"))]

  dt[, stem_degree := as.integer(!is.na(SCIENGP) & SCIENGP == 1)]

  dt[, degree_field := factor(fifelse(is.na(FOD1P), "no_ba", as.character(FOD1P)))]

  # PAOC is asked only of women
  dt[, children := fcase(SEX == 1,     "male",
                         is.na(PAOC),  "female_na",
                         PAOC == 1,    "under6",
                         PAOC == 2,    "age6_17",
                         PAOC == 3,    "under6_and_6_17",
                         PAOC == 4,    "none")]
  dt[, children := factor(children, levels = c("none", "under6", "age6_17", "under6_and_6_17",
                                               "female_na", "male"))]
  # Census region from the state code (no REGION column in 2016)
  dt[, region := fcase(ST %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50),                  "northeast",
                       ST %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55),       "midwest",
                       ST %in% c(1, 5, 10, 11, 12, 13, 21, 22, 24, 28, 37, 40, 45, 47,
                                 48, 51, 54),                                            "south",
                       ST %in% c(2, 4, 6, 8, 15, 16, 30, 32, 35, 41, 49, 53, 56),       "west")]
  dt[, region := factor(region, levels = c("northeast", "midwest", "south", "west"))]
  invisible(dt)
}


# ---- Outcomes Y1-Y5 (NA = not in the population of this outcome) -------------------

make_y <- function(dt, year) {
  cpi_factor <- CPI_BASE / CPI[[as.character(year)]]

  dt[, is_employed := ESR %in% c(1, 2, 4, 5)]
  dt[, in_lf       := ESR %in% c(1, 2, 3, 4, 5)]
  dt[, age_25_64   := AGEP >= 25 & AGEP <= 64]
  dt[, age_18_64   := AGEP >= 18 & AGEP <= 64]

  # as.numeric: WAGP * ADJINC overflows R integers
  dt[, wage_real := fifelse(WAGP > 0, as.numeric(WAGP) * (ADJINC / 1e6) * cpi_factor, NA_real_)]

  dt[, y1_employed   := fifelse(age_25_64, as.integer(is_employed), NA_integer_)]
  dt[, y2_unemployed := fifelse(age_25_64 & in_lf, as.integer(ESR == 3), NA_integer_)]
  dt[, y3_log_wage   := fifelse(age_25_64 & is_employed & !is.na(wage_real), log(wage_real), NA_real_)]
  dt[, y4_hours      := fifelse(age_25_64 & is_employed & !is.na(WKHP), as.numeric(WKHP), NA_real_)]
  dt[, y5_poor       := fifelse(age_18_64 & !is.na(POVPIP), as.integer(POVPIP < 100), NA_integer_)]
  invisible(dt)
}


# ---- Variables for the worker clusters ----------------------------------------------

make_extra <- function(dt) {
  # code 11 = worked from home
  if ("JWTRNS" %in% names(dt)) {
    dt[, wfh := fifelse(!is.na(JWTRNS), as.integer(JWTRNS == 11), NA_integer_)]
  } else if ("JWTR" %in% names(dt)) {
    dt[, wfh := fifelse(!is.na(JWTR), as.integer(JWTR == 11), NA_integer_)]
  } else {
    dt[, wfh := NA_integer_]
  }
  dt[, selfemp     := fifelse(!is.na(COW), as.integer(COW %in% c(6, 7)), NA_integer_)]
  dt[, commute_min := as.numeric(JWMNP)]
  invisible(dt)
}


# ---- Checks ---------------------------------------------------------------------------

wmean <- function(y, w) {
  ok <- !is.na(y)
  sum(y[ok] * w[ok]) / sum(w[ok])
}

wmedian <- function(y, w) {
  ok <- !is.na(y)
  y <- y[ok]; w <- w[ok]
  o <- order(y)
  y[o][which(cumsum(w[o]) >= sum(w) / 2)[1]]
}

check_year <- function(dt, rows_raw, year) {
  data.table(
    year        = year,
    rows_raw    = rows_raw,
    rows_18_64  = nrow(dt),
    n_states    = uniqueN(dt$state),
    dup_keys    = sum(duplicated(dt, by = c("SERIALNO", "SPORDER"))),
    n_y1        = sum(!is.na(dt$y1_employed)),
    n_y2        = sum(!is.na(dt$y2_unemployed)),
    n_y3        = sum(!is.na(dt$y3_log_wage)),
    n_y4        = sum(!is.na(dt$y4_hours)),
    n_y5        = sum(!is.na(dt$y5_poor)),
    na_educ     = sum(is.na(dt$educ)),
    na_region   = sum(is.na(dt$region)),
    na_birth_region = sum(is.na(dt$birth_region)),
    na_children = sum(is.na(dt$children)),
    n_fields    = uniqueN(dt$degree_field),
    share_stem  = round(mean(dt$stem_degree), 3),
    na_marital  = sum(is.na(dt$marital))
  )
}

means_year <- function(dt, year) {
  w <- dt$PWGTP
  data.table(
    year                 = year,
    y1_employment_rate   = round(wmean(dt$y1_employed,   w), 3),
    y2_unemployment_rate = round(wmean(dt$y2_unemployed, w), 3),
    y3_median_real_wage  = round(wmedian(exp(dt$y3_log_wage), w)),
    y4_mean_hours        = round(wmean(dt$y4_hours, w), 1),
    y5_poverty_rate      = round(wmean(dt$y5_poor, w), 3)
  )
}

# Values computed on 10.11 (docs/PROJECT.md) to compare with
EXPECTED <- data.table(
  year                 = c(2019, 2022),
  y1_employment_rate   = c(0.757, 0.765),
  y2_unemployment_rate = c(0.037, 0.035),
  y3_median_real_wage  = c(52035, 52116),
  y4_mean_hours        = c(40.8, 40.8),
  y5_poverty_rate      = c(0.115, 0.117)
)


# ---- Main loop ------------------------------------------------------------------------

checks <- list()
means  <- list()

for (year in as.integer(names(RAW_FILES))) {
  if (length(find_person_files(year)) == 0) {
    message("Skip ", year, ": files not found: ", paste(RAW_FILES[[as.character(year)]], collapse = ", "))
    next
  }
  message("Year ", year, ": loading ...")
  dt <- load_year(year)
  rows_raw <- nrow(dt)

  make_x(dt)
  make_y(dt, year)
  make_extra(dt)

  dt <- dt[AGEP >= 18 & AGEP <= 64, ..KEEP]

  out_file <- file.path(OUT_DATA, paste0("acs_", year, ".rds"))
  saveRDS(dt, out_file)
  message("Year ", year, ": saved ", nrow(dt), " rows to ", out_file)

  checks[[as.character(year)]] <- check_year(dt, rows_raw, year)
  means[[as.character(year)]]  <- means_year(dt, year)
  rm(dt); gc()
}

checks <- rbindlist(checks)
means  <- rbindlist(means)
fwrite(checks, file.path(OUT_TAB, "prep_checks.csv"))
fwrite(means,  file.path(OUT_TAB, "eda_means_by_year.csv"))

cat("\n==== Checks ====\n");                         print(checks)
cat("\n==== Weighted means (our result) ====\n");     print(means)
cat("\n==== Expected ====\n");                       print(EXPECTED)
