#!/usr/bin/env Rscript
# This script takes two command line arguments.
#
# The first determines if the cross validation is on "mass" or "area" normalized
# traits. It is required.
#
# The second specifies the number of cross validations to do. If omitted, the
# default is 20.
library(shiklomanov2017np)
library(tidyverse)

argv <- commandArgs(trailingOnly = TRUE)
stopifnot(length(argv) %in% c(1, 2))
mass_area <- match.arg(argv[1], c("mass", "area"))
ncv <- argv[2]
if (is.na(ncv)) ncv <- 20

outdir <- here::here("output")
resultsdir <- here::here("results")
dir.create(resultsdir, showWarnings = FALSE, recursive = TRUE)

dat_df_all <- try_data(mass_area)

data_groups <- dat_df_all[["pft"]]
data_mat <- dat_df_all %>%
  select(-pft) %>%
  as.matrix() %>%
  log10()

hier_pattern <- paste0("hier\\.", mass_area, "\\.clm45")
r_hm_file <- tail(list.files("output", hier_pattern, full.names = TRUE), 1)
r_hm <- readRDS(r_hm_file)
# Remove unnecessary stuff from the hierarchical output so that
r_hm_mat <- coda:::as.matrix.mcmc.list(
  window(r_hm$samples, start = 10000)
)
r_hm_mat <- r_hm_mat[, !grepl("^Corr", colnames(r_hm_mat))]
r_hm_mat <- r_hm_mat[, !grepl("global", colnames(r_hm_mat))]

# Multivariate results, globally
gmulti_pattern <- paste0("multi\\.", mass_area, "\\.clm45\\.NA")
gmulti_file <- tail(list.files("output", gmulti_pattern,
                               full.names = TRUE), 1)
gmulti <- readRDS(gmulti_file)
gmulti_mat <- coda:::as.matrix.mcmc.list(gmulti$samples)
gmulti_mat <- gmulti_mat[, !grepl("^Corr", colnames(gmulti_mat))]

# Load all the multivariate results by PFT
multi_results <- lapply(
  levels(data_groups),
  function(g) {
    pattern <- paste0("multi\\.", mass_area, "\\.clm45\\.", g, ".2019")
    f <- tail(list.files("output", pattern, full.names = TRUE), 1)
    d <- readRDS(f)
    dmat <- coda:::as.matrix.mcmc.list(d$samples)
    dmat <- dmat[, !grepl("^Corr", colnames(dmat))]
    dmat
  }
)
names(multi_results) <- levels(data_groups)

# Load univariate results
uni_results <- lapply(
  levels(data_groups),
  function(g) {
    f <- paste0("output/uni.", mass_area, ".clm45.", g, ".rds")
    d <- readRDS(f)
    d$stats$mu$Mean
  }
)
names(uni_results) <- levels(data_groups)

##################################################
# Begin cross-validation
##################################################

# Create a matrix of indices (which I will eventually sample) where at least 2
# traits are present
present <- !is.na(data_mat)
gt2 <- rowSums(present) >= 2
data_mat_sub <- data_mat[gt2, ]
groups_sub <- data_groups[gt2]

cross_validate <- function(data_mat, groups, nremove) {

  message("Beginning cross validation")
  if (exists("incv")) {
    incv <<- incv + 1
    message(incv, " of ", ncv)
  }

  sample_inds <- which(!is.na(data_mat), arr.ind = TRUE)

  imissing <- sample_inds[sample(nrow(sample_inds), nremove), ]
  test_data <- data_mat
  test_data[imissing] <- NA

  calc_rmse <- function(j, means) {
    ii <- imissing[imissing[, "col"] == j, ]
    obs <- data_mat[ii]
    pred <- means[ii]
    sqrt(mean((obs - pred) ^ 2))
  }

  rmse_by_trait <- function(means) {
    out <- vapply(1:7, calc_rmse, numeric(1), means = means)
    names(out) <- colnames(data_mat)
    out
  }

  # Hierarchical model
  message("Hierarchical model")
  boot <- mvtraits::bootstrap_missing_hier(r_hm_mat, test_data, groups)
  boot_means <- apply(boot, c(1, 2), mean)
  rmse_h <- rmse_by_trait(boot_means)

  # Multivariate model -- global
  message("Global multivariate model")
  mgboot <- mvtraits::bootstrap_missing(gmulti_mat, test_data)
  mgboot_means <- apply(mgboot, c(1, 2), mean)
  rmse_mg <- rmse_by_trait(mgboot_means)

  # Multivariate model -- by PFT
  mboot_means <- matrix(NA_real_, nrow(test_data), ncol(test_data))
  for (g in levels(groups)) {
    message("PFT multivariate model for PFT: ", g)
    ig <- groups == g
    gdata <- test_data[ig, ]
    gmat <- multi_results[[g]]
    mboot <- mvtraits::bootstrap_missing(gmat, gdata)
    mboot_means[ig,] <- apply(mboot, c(1, 2), mean)
  }

  rmse_m <- rmse_by_trait(mboot_means)

  # Univariate model
  message("Univariate model")
  rmse_u <- vapply(
    1:7,
    function(x) {
      ii <- imissing[imissing[, "col"] == x, ]
      obs <- data_mat[ii]
      uni_vec <- vapply(uni_results, "[[", numeric(1), x = x)
      pred <- uni_vec[groups[ii[, "row"]]]
      sqrt(mean((obs - pred) ^ 2))
    },
    numeric(1)
  )
  names(rmse_u) <- colnames(data_mat)

  rmse_all <- rbind(hier = rmse_h, multi_global = rmse_mg,
                    multi_group = rmse_m, uni = rmse_u) %>%
    as_tibble(rownames = "model")

  message("Done.")

  rmse_all
}

incv <- 1
cv_results <- replicate(ncv, cross_validate(data_mat, data_groups),
                        simplify = FALSE)

cv_results_df <- bind_rows(cv_results, .id = "i")
resultsfile <- file.path(
  resultsdir,
  paste0("cv-results-", mass_area, ".csv")
)
write_csv(cv_results_df, resultsfile)
