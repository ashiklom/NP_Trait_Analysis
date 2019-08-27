#!/usr/bin/env Rscript

# Generate the input data for the N-fold cross-validation

library(tidyverse)
requireNamespace("here", quietly = TRUE)
set.seed(4321234)

# Number of samples to remove for each cross-validation
nremove <- 1000
# Number of cross-validations
ncv <- 50

nfold_dir <- here::here("output", "n-fold-inputs")
dir.create(nfold_dir, recursive = TRUE, showWarnings = FALSE)

try_data <- readRDS(here::here("extdata", "traits_analysis.rds"))

# Do this only for mass-based data. This is more about testing the method than
# about anything ecological.
use_rxp <- "leaf_lifespan|SLA|mass"
dat_df_all <- try_data %>%
  select(pft = clm45, matches(use_rxp)) %>%
  filter_at(vars(matches(use_rxp)), any_vars(!is.na(.)))

data_groups <- dat_df_all[["pft"]]
data_mat <- dat_df_all %>%
  select(-pft) %>%
  as.matrix()

source("scripts/informative_prior.R")

# Create a matrix of indices (which I will eventually sample)
present <- !is.na(data_mat)
ind_mat <- matrix(0, nrow(data_mat), ncol(data_mat))
ind_mat[present] <- seq_len(sum(present))

# Subset to rows where at least 2 traits are present
cv_rows <- rowSums(!is.na(data_mat)) >= 2
ind_mat_use <- ind_mat[cv_rows, ]
sample_inds <- ind_mat_use[ind_mat_use != 0]

cv_list <- list()

for (i in seq_len(ncv)) {
  samples <- sample(sample_inds, nremove)
  true_vals <- data_mat[samples]
  missing_dat <- data_mat
  missing_dat[samples] <- NA
  cv_list[[i]] <- list(
    data_matrix = missing_dat,
    data_groups = data_groups,
    true_values = true_vals,
    missing_indices = samples
  )
  ## outfile <- file.path(
  ##   nfold_dir,
  ##   sprintf("nfold-%02d.rds", i)
  ## )
  ## saveRDS(l, outfile)
}

nfold_cv_fun <- function(input, model_type, by_pft, prior) {
  library(mvtraits)

  autofit <- TRUE
  max_attempts <- 50
  keep_samples <- 20000
  threshold <- 1.3

  data_mat <- input$data_mat
  data_groups <- input$data_groups

  if (model_type == "univariate") {
    message("Fitting univariate model...")
    if (!by_pft) {
      message("...globally.")
      stop("not implemented")
    } else {
      message("...by PFT.")
      stop("not implemented")
    }
  } else if (model_type == "multivariate") {
    message("Fitting multivariate model...")
    if (!by_pft) {
      message("...globally.")
      result <- fit_mvnorm(
        data_mat,
        prior = prior,
        nchains = parallel::detectCores(),
        autofit = autofit,
        max_attempts = max_attempts,
        keep_samples = keep_samples,
        threshold = threshold
      )
      data_boot <- bootstrap_missing(data_pft)
      ii <- which(is.na(data_pft), arr.ind = TRUE)
      output <- tibble::tibble(
        irow = ii[, "row"],
        icol = ii[, "col",]
        trait = colnames(data_pft)[icol],
        true_value = input$true_vals[r_group],
        mean = apply(data_boot, c(1, 2), mean)[ii],
        lo = apply(data_boot, c(1, 2), quantile, probs = 0.025)[ii],
        hi = apply(data_boot, c(1, 2), quantile, probs = 0.975)[ii]
      )
    } else {
      message("...by PFT.")
      upft <- levels(data_groups)
      for (pft in upft) {
        message("Fitting PFT: ", pft)
        r_group <- data_groups == pft
        data_pft <- data_mat[r_group, ]
        result <- fit_mvnorm(
          data_pft,
          prior = prior,
          nchains = parallel::detectCores(),
          autofit = autofit,
          max_attempts = max_attempts,
          keep_samples = keep_samples,
          threshold = threshold
        )
        message("Bootstrap missing data")
        data_boot <- bootstrap_missing(data_pft)
        ii <- which(is.na(data_pft), arr.ind = TRUE)
        output <- tibble::tibble(
          irow = ii[, "row"],
          trait = colnames(data_pft)[ii[, "col"]],
          true_value = input$true_vals[r_group],
          mean = apply(data_boot, c(1, 2), mean)[ii],
          lo = apply(data_boot, c(1, 2), quantile, probs = 0.025)[ii],
          hi = apply(data_boot, c(1, 2), quantile, probs = 0.975)[ii]
        )
      }
    }
  } else if (model_type == "hierarchical") {
    stop("not implemented")
  }
}

dm <- cv_list[[1]]$data_matrix
ii <- which(is.na(dm), arr.ind = TRUE)
trait <-

cmq_result <-
