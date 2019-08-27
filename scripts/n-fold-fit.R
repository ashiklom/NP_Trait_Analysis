#!/usr/bin/env Rscript

# Fit a univariate, multivariate, or hierarchical model
library(tidyverse)
library(mvtraits)
requireNamespace("here", quietly = TRUE)
requireNamespace("clustermq", quietly = TRUE)

argv <- commandArgs(trailingOnly = TRUE)
# Example: argv <- c("1", "m", "glob")

i_nfold <- as.numeric(argv[[1]])
model_type <- match.arg(
  argv[[2]],
  c("univariate", "multivariate", "hierarchical")
)

if (model_type %in% c("univariate", "multivariate")) {
  by_pftc <- match.arg(argv[[3]], c("global", "pft"))
  by_pft <- by_pftc == "pft"
}

nfold_dir <- here::here("output", "n-fold-inputs")
nfold_file <- file.path(nfold_dir, sprintf("nfold-%02d.rds", i_nfold))
l <- readRDS(nfold_file)

nfold_outdir <- here::here("output", "n-fold-outputs")
dir.create(nfold_outdir, recursive = FALSE, showWarnings = TRUE)

use_rxp <- "leaf_lifespan|SLA|mass"
data_mat <- l[["data_matrix"]]
if (model_type == "hierarchical") {
  data_groups <- l[["data_groups"]]
}

source("scripts/informative_prior.R")

autofit <- TRUE
max_attempts <- 50
keep_samples <- 20000
threshold <- 1.3

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
    outfile <- file.path(
      nfold_outdir,
      sprintf("nfold-%02d-multi.rds", i_nfold)
    )
    saveRDS(result, outfile)
  } else {
    message("...by PFT.")
    grps <- l[["data_groups"]]
    upft <- levels(grps)
    for (pft in upft) {
      message("Fitting PFT: ", pft)
      data_pft <- data_mat[grps == pft, ]
      result <- fit_mvnorm(
        data_pft,
        prior = prior,
        nchains = parallel::detectCores(),
        autofit = autofit,
        max_attempts = max_attempts,
        keep_samples = keep_samples,
        threshold = threshold
      )
      outfile <- file.path(
        nfold_outdir,
        sprintf("nfold-%02d-multi-%s.rds", i_nfold, pft)
      )
      saveRDS(result, outfile)
    }
  }
} else if (model_type == "hierarchical") {

}
