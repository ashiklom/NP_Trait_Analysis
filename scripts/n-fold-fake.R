library(tidyverse)

outdir <- here::here("output")

try_data <- readRDS(here::here("extdata", "traits_analysis.rds"))

use_rxp <- "leaf_lifespan|SLA|mass"
dat_df_all <- try_data %>%
  as_tibble() %>%
  select(pft = clm45, matches(use_rxp)) %>%
  filter_at(vars(matches(use_rxp)), any_vars(!is.na(.)))

data_groups <- dat_df_all[["pft"]]
data_mat <- dat_df_all %>%
  select(-pft) %>%
  as.matrix()

r_hm <- readRDS("output/hier.mass.clm45.NA.2018-02-12-1154.rds")

# Create a matrix of indices (which I will eventually sample)
present <- !is.na(data_mat)
ind_mat <- matrix(0, nrow(data_mat), ncol(data_mat))
ind_mat[present] <- seq_len(sum(present))

# Subset to rows where at least 2 traits are present
cv_rows <- rowSums(!is.na(data_mat)) >= 2
ind_mat_use <- ind_mat[cv_rows, ]
sample_inds <- ind_mat_use[ind_mat_use != 0]

ncv <- 5
nremove <- 1000

library(mvtraits)

l <- list()

library(foreach, exclude = c("accumulate", "when"))
library(doFuture)

options(future.globals.maxSize = 700 * 1024^2)

future::plan(future.callr::callr())
registerDoFuture()

l <- times(ncv) %dopar% {
  # create missing data
  imissing <- sort(sample(sample_inds, nremove))
  true_vals <- data_mat[imissing]
  missing_dat <- data_mat
  missing_dat[imissing] <- NA
  mmissing <- which(is.na(missing_dat), arr.ind = TRUE)
  # Bootstrap missing data
  boot <- bootstrap_missing_hier(r_hm, missing_dat, data_groups, n = 20)
  # Summarize more efficiently
  boot_out <- matrix(numeric(), nrow(mmissing), dim(boot)[[3]])
  for (j in seq_len(ncol(boot_out))) {
    boot_out[,j] <- boot[,,j][mmissing]
  }
  boot_means <- rowMeans(boot_out)
  list(true = true_vals, boot = boot_means)
}

for (i in seq_len(ncv)) {
  message("Iteration ", i, " of ", ncv)
}
