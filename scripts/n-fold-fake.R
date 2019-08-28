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

r_hm <- readRDS("output/hier.mass.clm45.NA.2019-08-26-1533.rds")

# Remove unnecessary stuff from the hierarchical output so that
r_hm_mat <- coda:::as.matrix.mcmc.list(
  window(r_hm$samples, start = 10000)
)
r_hm_mat <- r_hm_mat[, !grepl("^Corr", colnames(r_hm_mat))]
r_hm_mat <- r_hm_mat[, !grepl("global", colnames(r_hm_mat))]

# Create a matrix of indices (which I will eventually sample) where at least 2
# traits are present
present <- !is.na(data_mat)
cv_rows <- matrix(rowSums(present) >= 2, nrow(data_mat), ncol(data_mat))
sample_inds <- which(cv_rows & present)

# Rows: ((i - 1) %% nrow) + 1
# Cols: ((i - 1) %/% nrow) + 1

ncv <- 5
nremove <- 1000

library(foreach, exclude = c("accumulate", "when"))
library(doFuture)

future::plan(future.callr::callr())
registerDoFuture()

l <- times(ncv) %dopar% {
## for (i in seq_len(ncv)) {
  # create missing data
  imissing <- sort(sample(sample_inds, nremove))
  imissing_m <- cbind(
    i = imissing,
    row = ((imissing - 1) %% nrow(data_mat)) + 1,
    col = ((imissing - 1) %/% nrow(data_mat)) + 1
  )
  true_vals <- data_mat[imissing]
  missing_dat <- data_mat
  missing_dat[imissing] <- NA
  ## mmissing <- which(is.na(missing_dat), arr.ind = TRUE)
  # Bootstrap missing data
  boot <- mvtraits::bootstrap_missing_hier(r_hm_mat, missing_dat, data_groups)
  # Summarize more efficiently
  boot_out <- matrix(numeric(), length(imissing), dim(boot)[[3]])
  for (j in seq_len(ncol(boot_out))) {
    boot_out[,j] <- boot[,,j][imissing]
  }
  boot_means <- rowMeans(boot_out)
  list(true = true_vals, boot = boot_means, inds = imissing_m)
}

# Something's not right here...
jj <- imissing_m[, "col"] == 4
plot(true_vals[jj] ~ boot_means[jj])
