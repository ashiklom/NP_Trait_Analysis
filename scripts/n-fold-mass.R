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
  as.matrix() %>%
  log10()

r_hm <- readRDS("output/hier.mass.clm45.NA.2019-08-26-1533.rds")
# Remove unnecessary stuff from the hierarchical output so that
r_hm_mat <- coda:::as.matrix.mcmc.list(
  window(r_hm$samples, start = 10000)
)
r_hm_mat <- r_hm_mat[, !grepl("^Corr", colnames(r_hm_mat))]
r_hm_mat <- r_hm_mat[, !grepl("global", colnames(r_hm_mat))]

# Multivariate results, globally
gmulti <- readRDS("output/multi.mass.clm45.NA.2019-08-26-1719.rds")
gmulti_mat <- coda:::as.matrix.mcmc.list(gmulti$samples)
gmulti_mat <- gmulti_mat[, !grepl("^Corr", colnames(gmulti_mat))]

# Load all the multivariate results by PFT
multi_results <- lapply(
  levels(data_groups),
  function(g) {
    pattern <- paste0("multi\\.mass\\.clm45\\.", g, ".2019")
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
    f <- paste0("output/uni.mass.clm45.", g, ".rds")
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
  boot <- mvtraits::bootstrap_missing_hier(r_hm_mat, test_data, groups)
  boot_means <- apply(boot, c(1, 2), mean)
  rmse_h <- rmse_by_trait(boot_means)

  # Multivariate model -- global
  mgboot <- mvtraits::bootstrap_missing(gmulti_mat, test_data)
  mgboot_means <- apply(mgboot, c(1, 2), mean)
  rmse_mg <- rmse_by_trait(mgboot_means)

  # Multivariate model -- by PFT
  mboot_means <- matrix(NA_real_, nrow(test_data), ncol(test_data))
  for (g in levels(data_groups)) {
    ig <- groups == g
    gdata <- test_data[ig, ]
    gmat <- multi_results[[g]]
    mboot <- mvtraits::bootstrap_missing(gmat, gdata)
    mboot_means[ig,] <- apply(mboot, c(1, 2), mean)
  }

  rmse_m <- rmse_by_trait(mboot_means)

  # Univariate model
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

  rmse_all
}

ncv <- 20
cv_results <- replicate(ncv, cross_validate(data_mat, data_groups),
                        simplify = FALSE)

cv_results_df <- bind_rows(cv_results, .id = "i")
write_csv(cv_results_df, "results/cv-results-mass.csv")

plt <- cv_results_df %>%
  gather(trait, value, -i, -model) %>%
  group_by(model, trait) %>%
  summarize(M = mean(value), S = sd(value)) %>%
  ungroup() %>%
  mutate(model = factor(model, c("uni", "multi_global",
                                 "multi_group", "hier")),
         trait = factor(trait, shiklomanov2017np::mass_params)) %>%
  ggplot() +
  aes(x = model, y = M) +
  geom_col() +
  facet_wrap(vars(trait), scales = "free_y") +
  cowplot::theme_cowplot() +
  theme(
    axis.text.x = element_text(angle = )
  )
