prior_df <- tribble(
  ~param, ~mean, ~stdev,
  'Jmax_area', 2, 0.5,
  'Jmax_mass', 0, 0.4,
  'leaf_lifespan', 0.75, 0.5,
  'Narea', 0.25, 0.5,
  'Nmass', 1.25, 0.5,
  'Parea', -1, 0.5,
  'Pmass', 0, 0.3,
  'Rdarea', 0, 0.5,
  'Rdmass', -2, 0.5,
  'SLA', 1, 0.75,
  'Vcmax_area', -0.5, 0.5,
  'Vcmax_mass', 1.7, 0.5
) %>%
  mutate(variance = stdev ^ 2)

if (exists('data_df') && exists('use_rxp') && exists('data_mat')) {
  prior_sub <- prior_df %>%
    filter(grepl(use_rxp, param)) %>%
    mutate(param = factor(param, colnames(data_mat))) %>%
    arrange(param)
  prior <- list()
  prior_names <- as.character(prior_sub[['param']])
  means <- setNames(prior_sub[['mean']], prior_names)
  variances <- diag(prior_sub[['variance']])
  dimnames(variances) <- list(prior_names, prior_names)
  prior[['mu_global']] <- means
  prior[['sigma_global']] <- variances
  if (exists('data_groups')) {
    npft <- max(as.integer(data_groups))
    stopifnot(npft == length(unique(data_groups)))
    prior[['mu_group']] <- matrix(rep(means, npft), nrow = npft, byrow = TRUE)
    prior[['sigma_group']] <- mvtraits::matrep(variances, npft)
  }
}
