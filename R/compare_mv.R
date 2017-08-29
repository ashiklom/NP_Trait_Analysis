#' Compare means and variances between multivariate and hierarchical models
#'
#' @param summary_data Nested tibble containing summary of results
#' @param area_mass Normalization, either "mass" or "area" (character)
#' @param pft_scheme PFT definition scheme (e.g. "clm45") (character)
#'
#' @return tibble containing plots comparing mean and variance
#' @export
compare_mv <- function(summary_data, area_mass, pft_scheme) {

  dat <- summary_data %>%
    filter_(paste('area_mass ==', shQuote(area_mass)), paste('pft_scheme ==', shQuote(pft_scheme)))
  stopifnot(nrow(dat) > 0)

  hier_dat <- dat %>%
    filter(model_type == 'hier') %>%
    unnest()

  pft_levels <- hier_dat %>% pull(pft) %>% levels()

  dat2 <- dat %>%
    mutate(run_pft = factor(run_pft, pft_levels)) %>%
    unnest() %>%
    mutate(pft = if_else(model_type == 'hier', pft, run_pft))

  compare_mean <- ggplot(dat2 %>% filter(variable == 'mu')) +
    aes(x = model_type, y = Mean, ymin = `2.5%`, ymax = `97.5%`) +
    geom_pointrange() +
    facet_grid(param ~ pft, scales = 'free_y') +
    labs(x = "Model type", y = "log10(Trait) variance estimate")

  compare_var <- ggplot(dat2 %>% filter(variable == 'Sigma', xparam == yparam)) +
    aes(x = model_type, y = Mean, ymin = `2.5%`, ymax = `97.5%`) +
    geom_col() +
    facet_grid(xparam ~ pft) +
    labs(x = "Model type", y = "log10(Trait) variance estimate")

  return(tibble(mean_plot = list(compare_mean), var_plot = list(compare_var)))
}
