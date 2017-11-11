library(mvtraits)
library(tidyverse)

source("scripts/pft_abbreviations.R")
cachefile <- ".cache/mvtraits_results.rds"
results_all <- readRDS(cachefile)

results_hier <- results_all %>%
  filter(model_type == "hier") %>%
  select(mass_area, data) %>%
  mutate(
    mu_group = map(data, "mu_group"),
    Sigma_group = map(data, "Sigma_group"),
    pft_names = map(mu_group, names),
    pft_dat = pmap(
      list(mu_group, Sigma_group, pft_names),
      ~tibble(pft = ..3, mu = ..1, Sigma = ..2)
    )
  ) %>%
  unnest(pft_dat) %>%
  mutate(
    mu_mean = map(mu, "Mean"),
    Sigma_mean = map(Sigma, "Mean") %>% map(diag),
    param = map(mu_mean, names)
  ) %>%
  unnest(mu_mean, Sigma_mean, param)

results_table <- results_hier %>%
  mutate(
    lo = 10 ^ (mu_mean - 1.96 * Sigma_mean),
    mid = 10 ^ mu_mean,
    hi = 10 ^ (mu_mean + 1.96 * Sigma_mean),
    lo_f = formatC(lo, digits = 3),
    mid_f = formatC(mid, digits = 3),
    hi_f = formatC(hi, digits = 3),
    value = sprintf("%s (%s - %s)", mid_f, lo_f, hi_f)
  )
results_table %>%
  filter(!(param %in% c("SLA", "leaf_lifespan") & mass_area == "mass")) %>%
  select(pft, param, value) %>%
  mutate(
    param = factor(param, both_params) %>% 
      forcats::lvls_revalue(param_markdown[both_params]),
    pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
  ) %>%
  spread(param, value) %>%
  arrange(pft) %>%
  pander::pandoc.table("")
