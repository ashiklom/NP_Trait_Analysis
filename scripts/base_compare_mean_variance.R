library(mvtraits)
library(tidyverse)

# TODO: Redraw both of these as single figures, faceted as param ~ mass_area.

source("scripts/pft_abbreviations.R")
cachefile <- ".cache/mvtraits_results.rds"
results_all <- readRDS(cachefile)
try_data <- readRDS("extdata/traits_analysis.rds")
try_sub <- try_data %>%
  select(pft = clm45, Jmax_area:Vcmax_mass,
         -Latitude, -Longitude, -LMA)

for (ma in c("mass", "area")) {
  message("Generating plots for: ", ma)

  par_levels <- switch(ma, mass = mass_params, area = area_params)

  sample_size <- try_sub %>%
    select(pft, one_of(par_levels)) %>%
    mutate(
      pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
    ) %>%
    group_by(pft) %>%
    summarize_all(~sum(!is.na(.))) %>%
    gather("param", "sample_size", -pft) %>%
    mutate(
      param = factor(param, par_levels)
    )

    max_ss <- max(sample_size$sample_size)

  multi_summary <- results_all %>%
    filter(model_type %in% c("uni", "multi"), mass_area == ma) %>%
    mutate(
      mu_mean = map(data, c("mu", "Mean")) %>%
        map(~as_tibble(as.list(.))),
      mu_lo = map(data, c("mu", "2.5%")) %>%
        map(~as_tibble(as.list(.))),
      mu_hi = map(data, c("mu", "97.5%")) %>%
        map(~as_tibble(as.list(.)))
        ) %>%
    select(model_type:pft, mu_mean:mu_hi) %>%
    unnest(.sep = ".") %>%
    gather("variable", "value", -(model_type:pft), na.rm = TRUE) %>%
    separate(variable, c("stat", "param"), sep = "\\.") %>%
    spread(stat, value) %>%
    mutate(
      pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr),
      param = factor(param, par_levels)
    )

  hier_prep <- results_all %>%
    filter(model_type == "hier", mass_area == ma) %>%
    select(model_type:pft_type, data) %>%
    mutate(
      mu_vals = map(data, "mu_group"),
      pft = map(mu_vals, names),
      mu_df = map2(pft, mu_vals, ~tibble(pft = ..1, dat = ..2))
    ) %>%
    select(-data, -mu_vals, -pft) %>%
    unnest()

  hier_summary <- hier_prep %>%
    mutate(
      mu_mean = map(dat, c("Mean")) %>%
        map(~as_tibble(as.list(.))),
      mu_lo = map(dat, c("2.5%")) %>%
        map(~as_tibble(as.list(.))),
      mu_hi = map(dat, c("97.5%")) %>%
        map(~as_tibble(as.list(.)))
        ) %>%
    select(model_type:pft, mu_mean:mu_hi) %>%
    unnest(.sep = ".") %>%
    gather("variable", "value", -(model_type:pft), na.rm = TRUE) %>%
    separate(variable, c("stat", "param"), sep = "\\.") %>%
    spread(stat, value) %>%
    mutate(
      pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr),
      param = factor(param, par_levels)
    )

  # Absolute comparison of means
  plot_dat <- multi_summary %>%
    filter(mass_area == ma, pft != "GLOB") %>%
    bind_rows(hier_summary) %>%
    left_join(sample_size) %>%
    mutate(
      model_type = factor(model_type, c("uni", "multi", "hier")) %>%
        forcats::lvls_revalue(c("univariate", "multivariate", "hierarchical")),
      too_few = sample_size < 2,
      mu_hi = if_else(too_few, NA_real_, mu_hi),
      mu_lo = if_else(too_few, NA_real_, mu_lo),
      mu_mean = if_else(too_few, NA_real_, mu_mean)
    ) %>%
    mutate_at(
      c("mu_mean", "mu_lo", "mu_hi"),
      ~10 ^ .
    ) %>%
    mutate(
      mu_hi = if_else(param == "Vcmax_mass" & mu_hi > 100, 100, mu_hi),
      mu_hi = if_else(param == "Rdarea" & mu_hi > 2, 2, mu_hi),
      param = forcats::lvls_revalue(param, param_fancy_chr[par_levels])
    )

  p_means <- ggplot(plot_dat) +
    aes(x = interaction(model_type, pft),
        y = mu_mean, ymin = mu_lo, ymax = mu_hi,
        color = pft, shape = model_type) +
    geom_pointrange(size = 0.5) +
    facet_wrap(~param, scales = "free_y", ncol = 2,
               labeller = label_parsed) +
    scale_y_continuous() +
    scale_color_manual(values = pft_colors) +
    guides(
      shape = guide_legend(title = "Model type"),
      color = guide_legend(title = "PFT")
    ) +
    ylab("Trait estimate mean and 95% CI") +
    xlab("Model type and PFT") +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  p_means
  ggsave(sprintf("figures/mean_comparison_%s.pdf", ma), 
          p_means, width = 9, height = 7)

  ## Relative uncertainty
  cv_dat <- plot_dat %>%
    mutate(
      cv = abs((mu_hi - mu_lo) / mu_mean)
    ) %>%
  filter(sample_size > 3)

  cv_mod <- cv_dat %>%
    group_by(model_type) %>%
    nest() %>%
    mutate(
      cvfit = map(data, ~lm(log10(cv) ~ log10(sample_size), data = .))
    )

  cv_pred <- cv_mod %>%
    mutate(
      grid = map(data, modelr::data_grid, sample_size = seq(5, max_ss)),
      cvpred = map2(grid, cvfit, modelr::add_predictions)
    ) %>%
  unnest(cvpred) %>%
  mutate(cv = 10 ^ pred)
  
  x_lims <- range(cv_pred$sample_size)
  y_lims <- range(cv_pred$cv)

  relative_ci <- ggplot(cv_dat) +
    aes(x = sample_size, y = cv, color = model_type) +
    geom_point() +
    geom_line(data = cv_pred) +
    scale_x_continuous(trans = "log10") +
    coord_cartesian(xlim = x_lims, ylim = y_lims) +
    xlab("Sample size") +
    ylab("Relative width of 95% CI") +
    scale_color_brewer(type = "qual", palette = 2) +
    guides(color = guide_legend(title = "Model type")) +
    theme_bw() +
    theme(legend.position = c(0.8, 0.6))
  relative_ci
  ggsave(sprintf("figures/relative_ci_sample_size_%s.pdf", ma),
          relative_ci, width = 4.4, height = 3)
}
