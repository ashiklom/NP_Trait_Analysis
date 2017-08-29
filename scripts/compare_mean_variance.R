library(nptraits)

all_summaries <- readRDS('results/summaries_processed.rds')
figures_dir <- 'figures'

plot_data <- all_summaries %>%
    distinct(area_mass, pft_scheme) %>%
    mutate(plots = map2(area_mass, pft_scheme, compare_mv, summary_data = all_summaries)) %>%
    mutate_if(is.factor, as.character) %>%
    unnest()

plot_data %$%
    pwalk(list(area_mass, pft_scheme, mean_plot),
          ~ ggsave(filename = file.path(
              figures_dir, paste(..1, ..2, 'compare_mean.pdf', sep = '.')
          ),
          plot = ..3))

plot_data %$%
    pwalk(list(area_mass, pft_scheme, var_plot),
          ~ ggsave(filename = file.path(
              figures_dir, paste(..1, ..2, 'compare_variance.pdf', sep = '.')
          ),
          plot = ..3))
