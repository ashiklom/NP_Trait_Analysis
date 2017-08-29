library(nptraits)

figures_dir <- 'figures'
dir.create(figures_dir, showWarnings = FALSE)

all_summaries <- readRDS('results/summaries_processed.rds')

ellipses <- all_summaries %>%
  filter(model_type == 'hier') %>%
  mutate(
    ellipse_plot = map(data, ellipse_matrix),
    fname = paste(model_type, area_mass, pft_scheme, 'pdf', sep = '.'),
    fname = file.path(figures_dir, fname)
  )

ellipses %$%
  walk2(fname, ellipse_plot, ggsave)
