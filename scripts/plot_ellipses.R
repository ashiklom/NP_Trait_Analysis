library(nptraits)

figures_dir <- 'figures'
dir.create(figures_dir, showWarnings = FALSE)

all_summaries <- readRDS('results/summaries_processed.rds')

example_dat <- all_summaries %>%
    filter(model_type == 'hier', area_mass == 'mass', pft_scheme == 'clm45') %>%
    unnest()

pft_cols <- pft_colors[["clm45"]]

custom_ellipse <- function(...) {
    update_geom_defaults("spoke", list(size = 2))
    update_geom_defaults("point", list(size = 4))
    single_ellipse(...) +
        scale_color_manual(values = pft_cols) +
        theme_bw()
}

all_params <- example_dat$param %>%
    forcats::fct_relevel('SLA') %>%
    levels()

ellipse_dat <- all_params %>%
    combn(m = 2) %>%
    t() %>%
    as_tibble() %>%
    select(xparam = V1, yparam = V2) %>%
    mutate(
        filename = paste('ellipse_plot', xparam, yparam, 'hier', 'clm45', 'png', sep = '.'),
        filename = file.path(figures_dir, filename),
        ellipse_plot = map2(xparam, yparam, ~custom_ellipse(example_dat, .x, .y))
    )

# ellipse_dat[[1, 'ellipse_plot']]

ellipse_dat %$%
    pwalk(list(filename = filename, plot = ellipse_plot), ggsave)


# Old full ellipse code ---------------------------------------------------

ellipses <- all_summaries %>%
  filter(model_type == 'hier') %>%
  mutate(
    ellipse_plot = map(data, ellipse_matrix),
    fname = paste('ellipse_plot', model_type, area_mass, pft_scheme, 'png', sep = '.'),
    fname = file.path(figures_dir, fname)
  )

ellipses %$%
  walk2(fname, ellipse_plot, ggsave)
