library(nptraits)

all_summaries <- readRDS('results/summaries_processed.rds')
figures_dir <- 'figures'

prep_data <- function(dat) {
    dat %>%
        mutate(xparam = factor(xparam, tail(levels(param), -1)),
               yparam = factor(yparam, head(levels(param), -1)))
}

custom_barplot <- function(..., pft_scheme) {
    pft_cols <- pft_colors[[pft_scheme]]
    summary_barplot(...) +
        theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        scale_fill_manual(values = pft_cols)
}

plots <- all_summaries %>%
    filter(model_type == 'hier') %>%
    mutate(
        data = map(data, prep_data),
        facet_list = ifelse(area_mass == 'mass', list(list()), list(list(facets = ~yparam + xparam))),
        bar_plot = pmap(
            list(
                summary_data = data,
                facet_list = facet_list,
                pft_scheme = pft_scheme
            ),
            custom_barplot,
            varname = 'Corr'
        ),
        filename = paste(
            'correlation_barplot',
            model_type,
            area_mass,
            pft_scheme,
            'png',
            sep = '.'
        ),
        filename = file.path(figures_dir, filename)
    )

# plots[[1, 'bar_plot']]

plots %$%
    pwalk(list(filename = filename, plot = bar_plot), ggsave, width = 9, height = 8)
