library(nptraits)

all_summaries <- readRDS('results/summaries_processed.rds')
figures_dir <- 'figures'

prep_data <- function(dat) {
    dat %>%
        mutate(xparam = factor(xparam, tail(levels(param), -1)),
               yparam = factor(yparam, head(levels(param), -1)))
}

plots <- all_summaries %>%
    filter(model_type == 'hier') %>%
    mutate(data = map(data, prep_data),
           bar_plot = map(data, summary_barplot, varname = 'Corr'),
           filename = paste('correlation_barplot', model_type, area_mass, pft_scheme, 'pdf', sep = '.'),
           filename = file.path(figures_dir, filename)
    )

plots %$%
    pwalk(list(filename = filename, plot = bar_plot), ggsave)
