library(mvtraits)
library(tidyverse)

all_summaries <- readRDS('results/summaries_processed.rds')

mass_params <- c('leaf_lifespan', 'SLA', 'Nmass', 'Pmass', 'Rdmass', 'Vcmax_mass', 'Jmax_mass')
area_params <- gsub('mass$', 'area', mass_params)

barplot_prep <- function(dat, area_mass) {
  param_levels <- switch(as.character(area_mass), area = area_params, mass = mass_params)
  dat %>%
    mutate(
      pft = factor(pft) %>% forcats::fct_relevel("global"),
      param = factor(param, param_levels),
      xparam = factor(xparam, param_levels[-1]),
      yparam = factor(yparam, param_levels[-length(param_levels)])
      )
}

plots <- all_summaries %>%
  filter(model_type == 'hier') %>%
  group_by(model_type, area_mass, pft_scheme, run_pft) %>%
  nest() %>%
  mutate(data = map2(data, area_mass, barplot_prep),
         bar_plot = map(data, summary_barplot, varname = 'Corr'))

walk(plots[['bar_plot']], print)
