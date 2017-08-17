library(mvtraits)
library(tidyverse)

all_summaries <- readRDS('results/summaries.rds')

mass_params <- c('leaf_lifespan', 'SLA', 'Nmass', 'Pmass', 'Rdmass', 'Vcmax_mass', 'Jmax_mass')
area_params <- gsub('mass$', 'area', mass_params)

proc <- function(dat, area_mass) {
  params <- switch(area_mass, 'mass' = mass_params, 'area' = area_params)
  dat %>%
    select(-rowname) %>%
    rearrange_df(params)
}

plots <- all_summaries %>%
  group_by(model_type, area_mass, pft_scheme, pft, fname) %>%
  nest()

for (i in 1:nrow(plots)) {
  print(i)
  sdat <- plots[i, ]
  print(sdat)
  params <- switch(sdat[['area_mass']], 'mass' = mass_params, 'area' = area_params)
  sdat[[1, 'data']] %>%
    select(-rowname) %>%
    rearrange_df(params) %>%
    rename(pft = group) %>%
    summary_barplot('Corr') %>%
    print()
}

dat %>% distinct(variable)

summary_barplot(dat, 'Corr', aes_list = list(x = 'group', fill = 'group'))
