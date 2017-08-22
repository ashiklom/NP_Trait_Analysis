library(tidyverse)

all_summaries <- readRDS('results/summaries_processed.rds')
try_data <- readRDS('traits_analysis.rds')

pft_levels <- select(try_data, jules1:custom) %>% map(levels)
pft_lengths <- map(pft_levels, length)
mass_params <- c('SLA', 'leaf_lifespan', 'Nmass', 'Pmass', 'Rdmass', 'Vcmax_mass', 'Jmax_mass')
area_params <- gsub('mass', 'area', mass_params)

validate_data <- function(dat, model_type, run_pft, area_mass) {
  message('model_type: ', model_type)
  message('run_pft: ', run_pft)
  message('area_mass: ', area_mass)
  params <- switch(area_mass, mass = mass_params, area = area_params)
  if (model_type == 'hier') {
    stopifnot(dat %>% distinct(pft) %>% pull %>% setequal(pft_levels))
  } else if (model_type == 'multi') {
    stopifnot(dat %>% distinct(pft) %>% length %>% magrittr::equals(1),
              dat %>% distinct(pft) %>% magrittr::equals(run_pft))
  } else {
    stop('Invalid model type ', model_type)
  }
  stopifnot(dat %>% distinct(param) %>% pull %>% setequal(params))
  return(dat)
}

sums_nested <- all_summaries %>%
  group_by(model_type, area_mass, pft_scheme, run_pft) %>%
  nest() %>%
  mutate_if(is.factor, as.character)

with(sums_nested, pmap(list(data, model_type, run_pft, area_mass), validate_data))

sums_nested[[1, 'data']] %>%
  distinct(pft) %>%
  pull
