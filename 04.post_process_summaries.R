library(tidyverse)
library(mvtraits)

all_summaries <- readRDS('results/summaries.rds')
try_data <- readRDS('traits_analysis.rds')

pft_levels <- list(jules1 = levels(try_data[['jules1']]),
                   jules2 = levels(try_data[['jules2']]),
                   clm45 = levels(try_data[['clm45']]),
                   custom = levels(try_data[['custom']])) %>%
  lapply(function(x) c('global', x))

pft_scheme_levels <- c('jules1', 'jules2', 'clm45', 'custom')
area_mass_levels <- c('area', 'mass')
model_type_levels <- c('multi', 'hier')

mass_params <- c('leaf_lifespan', 'SLA', 'Nmass', 'Pmass', 'Rdmass', 'Vcmax_mass', 'Jmax_mass')
area_params <- gsub('mass$', 'area', mass_params)

summaries_proc1 <- all_summaries %>%
  select(-fname) %>%
  rename(run_pft = pft) %>%
  mutate(
    run_pft = case_when(
      .$model_type == 'hier' ~ 'hier',
      .$model_type == 'multi' & .$run_pft == 'NA' ~ 'global',
      TRUE ~ .$run_pft
    ),
    pft_scheme = factor(pft_scheme, pft_scheme_levels),
    area_mass = factor(area_mass, area_mass_levels),
    model_type = factor(model_type, model_type_levels)
  )

cleanup_data <- function(dat, model_type, area_mass, pft_scheme, run_pft) {
  pft_lvl <- pft_levels[[pft_scheme]]
  dat %>%
    select(-rowname, -`Naive SE`, -`Time-series SE`) %>%
    mutate(
      group = factor(group) %>% 'levels<-'(pft_lvl)
    ) %>%
    rearrange_df(switch(area_mass, area = area_params, mass = mass_params)) %>%
    mutate(
      pft = if (model_type == "multi") factor(group, pft_lvl) else group
    ) %>%
    select(-group)
}

summaries_cleaned <- summaries_proc1 %>%
  mutate(data = pmap(
    list(
      dat = data,
      model_type = as.character(model_type),
      area_mass = as.character(area_mass),
      pft_scheme = as.character(pft_scheme),
      run_pft = as.character(run_pft)
    ),
    cleanup_data
  ))

saveRDS(summaries_cleaned, "results/summaries_processed.rds")
