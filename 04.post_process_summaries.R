library(tidyverse)
library(mvtraits)

all_summaries <- readRDS('results/summaries.rds')
try_data <- readRDS('traits_analysis.rds')

pft_levels <- list(jules1 = levels(try_data[['jules1']]),
                   jules2 = levels(try_data[['jules2']]),
                   clm45 = levels(try_data[['clm45']]),
                   custom = levels(try_data[['custom']])) %>%
  lapply(function(x) c('global', x))

mass_params <- c('leaf_lifespan', 'SLA', 'Nmass', 'Pmass', 'Rdmass', 'Vcmax_mass', 'Jmax_mass')
area_params <- gsub('mass$', 'area', mass_params)

summaries_proc1 <- all_summaries %>%
  select(-fname, -rowname, -`Naive SE`, -`Time-series SE`, -`25%`, -`75%`) %>%
  rename(run_pft = pft) %>%
  mutate(
    run_pft = case_when(.$model_type == 'hier' ~ 'hier',
                        .$model_type == 'multi' & .$run_pft == 'NA' ~ 'global',
                        TRUE ~ .$run_pft),
    group = case_when(!is.na(.$group) ~ .$group,
                    is.na(.$group) ~ .$run_pft)
  ) %>%
  group_by(model_type, area_mass, pft_scheme, run_pft) %>%
  nest()

process_df <- function(dat, area_mass, pft_scheme) {
  dat %>%
    mutate(
      group = factor(group) %>% 'levels<-'(pft_levels[[pft_scheme]])
    ) %>%
    rearrange_df(switch(area_mass, area = area_params, mass = mass_params)) %>%
    rename(pft = group)
}

summaries_proc2 <- summaries_proc1 %>%
  mutate(data = pmap(list(data, area_mass, pft_scheme), process_df))

summaries_proc3 <- unnest(summaries_proc2) %>%
  select(model_type:pft, param:yparam, Mean:`97.5%`)

saveRDS(summaries_proc3, 'results/summaries_processed.rds')
