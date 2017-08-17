library(tidyverse)

all_summaries <- readRDS('results/summaries.rds')

pft_scheme_levels <- c('jules1', 'jules2', 'clm45', 'custom')

global_means <- all_summaries %>%
  filter(
    variable == 'mu',
    group %in% c('global', 'FALSE'),
    !(model_type == 'multi' & pft != 'NA')
  ) %>%
  select(model_type, area_mass, pft_scheme, index, Mean, `2.5%`, `97.5%`) %>%
  mutate(pft_scheme = factor(pft_scheme, pft_scheme_levels))

ggplot(global_means) +
  aes(x = interaction(pft_scheme, model_type), y = Mean, ymin = `2.5%`, ymax = `97.5%`) +
  geom_pointrange(aes(color = pft_scheme, shape = model_type)) +
  facet_wrap(~ index + area_mass, scales = 'free', ncol = 2) +
  xlab('PFT scheme and model type') +
  ylab('Mean and 95% credible interval') +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
