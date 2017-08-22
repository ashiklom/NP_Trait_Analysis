library(tidyverse)

all_summaries <- readRDS('results/summaries_processed.rds')

global_means <- all_summaries %>%
  filter(
    variable == 'mu',
    run_pft %in% c('hier', 'global'),
    pft == 'global'
  ) %>%
  select(model_type, area_mass, pft_scheme, param, Mean:`97.5%`)

ggplot(global_means) +
  aes(x = interaction(pft_scheme, model_type), y = Mean, ymin = `2.5%`, ymax = `97.5%`) +
  geom_pointrange(aes(color = pft_scheme, shape = model_type)) +
  facet_wrap(~ param + area_mass, scales = 'free', ncol = 2) +
  xlab('PFT scheme and model type') +
  ylab('Mean and 95% credible interval') +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
