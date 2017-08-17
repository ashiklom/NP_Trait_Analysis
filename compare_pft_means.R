library(tidyverse)

all_summaries <- readRDS('results/summaries_processed.rds')

pft_means <- all_summaries %>%
  filter(
    variable == 'mu',
    pft != 'global'
  ) %>%
  select(model_type, area_mass, pft_scheme, pft, param, Mean:`97.5%`)

ggplot(pft_means) +
  aes(x = interaction(pft_scheme, model_type, pft), y = Mean, ymin = `2.5%`, ymax = `97.5%`,
      color = pft_scheme, shape = model_type) +
  geom_pointrange() +
  facet_wrap(~ param + area_mass, scales = 'free') +
  xlab('PFT, PFT_scheme, and model type') +
  ylab('Mean and 95% credible interval') +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
