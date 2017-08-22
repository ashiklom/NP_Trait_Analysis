library(tidyverse)

all_summaries <- readRDS('results/summaries_processed.rds')

## Compare variances in the global mean
all_summaries %>%
  filter(
    variable == 'Sigma',
    xparam == yparam,
    pft == 'global'
  ) %>%
  ggplot() +
  aes(x = pft_scheme, y = Mean, ymin = `2.5%`, ymax = `97.5%`) +
  geom_pointrange(mapping = aes(color = pft_scheme, shape = model_type)) +
  facet_wrap(~xparam + area_mass + model_type, scales = 'free')

# all_summaries %>%
#   filter(
#     variable == 'Sigma',
#     xparam == yparam,
#     pft != 'global'
#   ) %>%
#   ggplot() +
#   aes(x = )

calc_t_statistic <- function(dat) {
  dat %>%
    mutate(global_mean = .$Mean[.$pft == 'global'],
           t_stat = Mean / global_mean)
}

t_statistic <- all_summaries %>%
  filter(
    variable == 'Sigma',
    xparam == yparam,
    model_type == 'hier'
  ) %>%
  select(area_mass, pft_scheme, pft, param = xparam, Mean:`97.5%`) %>%
  group_by(area_mass, pft_scheme, param) %>%
  nest() %>%
  mutate(data = map(data, calc_t_statistic)) %>%
  unnest() %>%
  filter(pft != 'global') %>%
  mutate(param = gsub('area|mass', '', param))

ggplot(t_statistic) +
  aes(x = param, y = t_stat, fill = pft) +
  geom_col(position = 'dodge') +
  facet_grid(area_mass ~ pft_scheme, scales = 'free') +
  coord_cartesian(ylim = c(0, 2))
