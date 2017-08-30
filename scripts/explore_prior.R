library(mvtraits)
library(tidyverse)

try_data <- readRDS('extdata/traits_analysis.rds')

# try_data %>%
#   mutate_at(
#     vars(Jmax_area:Vcmax_mass),
#     ~log10(.)
#   ) %>%
#   summarize_at(
#     vars(Jmax_area:Vcmax_mass),
#     funs(mean = mean(., na.rm = TRUE),
#          var = var(., na.rm = TRUE))
#   ) %>%
#   gather() %>%
#   mutate(
#     vartype = gsub('.*(mean|var)$', '\\1', key),
#     param = gsub('_(mean|var)$', '', key)
#   ) %>%
#   select(-key) %>%
#   spread(vartype, value)

try_long <- try_data %>%
  select_if(is_double) %>%
  gather() %>%
  filter(!is.na(value), key != 'LMA') %>%
  mutate(value = log10(value))

source('informative_prior.R')

prior_long <- prior_df %>%
  mutate(samples = map2(mean, stdev, ~rnorm(5000, .x, .y))) %>%
  select(key = param, value = samples) %>%
  unnest()

plot_df <- bind_rows(try_long %>% mutate(type = 'data'),
                     prior_long %>% mutate(type = 'prior'))

ggplot(plot_df) +
  aes(x = key, y = value, fill = type) +
  geom_violin()
