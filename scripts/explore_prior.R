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
  select(-Latitude, -Longitude, -LMA) %>%
  select_if(is_double) %>%
  gather() %>%
  filter(!is.na(value)) %>%
  mutate(value = log10(value))

source('scripts/informative_prior.R')
prior_long <- prior_df %>%
  mutate(samples = map2(mean, stdev, ~rnorm(5000, .x, .y))) %>%
  select(key = param, value = samples) %>%
  unnest()
plot_df <- bind_rows(try_long %>% mutate(type = 'data'),
                     prior_long %>% mutate(type = 'prior'))
ggplot(plot_df) +
  aes(x = key, y = 10^value, fill = type) +
  geom_boxplot() +
  facet_wrap(~key, scales = "free")

##################################################
# Plot the data (log-transformed) and prior +/- 1.96 SD (95% CI)
library(tidyverse)

try_data <- readRDS('extdata/traits_analysis.rds')

# toggle this manually to control which plot we draw
data_type <- "area"
area_rxp <- 'leaf_lifespan|SLA|area'
mass_rxp <- 'leaf_lifespan|SLA|mass'
use_rxp <- switch(data_type, area = area_rxp, mass = mass_rxp)
data_df <- try_data %>%
    dplyr::select(clm45, matches(use_rxp)) %>%
    dplyr::filter_at(dplyr::vars(matches(use_rxp)), dplyr::any_vars(!is.na(.)))
data_df %>%
  gather(trait, value, -clm45) %>%
  filter(!is.na(value)) %>%
  mutate(value = log10(value)) %>%
  left_join(prior_df, c("trait" = "param")) %>%
  ggplot() +
  aes(x = clm45, y = value) +
  geom_jitter(color = "gray80", size = 0.2) +
  geom_hline(aes(yintercept = mean)) +
  geom_hline(aes(yintercept = mean + 1.96 * stdev), linetype = "dashed") +
  geom_hline(aes(yintercept = mean - 1.96 * stdev), linetype = "dashed") +
  facet_wrap(vars(trait), scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
