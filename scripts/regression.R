library(tidyverse)

try_data <- readRDS('extdata/traits_analysis.rds')
pft_data <- readRDS('extdata/all_pfts.rds')

pft_sub <- pft_data %>%
  select(AccSpeciesID:climate_zone)

try_join <- try_data %>%
  left_join(pft_sub)

fit <- lm(log(SLA) ~ leaf_type * climate_zone * phenology * growth_form, data = try_join %>% mutate(species = factor(AccSpeciesID)))
fit_anova <- anova(fit)
fit_names <- rownames(fit_anova)
fit_ss <- fit_anova[['Sum Sq']]
fit_propvar <- fit_ss / sum(fit_ss)
names(fit_propvar) <- fit_names
sort(fit_propvar * 100, decreasing = TRUE)

fit <- lm(Jmax_area ~ climate_zone + n_fixation + growth_form + ps_pathway + phenology + leaf_type + AccSpeciesID, data = try_join)
