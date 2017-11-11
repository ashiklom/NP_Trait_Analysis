library(tidyverse)
d <- readRDS("extdata/traits_analysis.rds")

d %>%
  filter(
    #clm45 == "broadleaf_deciduous_temperate",
    !is.na(Narea), !is.na(Vcmax_area)
  ) %>%
  ggplot() +
  aes(x = Narea, y = Vcmax_area) +
  geom_point() +
  geom_smooth(method = "lm")

