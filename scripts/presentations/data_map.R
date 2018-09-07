library(tidyverse)

try_dat <- readRDS("extdata/traits_analysis.rds")

miss <- function(x) {
  as.integer(is.na(x))
}

map_dat <- try_dat %>%
  mutate(
    nonmissing_mass = miss(leaf_lifespan) + miss(SLA) + miss(Nmass) +
      miss(Pmass) + miss(Rdmass) + miss(Vcmax_mass) + miss(Jmax_mass),
    nonmissing_area = miss(leaf_lifespan) + miss(SLA) + miss(Narea) +
      miss(Parea) + miss(Rdarea) + miss(Vcmax_area) + miss(Jmax_area)
  ) %>%
  group_by(Latitude, Longitude) %>%
  summarize_at(vars(one_of("nonmissing_mass", "nonmissing_area")), sum) %>%
  filter(nonmissing_mass > 0, nonmissing_area > 0)

ggplot(map_dat) +
  aes(x = Longitude, y = Latitude) +
  borders("world", colour = "black") +
  geom_point(aes(color = nonmissing_mass)) +
  scale_color_gradient(low = "azure1", high = "red4", trans = "log10") +
  guides(color = guide_colorbar(title = "Sample size")) +
  theme_bw()
ggsave("figures/data_map.png", width = 10, height = 6, units = "in", dpi = 300)
