library(shiklomanov2017np)
library(tidyverse)
library(here)
library(cowplot)

model_lvl <- c("uni", "multi_global", "multi_group", "hier")
cols <- cols(i = col_number(),
             model = col_factor(model_lvl),
             .default = col_number())
cv_results_mass <- here("results", "cv-results-mass.csv") %>%
  read_csv(col_types = cols) %>%
  rename_all(~gsub("_?mass", "", .))
cv_results_area <- here("results", "cv-results-area.csv") %>%
  read_csv(col_types = cols) %>%
  rename_all(~gsub("_?area", "", .))
cv_results <- bind_rows("mass" = cv_results_mass, "area" = cv_results_area,
                        .id = "mass_area")

cv_long <- cv_results %>%
  gather(trait, value, -i, -model, -mass_area) %>%
  group_by(model, mass_area, trait) %>%
  summarize(M = mean(value), S = sd(value)) %>%
  ungroup() %>%
  mutate(trait = drop_mass_area(trait),
         mass_area = factor(mass_area, c("mass", "area"))) %>%
  arrange(mass_area, trait)

plt <- cv_long %>%
  group_by(mass_area, trait) %>%
  mutate(Mnorm = M / max(M)) %>%
  ggplot() +
  aes(x = model, y = Mnorm, group = 1) +
  geom_point() +
  geom_line() +
  facet_grid(vars(trait), vars(mass_area), scales = "free_y") +
  labs(y = "Normalized mean RMSE", x = "Model") +
  theme_cowplot()
if (interactive()) plt

figdir <- here("figures", "manuscript")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(figdir, "n-fold-rmse.pdf"), plt,
       width = 8.78, height = 8.54)

## # GGmatrix
## plt_l <- cv_long %>%
##   group_by(mass_area, trait) %>%
##   nest() %>%
##   mutate(
##     plt = map(
##       data,
##       ~ggplot(.x) + aes(x = model, y = M, group = 1) + geom_point() + geom_line()
##     ))
## plt <- GGally::ggmatrix(
##   plt_l$plt,
##   nrow = 7,
##   ncol = 2,
##   byrow = FALSE,
##   xAxisLabels = unique(plt_l$mass_area),
##   yAxisLabels = levels(plt_l$trait)
## )
## plt
