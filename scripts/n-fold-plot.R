library(shiklomanov2017np)
library(tidyverse)
library(here)

model_lvl <- c("uni", "multi_global", "multi_group", "hier")
cols <- cols(i = col_number(),
             model = col_factor(model_lvl),
             .default = col_number())
cv_results_mass <- here("results", "cv-results-mass.csv") %>%
  read_csv(col_types = cols)
cv_results_area <- here("results", "cv-results-area.csv") %>%
  read_csv(col_types = cols)
cv_results <- bind_rows("mass" = cv_results_mass, "area" = cv_results_area,
                        .id = "mass_area")

plt <- cv_results %>%
  gather(trait, value, -i, -model, -mass_area) %>%
  group_by(model, mass_area, trait) %>%
  summarize(M = mean(value), S = sd(value)) %>%
  ungroup() %>%
  mutate(trait = drop_mass_area(trait)) %>%
  ggplot() +
  aes(x = model, y = M) +
  geom_col() +
  facet_grid(vars(trait), vars(mass_area), scales = "free_y") +
  labs(y = "Mean RMSE", x = "Model") +
  cowplot::theme_cowplot() +
  theme(
    axis.text.x = element_text(angle = )
  )
if (interactive()) plt
