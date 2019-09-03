library(shiklomanov2017np)
library(tidyverse)
library(pwr)
library(ggrepel)

out <- pwr.r.test(14, power = 0.95, sig.level = 0.05, alternative = "greater")
out

power_n <- function(n) {
  out <- numeric(length(n))
  for (i in seq_along(n)) {
    nn <- n[[i]]
    if (nn < 4) {
      out[[i]] <- 1
    } else {
      out[i] <- pwr.r.test(
        nn,
        power = 0.95,
        sig.level = 0.05,
        alternative = "greater"
      )[["r"]]
    }
  }
  out
}

pm_missing <- . %>%
  group_by(pft) %>%
  nest() %>%
  mutate(miss = map(data, get_all_pairwise_missing)) %>%
  unnest(miss)

dat_mass <- try_data("mass")
pm_mass <- dat_mass %>% pm_missing

dat_area <- try_data("area")
pm_area <- dat_area %>% pm_missing

lvl <- c("LL", "SLA", "N", "P", "Rd", "Vcmax", "Jmax")
pm_both <- bind_rows(mass = pm_mass, area = pm_area,
                     .id = "mass_area") %>%
  mutate(
    yvar = gsub("_?(mass|area)", "", yvar) %>%
      gsub("leaf_lifespan", "LL", .) %>%
      factor(lvl),
    xvar = gsub("_?(mass|area)", "", xvar) %>%
      gsub("leaf_lifespan", "LL", .) %>%
      factor(lvl),
    label = paste(yvar, xvar, sep = "--"),
    power = power_n(present)
  )

pm_plot <- pm_both %>%
  mutate(present_p = ifelse(present == 0, 1, present)) %>%
  group_by(pft, present_p) %>%
  mutate(j1 = n() > 6, j2 = !j1 & n() > 1)
plt_labelled <- ggplot() +
  aes(x = present_p, y = power, label = label) +
  stat_function(fun = power_n, data = pm_plot) +
  geom_jitter(width = 0, height = 0.3,
              data = filter(pm_plot, j1)) +
  geom_jitter(width = 0, height = 0.1,
              data = filter(pm_plot, j2)) +
  geom_point(data = filter(pm_plot, !j1)) +
  geom_text_repel(data = pm_plot, size = 2) +
  scale_x_log10(breaks = c(10, 100, 1000)) +
  facet_wrap(vars(mass_area, pft), ncol = 7)

plt_unlabelled <- ggplot() +
  aes(x = present_p, y = power) +
  stat_function(fun = power_n, data = pm_plot) +
  geom_jitter(width = 0, height = 0.3,
              data = filter(pm_plot, j1)) +
  geom_jitter(width = 0, height = 0.1,
              data = filter(pm_plot, j2)) +
  geom_point(data = filter(pm_plot, !j1)) +
  geom_point(aes(x = 14, y = power_n(14)), color = "red") +
  scale_x_log10(breaks = c(10, 100, 1000)) +
  facet_wrap(vars(pft), ncol = 7)
plt_unlabelled

plt_color <- ggplot() +
  aes(x = present_p, y = power, color = pft) +
  stat_function(fun = power_n, data = pm_plot) +
  geom_jitter(width = 0, height = 0.3,
              data = filter(pm_plot, j1)) +
  geom_jitter(width = 0, height = 0.1,
              data = filter(pm_plot, j2)) +
  geom_point(data = filter(pm_plot, !j1)) +
  ## geom_point(aes(x = 14, y = power_n(14)), color = "red") +
  scale_x_log10(breaks = c(10, 100, 1000)) +
  facet_grid(vars(yvar), vars(xvar)) +
  cowplot::theme_cowplot()
plt_color

figdir <- here::here("figures", "manuscript")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(figdir, "correlation-"))
