library(shiklomanov2017np)

manuscript_fig_dir <- here("figures", "manuscript")
dir.create(manuscript_fig_dir, showWarnings = FALSE)

resultsfile <- here("results", "mvtraits_results.rds")
results_all <- readRDS(resultsfile)

results_hier <- results_all %>%
  filter(model_type == "hier")

rh_list <- map(results_hier[["data"]], "result")

means <- rh_list %>%
  pluck(2, "mu_group") %>%
  map(~pmap(..1, .f = tibble::tibble)) %>%
  map(bind_rows, .id = "trait") %>%
  bind_rows(.id = "pft")

library(ggplot2)
library(GGally, exclude = "nasa")

means_wide <- means %>%
  mutate(trait = forcats::fct_inorder(trait),
         pft = forcats::fct_inorder(pft)) %>%
  gather(stat, value, -pft, -trait) %>%
  mutate(stat = recode(stat, "2.5%" = "lo",
                       "Mean" = "mid",
                       "97.5%" = "hi") %>% factor(c("lo", "mid", "hi")),
         value = 10 ^ value,
         variable = interaction(trait, stat)) %>%
  select(-trait, -stat) %>%
  spread(variable, value)

errorbar_xy <- function(xtrait, ytrait) {
  x <- paste0(xtrait, ".mid")
  y <- paste0(ytrait, ".mid")
  xmin <- paste0(xtrait, ".lo")
  ymin <- paste0(ytrait, ".lo")
  xmax <- paste0(xtrait, ".hi")
  ymax <- paste0(ytrait, ".hi")
  ggplot(means_wide) +
    aes_string(x = x, y = y, xmin = xmin, xmax = xmax,
               ymin = ymin, ymax = ymax, color = "pft") +
    geom_point() +
    geom_errorbar() +
    geom_errorbarh() +
    xlab(xtrait) +
    ylab(ytrait) +
    scale_color_manual(values = pft_colors) +
    scale_x_log10() +
    scale_y_log10() +
    guides(color = FALSE) +
    theme_bw()
}

errorbar_xy("leaf_lifespan", "SLA")
errorbar_xy("leaf_lifespan", "Nmass")
errorbar_xy("Nmass", "Pmass")
errorbar_xy("Vcmax_mass", "Jmax_mass") +
  coord_cartesian(xlim = c(0.10, 3))

hmg <- results_hier %>%
  filter(model_type == "hier",
         mass_area == "mass") %>%
  pull(data) %>%
  pluck(1, "result", "Corr_global", "Mean")

hmg_eig <- eigen(hmg)
hmg_ev <- hmg_eig$vectors
rownames(hmg_ev) <- rownames(hmg)
hmg_ev

plot(hmg_ev[, 1], hmg_ev[, 2], type = "n", xlab = "EV1", ylab = "EV2")
arrows(0, 0, hmg_ev[, 1], hmg_ev[, 2])
text(x = hmg_ev[, 1], y = hmg_ev[, 2], labels = rownames(hmg_ev),
     offset = 1)

plot(hmg_ev[, 2], hmg_ev[, 3], type = "n", xlab = "EV2", ylab = "EV3")
arrows(0, 0, hmg_ev[, 2], hmg_ev[, 3])
text(x = hmg_ev[, 2], y = hmg_ev[, 3], labels = rownames(hmg_ev),
     offset = 2)


xx <- rh_list %>%
  pluck(2, "Sigma_global", list("Mean", )) %>%
  .["SLA", "Nmass"]

x <- rh_list %>%
  pluck(2, "Sigma_group") %>%
  map("Mean") %>%
  map(~.["SLA", "Nmass"]) %>%
  bind_rows() %>%
  gather("pft", "sigma")

ggplot(x) +
  aes(x = pft, y = sigma) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

rh_list
