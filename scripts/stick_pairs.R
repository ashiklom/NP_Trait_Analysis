library(mvtraits)
library(tidyverse)

#devtools::reload(devtools::inst("mvtraits"))

source("scripts/pft_abbreviations.R")

cachefile <- ".cache/mvtraits_results.rds"
results <- readRDS(cachefile)
results_sub <- results %>%
  filter((model_type == "multi" & pft == "global") |
         (model_type == "hier"))
dat_list <- results_sub$data
names(dat_list) <- with(results_sub, paste(model_type, mass_area, sep = "_"))

data_file <- "extdata/traits_analysis.rds"
try_data <- readRDS(data_file)

pfts <- levels(try_data$clm45)
npft <- length(pfts)

close.screen(all.screens = TRUE)

#tikzDevice::tikz("manuscript-tex/stick_pairsplot.tex", width = 5, height = 5)
pdf("figures/stick_pairsplot.pdf", width = 5, height = 5)
stickplot_pairs(
  mu_global_lower = dat_list$multi_mass$mu,
  Sigma_global_lower = dat_list$multi_mass$Sigma,
  mu_group_lower = dat_list$hier_mass$mu_group,
  Sigma_group_lower = dat_list$hier_mass$Sigma_group,
  vars_lower = mass_params,
  group_names = pft2abbr[pfts],
  vars_label = param_labels,
  unlog_axes = TRUE,
  mu_global_upper = dat_list$multi_area$mu,
  Sigma_global_upper = dat_list$multi_area$Sigma,
  mu_group_upper = dat_list$hier_area$mu_group,
  Sigma_group_upper = dat_list$hier_area$Sigma_group,
  vars_upper = area_params,
  col_group = pft_colors,
  cex_g = 1.2,
  lwd_g = 1,
  lty_g = 1,
  lwd_s = 1.3,
  par_plot = list(oma = c(0, 3, 3, 3), cex = 0.4),
  par_legplot = list(mar = c(0, 0, 0, 0)),
  par_legend = list(ncol = 5, x = "center", cex = 0.6)
)
dev.off()
