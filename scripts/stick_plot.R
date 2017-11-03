library(mvtraits)
devtools::reload(devtools::inst("mvtraits"))

source("scripts/pft_abbreviations.R")

cachefile <- ".cache/mvtraits_results.rds"
dat_list <- readRDS(cachefile)

data_file <- "extdata/traits_analysis.rds"
try_data <- readRDS(data_file)
pfts <- levels(try_data$clm45)
npft <- length(pfts)


close.screen(all.screens = TRUE)
stickplot_pairs(
  mu_global_lower = dat_list$mass_global$mu,
  Sigma_global_lower = dat_list$mass_global$Sigma,
  mu_group_lower = dat_list$mass_group$mu_group,
  Sigma_group_lower = dat_list$mass_group$Sigma_group,
  vars_lower = mass_params,
  group_names = pfts,
  vars_label = param_labels,
  unlog_axes = TRUE,
  mu_global_upper = dat_list$area_global$mu,
  Sigma_global_upper = dat_list$area_global$Sigma,
  mu_group_upper = dat_list$area_group$mu_group,
  Sigma_group_upper = dat_list$area_group$Sigma_group,
  vars_upper = area_params,
  col_group = pft_colors,
  par_plot = list(oma = c(0, 3, 3, 3)),
  par_legplot = list(mar = c(0, 0, 0, 0)),
  par_legend = list(ncol = 5, x = "right")
)

my_stick <- function(mass_area, xvar, yvar, hide_insignificant = TRUE,
                     xlab = xvar,
                     ylab = yvar,
                     par_plot = list(cex = 1.5,
                                     mar = c(4, 4, 1, 1)),
                     par_legend = modifyList(par_plot,
                                             list(mar = c(4, 0, 1, 0)))) {
  cols <- RColorBrewer::brewer.pal(npft, "Paired")
  col_g <- "black"
  dat_global <- dat_list[[paste(mass_area, "global", sep = "_")]]
  dat_group <- dat_list[[paste(mass_area, "group", sep = "_")]]
  draw_sticks(
      dat_global$mu, dat_global$Sigma,
      dat_group$mu_group, dat_group$Sigma_group,
      xvar, yvar, pft2abbr[pfts],
      hide_insignificant = hide_insignificant,
      par_plot = par_plot, par_legend = par_legend,
      cols = cols, col_g = col_g,
      xlab = xlab, ylab = ylab
  )
}

#draw_png <- function(mass_area, xvar, yvar, ...) {
  #filename <- sprintf("figures/mvtraits_stick_%s_%s_%s.png", 
                      #mass_area, xvar, yvar)
  #png(filename, width = 6.5, height = 4, units = "in", res = 300)
  #my_stick(mass_area, xvar, yvar, ...)
  #dev.off()
#}


draw_tikz <- function(mass_area, xvar, yvar, ...) {
  filename <- sprintf("manuscript-tex/stick_%s_%s_%s.tex",
                      mass_area, xvar, yvar)
  tikzDevice::tikz(filename, width = 5, height = 3)
  my_stick(mass_area, xvar, yvar, ...)
  dev.off()
}

parp <- list(cex = 0.8, cex.lab = 1.25, cex.axis = 0.75,
             mar = c(4.5, 4.5, 1, 0.4),
             bg = "white", fg = "black")
parl <- modifyList(parp, list(mar = c(3, 0, 1, 0), cex = 0.9))

p1 <- my_stick("mass", "Nmass", "Pmass", par_plot = parp, par_legend = parl)

vertex <- function(mass_area, xvar, yvar) {
  p <- my_stick(mass_area, xvar, yvar, par_plot = parp, par_legend = parl)
  par(new = TRUE)
  xlim <- c(0.1, 3)
  ylim <- c(0.1, 30)
  plot(0, 0, type = "n", xlim = log10(xlim), ylim = log10(ylim), ann = FALSE, axes = FALSE)
  p$sticks()
  p$draw_axes(list(xlab = "", ylab = ""))
}

layout(matrix(c(1, 2, 5, 3, 4, 5), 3, 2))
vertex("mass", "Nmass", "Pmass")
#p1$plot()
#p1$plot()
#p1$plot()
#p1$legend()

## Consistent
#draw_tikz("mass", "Nmass", "Pmass", par_plot = parp, par_legend = parl,
          #xlab = Nmass_label, ylab = Pmass_label)

#draw_tikz("mass", "SLA", "leaf_lifespan", par_plot = parp, par_legend = parl,
          #xlab = SLA_label, ylab = "Leaf lifespan (months)")

## Not consistent
#draw_tikz("mass", "Jmax_mass", "Vcmax_mass", par_plot = parp, par_legend = parl,
          #xlab = Jmax_label, ylab = Vcmax_label)

#draw_tikz("mass", "Nmass", "Vcmax_mass", par_plot = parp, par_legend = parl,
          #xlab = Nmass_label, ylab = Vcmax_label)

#draw_tikz("area", "Narea", "Vcmax_area", par_plot = parp, par_legend = parl)

#try_data %>%
  #dplyr::filter(
    #clm45 == "broadleaf_deciduous_temperate",
    #!is.na(Narea), !is.na(Vcmax_area),
    #Vcmax_area < 2
  #) %>%
  #dplyr::select(Narea, Vcmax_area) %>%
  #ggplot2::qplot(x = Narea, y = Vcmax_area, data = .)
