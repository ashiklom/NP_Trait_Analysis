library(mvtraits)
library(RColorBrewer)
library(grid)
library(gridExtra)

source("eigen.ellipses.R")
source("stacked_corr_biome.R")

figdir <- '~/Projects/new-phytologist-traits/agu_presentation/'

# Global Theme for plots
global_theme <- theme_bw() + 
    theme(text = element_text(size = 75),
          title = element_blank(),
          axis.text = element_text(size = rel(0.3)),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(0.6)),
          legend.direction = "horizontal"
          )

pe <- eigenEllipses("hier", "Sigma_pft", "mu_pft", line_size = 1, 
                    point_size = 3, center_point_size = 3)

pe$LMA$Nmass + coord_cartesian(xlim = c(-4.5, 0), ylim = c(2, 3.5))
ggsave(file.path(figdir, 'ellipse.LMA_Nmass.png'))

pe$LL$LMA + coord_cartesian(xlim = c(0.5, 4), ylim = c(-4, 0))
ggsave(file.path(figdir, 'ellipse.LL_LMA.png'))

pe$Nmass$Rdmass + coord_cartesian(xlim = c(2, 3.7), ylim = c(-6, -3))
ggsave(file.path(figdir, 'ellipse.Nmass_Rdmass.png'))

pe$Nmass$Pmass + coord_cartesian(xlim = c(2, 3.7), ylim = c(-1, 1.5))
ggsave(file.path(figdir, 'ellipse.Nmass_Pmass.png'))


global_theme <- theme_bw() + 
    theme(text = element_text(size = 25),
          title = element_blank(),
          axis.text = element_text(size = rel(0.3)),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(0.6)),
          legend.direction = "horizontal"
          )
pe2 <- eigenEllipses('hier', 'Sigma_pft', 'mu_pft')
blank <- rectGrob(gp = gpar(col = NA))
png(file.path(figdir, 'ellipse.all.png'), width = 7.6, height = 7, units = 'in', 
    res = 300)
grid.arrange(
    pe2$LL$LMA, blank, blank, blank,
    pe2$LL$Nmass, pe2$LMA$Nmass, blank, blank,
    pe2$LL$Pmass, pe2$LMA$Pmass, pe2$Nmass$Pmass, blank,
    pe2$LL$Rdmass, pe2$LMA$Rdmass, pe2$Nmass$Rdmass, pe2$Pmass$Rdmass,
    ncol = 4)
dev.off()


## Stacked correlation plots
pd <- stackedCorrPlot()

pd$Nmass$Pmass
ggsave(file.path(figdir, 'corrdens.Nmass_Pmass.png'))

pd$LMA$Nmass
ggsave(file.path(figdir, 'corrdens.LMA_Nmass.png'))

pd$LL$LMA
ggsave(file.path(figdir, 'corrdens.LL_LMA.png'))


