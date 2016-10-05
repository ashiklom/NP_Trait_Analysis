library(mvtraits)
library(RColorBrewer)
library(grid)
library(gridExtra)

source("eigen.ellipses.R")
source("stacked_corr_biome.R")

# Global Theme for plots
global_theme <- theme_bw() + 
    theme(text = element_text(size = 25),
          title = element_blank(),
          axis.text = element_text(size = rel(0.3)),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(0.6)),
          legend.direction = "horizontal"
          )

pe <- eigenEllipses("hier", "Sigma_pft", "mu_pft")
pd <- stackedCorrPlot()

png("/dev/null")
r <- textGrob("")
txt <- lapply(traits_nolog, textGrob,
              gp = gpar(cex = 1.5))
names(txt) <- traits_nolog
plt <- arrangeGrob(
        txt$LL, pd$LL$LMA, pd$LL$Nmass, pd$LL$Pmass, pd$LL$Rdmass, 
        pe$LL$LMA, txt$LMA, pd$LMA$Nmass, pd$LMA$Pmass, pd$LMA$Rdmass,
        pe$LL$Nmass, pe$LMA$Nmass, txt$Nmass, pd$Nmass$Pmass, pd$Nmass$Rdmass,
        pe$LL$Pmass, pe$LMA$Pmass, pe$Nmass$Pmass, txt$Pmass, pd$Pmass$Rdmass,
        pe$LL$Rdmass, pe$LMA$Rdmass, pe$Nmass$Rdmass, pe$Pmass$Rdmass, txt$Rdmass,
                 ncol = 5,
                 heights = c(1, 1, 1, 1, 1),
                 widths = c(1, 1, 1, 1, 1))
legplot <- pd$LL$LMA + global_theme + theme(legend.position = "right")
g <- ggplotGrob(legplot)$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
lwidth <- sum(legend$width)
dev.off()
png(filename = sprintf("figures/hier_corr.png"), 
    height = 7, width=9, units = "in", res = 300)
grid.newpage() 
grid.draw(arrangeGrob(plt, legend, nrow = 2,
                      heights = unit.c(unit(1, "npc") - lheight,
                                       lheight)))
dev.off()
