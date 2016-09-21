library(mvtraits)
library(RColorBrewer)
library(grid)
library(gridExtra)

##### Facet Grid Correlation Plot ##########################

h_summary <- readRDS("processed_output/summary.rds")[model_type == "hier"]
cov.dat <- h_summary[var_type == "Sigma_pft"][PFT != "global"]
cor.dat <- h_summary[var_type == "Omega_pft"][PFT != "global"]
cor.global.dat <- h_summary[var_type == "Omega_global"]
trait.pairs <- cor.dat[, unique(trait)]
#cov.dat[, lapply(.SD, function(x) {x[is.na(x)] <- 0; return(x)})]

cov.plt <- ggplot(cov.dat) + 
    aes(x=Function, y=q500, ymin=q025, ymax=q975, color=Function) +
    geom_pointrange() + 
    geom_hline(yintercept = 0) + 
    facet_grid(trait ~ Biome, scales="free") +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          legend.position = "bottom") +
    ylab("Covariance") + xlab("PFT")

mypng <- function(filename){
    png(filename, width=1000, height=700)
}

print("Generating covariance plot...")
mypng("figures/pft.cov.plot.png")
plot(cov.plt)
dev.off()

print("Generating correlation plot...")
mypng("figures/pft.cor.plot.png")
plot(cov.plt %+% cor.dat + ylab("Correlation"))
dev.off()

