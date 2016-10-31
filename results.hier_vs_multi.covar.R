library(mvtraits)

all_summary <- readRDS("processed_output/summary.rds")

cols <- colorRampPalette(c("red", "orange", "grey50", "grey50",
                           "skyblue", "blue"))(10)
cex <- 2.5


graphic("figures/cor.global.multi")
par(cex = cex)
plot_covar(all_summary[model_type == "multi" &
                      var_type == "Omega" &
                      PFT == "global"], 
           main="Multivariate", 
           colorder = traits_nolog,
           col = cols)
dev.off()

graphic("figures/cor.global.hierarchical")
par(cex = cex)
plot_covar(all_summary[model_type == "hier" & 
                      var_type == "Omega_global"], 
           main="Hierarchical", 
           colorder = traits_nolog,
           col = cols)
dev.off()

system2("gm", args = c("montage", "-tile 2x1", "-geometry 100%",
                       "figures/cor.global.multi.tiff",
                       "figures/cor.global.hierarchical.tiff",
                       "figures/fig2.tiff"))

