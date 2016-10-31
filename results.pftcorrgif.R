library(mvtraits)

# Get the covariance matrices for each PFT
outdir <- "figures/pft.corrs"
all_summary <- readRDS("processed_output/summary.rds")
cor_all <- all_summary[model_type == "hier"][var_type == "Omega_pft"]
dir.create(outdir, showWarnings = FALSE)

for(i in 1:npft){
    pft_name <- pft.names[i]
    print(paste(i, pft_name))
    cor_pft <- cor_all[PFT == pft_name]
    png(sprintf("figures/pft.corrs/%02d.%s.tiff", 
                i, gsub("/","-",pft_name)))
    plot_covar(cor_pft, colorder = traits_nolog, main = pft_name)
    dev.off()
}

print("Creating GIF using system command 'gm'...")
system("gm convert -delay 70 figures/pft.corrs/*.tiff figures/pft.corrs.gif")
print("Done!")
