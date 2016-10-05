stackedCorrPlot <- function() {

    library(mvtraits)
    library(RColorBrewer)
    library(grid)
    library(gridExtra)

    ##### Stacked Correlation Plot ################################

    h_summary <- readRDS("processed_output/summary.rds")[model_type == "hier"]
    cor.dat <- h_summary[var_type == "Omega_pft"][PFT != "global"]
    cor.global.dat <- h_summary[var_type == "Omega_global"]
    trait.pairs <- cor.dat[, unique(trait)]

    # playing with different colors
    Biome.colors <- brewer.pal(length(unique(cor.dat$Biome)) + 1, "Set1")  
    names(Biome.colors) <- c(unique(cor.dat$Biome), "Global")

    p <- list()

    trait_pairs_list <- strsplit(trait.pairs, split = "\\.")
    for(i in 1:length(trait.pairs)){
        t1 <- trait_pairs_list[[i]][1]
        t2 <- trait_pairs_list[[i]][2]
        dat <- filter(cor.dat, trait == trait.pairs[i])
        global.mean <- filter(cor.global.dat, trait == trait.pairs[i])$Mean

        p[[t2]][[t1]] <- ggplot() + 
            geom_density(data=dat, aes(x=Mean, y=..density.., fill=Biome),
                         position="stack") +
            geom_vline(xintercept = global.mean, size=1.2, color="black",
                    linetype = "4111") + 
            scale_colour_manual(values=Biome.colors)+ 
            scale_fill_manual(values=Biome.colors) +
            labs(title = trait.pairs[i]) + 
            xlim(-1,1) +
            geom_vline(xintercept = 0, size=.5, linetype = "longdash") + 
            global_theme + 
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "none")
    }

    return(p)
}

