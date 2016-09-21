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
Biome.colors <- brewer.pal(length(unique(cor.dat$Biome))+1,"Set1")  
names(Biome.colors) <- c(unique(cor.dat$Biome), "Global")

ps_type.colors <- brewer.pal(length(unique(cor.dat$ps_type)),"Set2")  
names(ps_type.colors) <- unique(cor.dat$ps_type)
leaf_type.colors <- brewer.pal(length(unique(cor.dat$leaf_type)),"Set3")  
names(leaf_type.colors) <- unique(cor.dat$leaf_type)
growth_form.colors <- brewer.pal(length(unique(cor.dat$growth_form)),"Dark2")  
names(growth_form.colors) <- unique(cor.dat$growth_form)
Function.colors <- brewer.pal(length(unique(cor.dat$Function)),"Accent")  
names(Function.colors) <- unique(cor.dat$Function)

global_theme <- theme_bw() + 
    theme(text = element_text(size = 25),
          title = element_blank(),
          axis.text = element_text(size = rel(0.3)),
          legend.title = element_text(size = rel(1)),
          legend.text = element_text(size = rel(0.8)),
          legend.direction = "horizontal",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
          )

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
      geom_vline(xintercept = global.mean, size=1.5, color="black",
                 linetype = "dotdash") + 
      scale_colour_manual(values=Biome.colors)+ 
      scale_fill_manual(values=Biome.colors) +
      labs(title = trait.pairs[i]) + 
      xlim(-1,1) +
      geom_vline(xintercept = 0, size=.5, linetype = "longdash") + 
      global_theme + 
      theme(legend.position = "none")
}

png("/dev/null")
r <- textGrob("")
txt <- lapply(traits_nolog, textGrob,
              gp = gpar(cex = 1.5))
names(txt) <- traits_nolog
p <- arrangeGrob(
        txt$LL, r, r, r, r, 
        p$LL$LMA, txt$LMA, r, r, r,
        p$LL$Nmass, p$LMA$Nmass, txt$Nmass, r, r,
        p$LL$Pmass, p$LMA$Pmass, p$Nmass$Pmass, txt$Pmass, r,
        p$LL$Rdmass, p$LMA$Rdmass, p$Nmass$Rdmass, p$Pmass$Rdmass, txt$Rdmass,
                 ncol = 5,
                 heights = c(0.15, 1, 1, 1, 1),
                 widths = c(1, 1, 1, 1, 0.5))
g <- ggplotGrob(p1 + global_theme + theme(legend.position="right"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
lwidth <- sum(legend$width)
dev.off()
png(filename = sprintf("figures/stacked.cor.biome.png"), 
    height = 7, width=9, units = "in", res = 300)
grid.newpage() 
grid.draw(arrangeGrob(p, legend, nrow = 2,
                      heights = unit.c(unit(1, "npc") - lheight,
                                       lheight)))
dev.off()
