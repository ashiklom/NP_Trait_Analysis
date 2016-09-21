library(mvtraits)
library(tibble)
library(RColorBrewer)

all_summary <- readRDS("processed_output/summary.rds")
cor.dat <- all_summary[model_type == "hier"][var_type == "Omega_pft"]

trait.pairs <- unique(cor.dat[, trait])
cor.list <- list()
rnames <- c("Biome", "ps_type", "growth_form", 
            "leaf_type", "phenology", "Residuals")
for(trt in trait.pairs){
    cor.list[[trt]] <- lm(Mean ~ Biome + ps_type + growth_form + ps_type + leaf_type + phenology,
                cor.dat[trait == trt]) %>% 
                anova %>% dplyr::select(matches("Sum Sq"))
}

cor.anova <- do.call(cbind, cor.list)
colnames(cor.anova) <- trait.pairs
cor.plot.dat <- cor.anova %>% rownames_to_column(var = "Type") %>%
    gather(Trait, Value, -Type) %>%
    setDT
cor.plot.dat[, scaledValue := Value / sum(Value), by=Trait]

## Compute total variance by type
tot.var.table <- cor.plot.dat[, list(tot.var = sum(Value)), by=Type]
tot.var.table[, pct.var := 100 * tot.var / sum(tot.var)]
sink("figures/tot.var.table.txt")
print(tot.var.table[order(tot.var, decreasing=TRUE)], 
      digits = 3)
sink()

Type.colors <- c(brewer.pal(5, "Spectral"), "grey")
names(Type.colors) <- rnames
  
cor.anova.plot <- ggplot(cor.plot.dat) + 
  aes(x = Trait, fill = Type) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = Type.colors) +
  theme_bw() + 
  theme(text = element_text(size = 24),
        axis.title.y = element_text(size = rel(1), 
                                    margin = margin(0, 10, 0, 0)),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(1), 
                                   angle = 90, 
                                   vjust = 0.5,
                                   hjust = 1, 
                                   margin = margin(10, 0, 0, 0)),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key.height = unit(2, "line"))


mypng("figures/pft.cor.anova.scaled.png")
plot(cor.anova.plot + aes(y = scaledValue) +
     ylab("Scaled sum of squares"))
dev.off()

mypng("figures/pft.cor.anova.png")
plot(cor.anova.plot + aes(y = Value) + 
     ylab("Sum of squares"))
dev.off()
