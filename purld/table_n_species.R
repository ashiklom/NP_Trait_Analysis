# Chunk name: "pfts"
# Description: Table of number of species in each PFT
library(shiklomanov2017np)
try_data <- readRDS("extdata/traits_analysis.rds")

## ----"pfts"--------------------------------------------------------------
n_species <- try_data %>%
  distinct(clm45, AccSpeciesID) %>%
  select(pft = clm45, species = AccSpeciesID) %>%
  count(pft) %>%
  mutate(
    pft = as.character(pft),
    label = pft2abbr[pft],
    pft = tools::toTitleCase(gsub("_", " ", pft))
  ) %>%
  select(`Label` = label, `PFT` = pft, `Number of species` = n)
species_caption <- paste(
  "Names, labels, and species counts for plant functional types (PFTs)",
  "used in this analysis."
)
kable(n_species, caption = species_caption, align = rep("c", ncol(n_species)))
