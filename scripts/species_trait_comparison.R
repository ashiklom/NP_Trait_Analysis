library(tidyverse)

spp_file <- "../preprocess-try/pfts_species/tps_species.rds"
data_file <- "extdata/trait_data.rds"

try_spp <- readRDS(spp_file)
try_data <- readRDS(data_file)

if (FALSE) {

  try_spp %>%
    filter(AccSpeciesName == "Acer saccharum")

}

early_hardwood <- c(
  "Acer barbatum",
  "Acer grandidentatum",
  "Acer leucoderme",
  "Acer negundo",
  "Acer nigrum",
  "Acer pensylvanicum",
  "Acer platanoides",
  "Acer rubrum",
  "Acer saccharinum",
  "Acer saccharum",
  "Acer spicatum",
  "Aesculus flava",
  "Alnus glutinosa",
  "Alnus rhombifolia",
  "Calycanthus floridus",
  "Carpinus caroliniana",
  "Cornus florida",
  "Diospyros texana",
  "Diospyros virginiana",
  "Fagus grandifolia",
  "Halesia carolina",
  "Halesia parviflora",
  "Ilex montana",
  "Oxydendrum arboreum",
  "Platanus occidentalis",
  "Symplocos tinctoria",
  "Tilia americana",
  "Fagus sylvatica"
)
sp_ids <- try_spp %>%
  filter(AccSpeciesName %in% early_hardwood) %>%
  select(species = AccSpeciesName, AccSpeciesID)

sp_compare <- sp_ids %>%
  inner_join(try_data) %>%
  mutate(
    species = factor(species) %>%
      forcats::fct_reorder(SLA, median, na.rm = TRUE)
  )

sp_means <- sp_compare %>%
  group_by(species) %>%
  summarize(SLA = median(SLA, na.rm = TRUE))

ggplot(sp_means) +
  aes(x = species, y = SLA) +
  geom_col()

ggplot(sp_compare) +
  aes(x = species) +
  geom_boxplot(aes(y = SLA))

