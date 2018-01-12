# Chunk name: trydata
# Description: Table of TRY data citations

library(shiklomanov2017np)
crtable <- readRDS("extdata/crossref_table.rds") %>%
  mutate(
    Reference = case_when(
      Reference == "@try_data_03" ~ "@baraloto_decoupled_2010",
      Reference == "@try_data_09" ~ "@wright_worldwide_2004",
      Reference == "@try_data_14" ~ "@kattge_2009_vcmax",
      Reference == "@try_data_15" ~ "@laughlin_multi-trait_2010",
      Reference == "@try_data_26" ~ "@onoda_2011_global",
      Reference == "@try_data_30" ~ "@messier_how_2010",
      Reference == "@try_data_33" ~ "@pierce_allocating_2013",
      Reference == "@try_data_41" ~ "@poorter_causes_2009",
      Reference == "@try_data_44" ~ "@choat_global_2012",
      Reference == "@try_data_59" ~ "@laughlin_climatic_2011",
      Reference == "@try_data_67" ~ "@wright_does_2012",
      Reference == "@try_data_80" ~ "@choat_global_2012",
      Reference == "@try_data_114" ~ "@kichenin_2013_contrasting",
      Reference == "@try_data_134" ~ "@diaz_plant_2004",
      TRUE ~ Reference
    )
  )
cap <- "Data sources and observation counts for foliar trait data"
kable(crtable, caption = cap)
