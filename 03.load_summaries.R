library(mvtraits)
library(tidyverse)

summaries_dir <- 'summaries'
results_dir <- 'results'
dir.create(results_dir, showWarnings = FALSE)

summary_files <- list.files(summaries_dir)
summary_file_pieces <- strsplit(summary_files, '\\.')
summary_file_dat <- do.call(rbind, summary_file_pieces) %>% 
    as_tibble() %>% 
    select(model_type = V1, area_mass = V2, pft_scheme = V3, pft = V4) %>% 
    mutate(fname = file.path(summaries_dir, summary_files))

summary_file_full <- summary_file_dat %>% 
    mutate(data = map(fname, readRDS))

fix_groups <- function(dat) {
    mutate(dat, group = as.character(group))
}

summary_file_fixed <- summary_file_full %>% 
    mutate(data = map(data, fix_groups))

saveRDS(summary_file_fixed, file = file.path(results_dir, 'summaries.rds'))
