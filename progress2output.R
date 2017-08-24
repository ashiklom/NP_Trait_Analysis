# Code for exploring status of runs in progress
library(tidyverse)
progress_dir <- "progress"
output_dir <- "output"
output_progress_dir <- file.path(output_dir, "from_progress")
dir.create(output_progress_dir)
nfiles <- 20

submit_df <- read_delim('submit_df.dat', ' ', col_names = FALSE) %>% 
    setNames(c('model_type', 'area_mass', 'pft_scheme', 'pft'))

get_fname <- function(fname) {
    file_list <- list.files(output_dir, fname)
    nfiles <- as.character(length(file_list))
    switch(nfiles, `0` = NA_character_, `1` = file_list, "MANY")
}

find_files <- submit_df %>% 
    mutate(
        file_string = paste(model_type, area_mass, pft_scheme, pft, sep = '.'),
        file_name = map_chr(file_string, get_fname)
    )

missing_files <- find_files %>% 
    filter(is.na(file_name))
print(missing_files)

progress2output <- function(fname) {
    print(fname)
    all_files <- list.files(progress_dir, fname, full.names = TRUE)
    sub_files <- tail(all_files, nfiles)
    all_results <- Reduce(
        mvtraits:::combine_results,
        lapply(sub_files, readRDS)
    )
    out_fname <- file.path(output_progress_dir, paste0(fname, '.rds'))
    saveRDS(all_results, out_fname)
}

missing_files %>% 
    pull(file_string) %>% 
    walk(progress2output)
