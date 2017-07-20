library(mvtraits)
library(tidyverse)
library(glue)
library(magrittr)
try_data <- readRDS('traits_analysis.rds')

pft_classes <- try_data %>% select_if(is.factor) %>% colnames
model_types <- c('multi', 'hier')
data_types <- c('mass', 'area')
pfts <- map(pft_classes, get_pfts)

get_pfts <- function(pft_class) {
    try_data %>% select(one_of(pft_class)) %>% pull %>% levels %>% c('', .)
}

submit_df <- expand.grid(pft_class = pft_classes, model_type = model_types, data_type = data_types,
                         stringsAsFactors = FALSE) %>% 
    as_tibble() %>% 
    mutate(pft = map(pft_class, get_pfts)) %>% 
    mutate(pft = case_when(.$model_type == 'hier' ~ list(''), TRUE ~ .$pft)) %>% 
    unnest() %>% 
    mutate(run_name = paste(model_type, data_type, pft_class, pft, sep = '.'))

########################################
# Write qsub submission script
########################################

out_fname <- "submit_all.sh"
nchains <- 4

bash_header <- "#!/bin/bash -l"

glue_string <- paste("qsub -N {run_name}",
                     "-pe omp {nchains} -v OMP_NUM_THREADS={nchains}", 
                     "run.rscript.sh 01.run.model.R {model_type} {data_type} {pft_class} {pft}")

submit_string <- c(bash_header, glue_data(submit_df, glue_string))

write(submit_string, file = out_fname)
