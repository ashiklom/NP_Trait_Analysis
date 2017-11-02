#!/usr/bin/env Rscript
library(nptraits)

submit_tab_fname <- "submit_df.dat"
array_submit_fname <- "array_submit.sh"

try_data <- readRDS("extdata/traits_analysis.rds")

pft_classes <- try_data %>% select_if(is.factor) %>% colnames
model_types <- c("multi", "hier")
data_types <- c("mass", "area")

get_pfts <- function(pft_class) {
    try_data %>%
      select(one_of(pft_class)) %>%
      pull() %>%
      levels %>%
      c("", .)
}

submit_df <- expand.grid(pft_class = pft_classes, model_type = model_types, data_type = data_types,
                         stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(pft = map(pft_class, get_pfts)) %>%
    mutate(pft = case_when(.$model_type == "hier" ~ list(""), TRUE ~ .$pft)) %>%
    unnest() %>%
    select(model_type, data_type, pft_class, pft)

message("Writing submission arguments to: ", submit_tab_fname)
write.table(submit_df, file = submit_tab_fname, quote = FALSE,
            row.names = FALSE, col.names = FALSE)

########################################
# Write qsub submission script
########################################
nchains <- 4
script <- c("#!/bin/bash -l",
            "#$ -j y",
            "#$ -o logs/",
            '#$ -q "geo*"',
            "#$ -l mem_total=94G",
            "#$ -l h_rt=11:59:00",
            paste("#$ -pe omp", nchains),
            paste0("#$ -t 1-", nrow(submit_df)),
            paste("Rscript run_model.R",
                  "$(head -n ${SGE_TASK_ID} submit_df.dat ",
                  "| tail -n 1)")
            )
message("Writing array submission script to: ", array_submit_fname)
write(script, file = array_submit_fname)
Sys.chmod(array_submit_fname, "0744")
