library(mvtraits)
try_data <- readRDS('traits_analysis.rds')
npfts <- max(as.integer(pull(try_data, pft)))

########################################
# Write qsub submission script
########################################

out_fname <- "submit.all.sh"
n.chains <- 5

bash_header <- "#!/bin/bash -l"

qsub_pattern <- "qsub -N %2$s -pe omp %3$d -v OMP_NUM_THREADS=%3$d run.rscript.sh 01.run.model.R %1$s %2$s n.chains=%3$d"

pft_numbers <- seq_len(npfts)
uni_models <- c("uni", sprintf("uni_%02d", pft_numbers))
multi_models <- c("multi", sprintf("multi_%02d", pft_numbers))

args <- commandArgs(trailingOnly = TRUE)
if(length(args) == 0) args <- c('area', "uni", "multi", "hier")
stopifnot(args[1] %in% c('mass', 'area'))

uni_string <- sprintf(qsub_pattern, args[1], uni_models, n.chains)
multi_string <- sprintf(qsub_pattern, args[1], multi_models, n.chains)
hier_string <- sprintf(qsub_pattern, args[1], "hier", n.chains)

out_file <- bash_header
if("uni" %in% args) out_file <- c(out_file, uni_string)
if("multi" %in% args) out_file <- c(out_file, multi_string)
if("hier" %in% args) out_file <- c(out_file, hier_string)

write(out_file, file = out_fname)

########################################
# Change permissions and run submission script
########################################

#system(paste("chmod +x", out_fname))
#system(paste0("./", out_fname))
#file.remove(out_fname)
