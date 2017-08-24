library(mvtraits)
library(magrittr)
output_dir <- "output"
summary_dir <- "summaries"
dir.create(summary_dir, showWarnings = FALSE)
outputs <- list.files(output_dir, recursive = TRUE)
nfiles <- length(outputs)

for (out_file in outputs[-1:-6]) {
    message('Processing file: ', out_file)
    nth_file <- grep(out_file, outputs)
    message('File ', nth_file, ' of ', nfiles)
    
    fname <- file.path(output_dir, out_file)
    
    message('Reading file...')
    raw_output <- readRDS(fname)
    if (is.null(raw_output)) {
        message('File ', fname, ' has NULL output. Moving on.')
        next
    }
    
    model_type <- gsub('^(from_progress/)?(multi|hier).*', '\\2', out_file)
    is_hier <- model_type == 'hier'
    if (is_hier) {
        nparam <- ncol(raw_output[[1]]$mu_global)
        ngroups <- ncol(raw_output[[1]]$mu_group) / nparam
    } else {
        ngroups <- NULL
    }
    
    niter <- nrow(raw_output[[1]][[1]])
    nburn <- pmax(niter - 5000, floor(0.5 * niter))
    
    burn <- function(samps) lapply(samps, function(x) x[nburn:niter,])
    raw_burned <- lapply(raw_output, burn)
    rm(raw_output); gc()
    
    # This step is slow!
    message('Adding correlations...')
    raw_withcorr <- add_correlations(raw_burned,
                                     hier = is_hier,
                                     ngroups = ngroups)
    
    message('Converting to MCMC list...')
    raw_mcmc <- results2mcmclist(raw_withcorr, model_type)
    rm(raw_burned, raw_withcorr); gc()
    
    message('Calculating summary...')
    out_summary <- summary_df(raw_mcmc, is_hier)
    saveRDS(out_summary, file.path(summary_dir, basename(out_file)))
    rm(raw_mcmc); gc()
}
