## LOAD DATA AND PACKAGES #############################################################
library(mvtraits)
library(tidyverse)

message("Current working directory: ", getwd())
print(sessionInfo())

try_data <- readRDS('extdata/traits_analysis.rds')

# For testing
if (FALSE) {
    # For testing
    cmdargs <- c("hier", "mass", "clm45")
    cmdargs <- c("hier", "area", "clm45")
    cmdargs <- c("multi", "mass", "clm45")
    cmdargs <- c("multi", "area", "clm45")
    cmdargs <- c('multi', 'area', 'clm45', 'shrub_evergreen')
    cmdargs <- c('multi', 'mass', 'jules1', 'c4_grass')
    cmdargs <- c('hier', 'mass', 'jules2')
    cmdargs <- c('multi', 'area', 'jules1')
}

out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE)

if (!exists("cmdargs")) cmdargs <- commandArgs(trailingOnly = TRUE)
message("Running with the following arguments:")
print(cmdargs)
# Arguments:
#   1. Model type. Must be either 'multi' or 'hier'
#   2. Data type. Must be either 'area' or 'mass'
#   3. PFT type. Must be one of the PFT types in the TRY data.
#   4. PFT. If 'hier', this argument must be empty. If multi and empty, fit to
#   all TRY data. If multi and present, fit to subset of TRY data corresponding
#   to provided PFT.
stopifnot(length(cmdargs) >= 3)
model_type <- cmdargs[1]
stopifnot(model_type %in% c('multi', 'hier'))
message('Selected model type: ', model_type)

data_type <- cmdargs[2]
stopifnot(data_type %in% c('area', 'mass'))
message('Selected data type: ', data_type)

pft_type <- cmdargs[3]
valid_pft_types <- try_data %>% select_if(is.factor) %>% colnames
stopifnot(pft_type %in% valid_pft_types)
message('Selected PFT scheme: ', pft_type)

pft <- cmdargs[4]
if (model_type == 'hier' && !is.na(pft)) stop('Cannot specify PFT for hierarchical model')
if (is.na(pft)) {
    message('PFT is NA. Running for all PFTs.')
} else {
    valid_pfts <- try_data %>% select(one_of(pft_type)) %>% distinct %>% pull %>% levels
    stopifnot(pft %in% valid_pfts)
    message('Selected PFT: ', pft)
}

area_rxp <- 'leaf_lifespan|SLA|area'
mass_rxp <- 'leaf_lifespan|SLA|mass'
use_rxp <- switch(data_type, area = area_rxp, mass = mass_rxp)

data_df_all <- try_data %>%
    dplyr::select(one_of(pft_type), matches(use_rxp)) %>%
    dplyr::filter_at(dplyr::vars(matches(use_rxp)), dplyr::any_vars(!is.na(.)))

pft_type_q <- rlang::sym(pft_type)

if (!is.na(pft)) {
    data_df <- filter(data_df_all, UQ(pft_type_q) == pft)
} else {
    data_df <- data_df_all
}
stopifnot(nrow(data_df) > 0)

date_format <- strftime(Sys.time(), '%Y-%m-%d-%H%M')
file_tag <- paste(model_type, data_type, pft_type, pft, date_format, 'rds', sep = '.')

data_mat <- data_df %>% dplyr::select(-!!pft_type_q) %>% as.matrix() %>% log10()
message('Data contains ', nrow(data_mat), ' rows and ', ncol(data_mat), ' columns')

niter <- 2500
nchains <- 4
parallel <- TRUE
autofit <- TRUE
max_attempts <- 50
keep_samples <- 20000
save_progress <- NULL
threshold <- 1.3
# save_progress <- file.path(progress_dir, file_tag)

progress_dir <- 'progress'
dir.create(progress_dir, showWarnings = FALSE)
message('Saving run progress in directory: ', progress_dir)

results_dir <- 'output'
dir.create(results_dir, showWarnings = FALSE)
message('Saving final run results in directory: ', results_dir)

if (model_type == 'hier') {
    data_groups <- data_df[[pft_type]]
    source('scripts/informative_prior.R')
    message('Starting hierarchical model run...')
    raw_fit <- fit_mvnorm_hier(dat = data_mat,
                               groups = data_groups,
                               niter = niter,
                               nchains = nchains,
                               parallel = parallel,
                               autofit = autofit,
                               priors = prior,
                               threshold = threshold,
                               max_attempts = max_attempts,
                               keep_samples = keep_samples,
                               save_progress = save_progress)

} else if (model_type == 'multi') {
    source('scripts/informative_prior.R')
    message('Starting multivariate model run...')
    raw_fit <- fit_mvnorm(dat = data_mat,
                          niter = niter,
                          nchains = nchains,
                          parallel = parallel,
                          autofit = autofit,
                          priors = prior,
                          threshold = threshold,
                          max_attempts = max_attempts,
                          keep_samples = keep_samples,
                          save_progress = save_progress)
}

message('Model run complete. Saving results.')
saveRDS(raw_fit, file.path(results_dir, file_tag))
