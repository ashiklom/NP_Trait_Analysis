## LOAD DATA AND PACKAGES #############################################################
library(mvtraits)
library(tidyverse)

try_data <- readRDS('traits_analysis.rds')

nowdate <- strftime(Sys.time(), "%Y_%m_%d")
out_dir <- "output_fixed"
dir.create(out_dir, showWarnings = FALSE)

if(!exists("cmdargs")) cmdargs <- commandArgs(trailingOnly=TRUE)
# Possible arguments: 
#   uni[_<pft>] -- Run univariate model for specified PFT number (no number means run all PFTs)
#   multi[_<pft>] -- Run multivariate model for specified PFT number
#   hier -- Run hierarchical model (global and PFT means)
#   n.chains=n -- Run with `n` chains, where `n` is an integer.
if(length(cmdargs) == 0){
  cmdargs <- c("area", "uni", "uni_01", "multi", "multi_01", "hier", "n.chains=3")   # For testing
}
area_mass <- cmdargs[1]
model_args <- cmdargs[-1]
stopifnot(area_mass %in% c('area', 'mass'))
message("Running with the following arguments:")
print(model_args)

area_rxp <- 'leaf_lifespan|LMA|area'
mass_rxp <- 'leaf_lifespan|SLA|mass'
use_rxp <- switch(area_mass, area = area_rxp, mass = mass_rxp)

data_df <- try_data %>% 
    dplyr::select(pft, matches(use_rxp)) %>% 
    dplyr::filter_at(dplyr::vars(matches(use_rxp)), dplyr::any_vars(!is.na(.)))

data_mat <- data_df %>% dplyr::select(-pft) %>% as.matrix() %>% log10()
data_groups <- data_df %>% dplyr::pull(pft) %>% as.integer()

# Get number of chains from arguments
default.chains <- 3
n.chains.index <- grep("n.chains", model_args)
n.chains <- ifelse(length(n.chains.index > 0),
                   as.numeric(gsub("n.chains=", "", model_args[n.chains.index])),
                   default.chains)

# Determine which models to run from arguments
model.args <- model_args[grep("uni|multi|hier", model_args)]
model.args.list <- strsplit(model.args, "_")

# Run models
errors <- character()
for (model_arg in model.args){
    arg <- strsplit(model_arg, "_")[[1]]
    message(paste("Running", model_arg))
    model_type <- arg[1]
    if (model_type %in% c('uni', 'multi')) {
        pft_number <- as.numeric(arg[2])
        if (!is.na(pft_number)) {
            stopifnot(pft_number <= max(data_groups))
            dat <- data_mat[data_groups == pft_number,]
        } else {
            dat <- data_mat
        }
        groups <- NA
        model_name <- sprintf('%s_%s_%s', area_mass, model_type, pft_number)
    } else if (model_type == 'hier') {
        dat <- data_mat
        groups <- data_groups
        model_name <- sprintf('%s_%s', area_mass, model_type)
    } else {
        stop('Invalid model type "', model_type, '"')
    }
    out <- runModel(model_type = model_type, 
                    dat = dat, 
                    groups = groups, 
                    model_name = model_name,
                    chains = n.chains, 
                    iter = 10000, 
                    save_each = FALSE)
    if(!all(is.error(out))){
        save(out, file = sprintf("%s/%s.%s.%s.Rdata", out_dir, area_mass, model_arg, nowdate))
    } else {
        warning(paste("Error running", model_arg))
        errors <- c(errors, arg)
    }
}

print("=================================")
print("PRINT WARNINGS")

warnings()
