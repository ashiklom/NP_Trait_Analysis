#' @export
mass_params <- c('leaf_lifespan', 'SLA', 'Nmass', 'Pmass', 'Rdmass', 'Vcmax_mass', 'Jmax_mass')

#' @export
area_params <- gsub('mass$', 'area', mass_params)

#' @export
model_types <- c('multi', 'hier')

# PFT levels --------------------------------------------------------------
#' @export
pft_levels <- list(
    jules1 = c(
        "global",
        "broadleaf",
        "needleleaf",
        "shrub",
        "c3_grass",
        "c4_grass"
    ),
    jules2 = c(
        "global",
        "broadleaf_evergreen_tropical",
        "broadleaf_evergreen_temperate",
        "broadleaf_deciduous",
        "needleleaf_evergreen",
        "needleleaf_deciduous",
        "shrub_evergreen",
        "shrub_deciduous",
        "c3_grass",
        "c4_grass"
    ),
    clm45 = c(
        "global",
        "broadleaf_evergreen_tropical",
        "broadleaf_evergreen_temperate",
        "broadleaf_deciduous_tropical",
        "broadleaf_deciduous_temperate",
        "needleleaf_evergreen",
        "needleleaf_deciduous",
        "shrub_evergreen",
        "shrub_deciduous_temperate",
        "shrub_deciduous_boreal",
        "c3_grass_arctic",
        "c3_grass_temperate",
        "c4_grass"
    ),
    custom = c(
        "global",
        "broadleaf_evergreen_tropical",
        "broadleaf_evergreen_temperate",
        "broadleaf_deciduous_tropical",
        "broadleaf_deciduous_temperate",
        "needleleaf_evergreen",
        "needleleaf_deciduous",
        "c3_grass",
        "c4",
        "succulent",
        "n_fixer"
    )
)

#' @export
pft_schemes <- names(pft_levels)
