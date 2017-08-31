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
        GLOB = "global",
        Bl = "broadleaf",
        Nl = "needleleaf",
        Sh = "shrub",
        C3G = "c3_grass",
        C4G = "c4_grass"
    ),
    jules2 = c(
        GLOB = "global",
        BlETr = "broadleaf_evergreen_tropical",
        BlETe = "broadleaf_evergreen_temperate",
        BlD = "broadleaf_deciduous",
        NlE = "needleleaf_evergreen",
        NlD = "needleleaf_deciduous",
        ShE = "shrub_evergreen",
        ShD = "shrub_deciduous",
        C3G = "c3_grass",
        C4G = "c4_grass"
    ),
    clm45 = c(
        GLOB = "global",
        BlETr = "broadleaf_evergreen_tropical",
        BlETe = "broadleaf_evergreen_temperate",
        BlDTr = "broadleaf_deciduous_tropical",
        BlDTe = "broadleaf_deciduous_temperate",
        NlE = "needleleaf_evergreen",
        NlD = "needleleaf_deciduous",
        ShE = "shrub_evergreen",
        ShDTe = "shrub_deciduous_temperate",
        ShDBo = "shrub_deciduous_boreal",
        C3GAr = "c3_grass_arctic",
        C3GTe = "c3_grass_temperate",
        C4G = "c4_grass"
    ),
    custom = c(
        GLOB = "global",
        BlETr = "broadleaf_evergreen_tropical",
        BlETe = "broadleaf_evergreen_temperate",
        BlDTr = "broadleaf_deciduous_tropical",
        BlDTe = "broadleaf_deciduous_temperate",
        NlE = "needleleaf_evergreen",
        NlD = "needleleaf_deciduous",
        C3G = "c3_grass",
        C4 = "c4",
        Su = "succulent",
        Nf = "n_fixer"
    )
)

#' @export
pft_schemes <- names(pft_levels)
