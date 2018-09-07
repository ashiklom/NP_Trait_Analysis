#' PFT abbreviations
#'
#' Dictionary from PFT abbreviations to PFTs and vice-versa.
#'
#' @export
abbr2pft <- c(
  GLOB = "global",
  BlETr = "broadleaf_evergreen_tropical",
  BlETe = "broadleaf_evergreen_temperate",
  BlDTr = "broadleaf_deciduous_tropical",
  BlDTe = "broadleaf_deciduous_temperate",
  BlDBo = "broadleaf_deciduous_boreal",
  NlETe = "needleleaf_evergreen_temperate",
  NlEBo = "needleleaf_evergreen_boreal",
  NlD = "needleleaf_deciduous",
  ShE = "shrub_evergreen",
  ShDTe = "shrub_deciduous_temperate",
  ShDBo = "shrub_deciduous_boreal",
  C3GAr = "c3_grass_arctic",
  C3GTe = "c3_grass_temperate",
  C4G = "c4_grass"
)

#' @rdname abbr2pft
#' @export
pft2abbr <- names(abbr2pft)
names(pft2abbr) <- abbr2pft
