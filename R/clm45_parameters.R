#' CLM Table 8.1
#'
#' Community Land Model (CLM) version 4.5 parameters table (8.1), for 
#' comparison with results.
#'
#' Columns are as follows:
#'  - `pft` -- Plant functional type, corresponding to analysis PFT
#' - `CN_leaf` -- Leaf C:N ratio, (g C g-1 N)
#' - `F_LNR` -- Fraction of leaf nitrogen in Rubisco (g N Rubisco g-1 N)
#' - `SLA` -- Specific leaf area (m2 g-1 C)
#' - `Vcmax_25` -- Vcmax at 25C at canopy top (mu~mol m-2 s-1)
#' @export
clm45_table81 <- tribble(
  ~pft, ~CN_leaf, ~F_LNR, ~SLA, ~Vcmax_25,
  "needleleaf_evergreen_temperate", 35, 0.0509, 0.01, 62.5,
  "needleleaf_evergreen_boreal", 40, 0.0466, 0.008, 62.6,
  "needleleaf_deciduous", 25, 0.0546, 0.024, 39.1,
  "broadleaf_evergreen_tropical", 30, 0.0461, 0.012, 55,
  "broadleaf_evergreen_temperate", 30, 0.0515, 0.012, 61.5,
  "broadleaf_deciduous_tropical", 25, 0.0716, 0.030, 41,
  "broadleaf_deciduous_temperate", 25, 0.1007, 0.030, 57.7,
  "broadleaf_deciduous_boreal", 25, 0.1007, 0.030, 57.7,
  "shrub_evergreen", 30, 0.0517, 0.012, 61.7,
  "shrub_deciduous_temperate", 25, 0.0943, 0.03, 54.0,
  "shrub_deciduous_boreal", 25, 0.0943, 0.03, 54.0,
  "c3_grass_arctic", 25, 0.1365, 0.03, 78.2,
  "c3_grass_temperate", 25, 0.1365, 0.03, 78.2,
  "c4_grass", 25, 0.09, 0.03, 51.6
)
