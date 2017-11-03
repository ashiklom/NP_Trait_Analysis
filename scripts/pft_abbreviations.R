abbr2pft <- c(
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
)

pft2abbr <- names(abbr2pft)
names(pft2abbr) <- abbr2pft

mass_params <- c("leaf_lifespan", "SLA", "Nmass", "Pmass", "Rdmass", "Vcmax_mass", "Jmax_mass")
area_params <- gsub("mass$", "area", mass_params)
param_labels <- c("Leaf lifespan", "SLA", "N", "P", "Rd", "Vcmax", "Jmax")

param_fancy <- list(
  leaf_lifespan = "Leaf lifespan (months)",
  Nmass = expression(paste(N[mass], " (\\%)")),
  Pmass = expression(paste(P[mass], " (\\%)")),
  SLA = expression(paste("SLA (", mm ^ 2 ~ mg ^ -1, ")")),
  Jmax_mass = expression(paste(J[{list(max, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  Vcmax_mass = expression(paste(V[{list(c, max, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  # TODO: Fix these
  Narea = expression(paste(N[mass], " (\\%)")),
  Parea = expression(paste(P[mass], " (\\%)")),
  Jmax_area = expression(paste(J[{list(max, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  Vcmax_area = expression(paste(V[{list(c, max, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")"))
)

source("scripts/paultolcolors.R")
pft_colors <- tol14rainbow[-c(10, 9)]
