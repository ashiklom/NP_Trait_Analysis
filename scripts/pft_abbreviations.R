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

param_fancy_chr <- c(
  leaf_lifespan = "Leaf~lifespan~(months)",
  Nmass = "N[mass]~('%')",
  Pmass = "P[mass]~('%')",
  SLA = "SLA~(mm ^ 2 ~ mg ^ -1)",
  Rdmass = "R[{list(d,mass)}]~(mu * mol ~ g ^ -1 ~ s ^ -1)",
  Jmax_mass = "J[{list(max, mass)}]~(mu * mol ~ g ^ -1 ~ s ^ -1)",
  Vcmax_mass = "V[{list(c, max, mass)}]~(mu * mol ~ g ^ -1 ~ s ^ -1)",
  # TODO: Fix these
  Narea = "N[area]~(mg ~ mm ^ -2)",
  Parea = "P[area]~(mg ~ mm ^ -2)",
  Rdarea = "R[{list(d, area)}]~(mu * mol ~ mm ^ -2 ~ s ^ -1)",
  Jmax_area = "J[{list(max, area)}]~(mu * mol ~ mm ^ -2 ~ s ^ -1)",
  Vcmax_area = "V[{list(c, max, area)}]~(mu * mol ~ mm ^ -2 ~ s ^ -1)"
)

param_fancy <- c(
  leaf_lifespan = "Leaf lifespan (months)",
  Nmass = expression(paste(N[mass], " (%)")),
  Pmass = expression(paste(P[mass], " (%)")),
  SLA = expression(paste("SLA (", mm ^ 2 ~ mg ^ -1, ")")),
  Rdmass = expression(paste(R[{list(d, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  Jmax_mass = expression(paste(J[{list(max, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  Vcmax_mass = expression(paste(V[{list(c, max, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  # TODO: Fix these
  Narea = expression(N[mass] ~ (mg ~ mm ^ -2)),
  Parea = expression(P[mass] ~ (mg ~ mm ^ -2)),
  Jmax_area = expression(J[{list(max, mass)}] ~ (mu * mol ~ g ^ -1 ~ s ^ -1)),
  Vcmax_area = expression(V[{list(c, max, mass)}] ~ (mu * mol ~ g ^ -1 ~ s ^ -1))
)

source("scripts/paultolcolors.R")
pft_colors <- tol21rainbow[c(1, 3, 4, 6, 7, 9, 13:15, 16:18)]
#pft_colors <- tol14rainbow[-c(10, 9)]
