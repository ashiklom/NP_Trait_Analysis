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

pft2abbr <- names(abbr2pft)
names(pft2abbr) <- abbr2pft

mass_params <- c("leaf_lifespan", "SLA", "Nmass", "Pmass", "Rdmass", "Vcmax_mass", "Jmax_mass")
area_params <- gsub("mass$", "area", mass_params)
both_params <- c("leaf_lifespan", "SLA", "Nmass", "Narea", "Pmass", "Parea",
                 "Rdmass", "Rdarea", "Vcmax_mass", "Vcmax_area",
                 "Jmax_mass", "Jmax_area")
param_labels <- c("Leaf lifespan", "SLA", "N", "P", "Rd", "Vcmax", "Jmax")

param_simple <- c(
  leaf_lifespan = "Leaf~lifespan",
  SLA = "SLA",
  Nmass = "N[mass]",
  Narea = "N[area]",
  Pmass = "P[mass]",
  Parea = "P[area]",
  Rdmass = "R[list(d, mass)]",
  Rdarea = "R[list(d, area)]",
  Vcmax_mass = "V[list(c, max, mass)]",
  Vcmax_area = "V[list(c, max, area)]",
  Jmax_mass = "J[list(max, mass)]",
  Jmax_area = "J[list(max, area)]"
)

param_markdown_nounit <- c(
  leaf_lifespan = "Leaf lifespan",
  SLA = "SLA",
  Nmass = "$N_{mass}$",
  Narea = "$N_{area}$",
  Pmass = "$P_{mass}$",
  Parea = "$P_{area}$",
  Rdmass = "$R_{d,mass}$",
  Rdarea = "$R_{d, area}$",
  Vcmax_mass = "$V_{c, max, mass}$",
  Vcmax_area = "$V_{c, max, area}$",
  Jmax_mass = "$J_{max, mass}$",
  Jmax_area = "$J_{max, area}$"
)
param_markdown <- c(
  leaf_lifespan = "Leaf lifespan (months)",
  SLA = "SLA (m$^2$ kg$^{-1}$)",
  Nmass = "$N_{mass}$ (mg g$^{-1}$)",
  Narea = "$N_{area}$ (g m$^{-2}$)",
  Pmass = "$P_{mass}$ (mg g$^{-1}$)",
  Parea = "$P_{area}$ (g m$^{-2}$)",
  Rdmass = "$R_{d,mass}$ (µmol g$^{-1}$ s$^{-1}$)",
  Rdarea = "$R_{d, area}$ (µmol m$^{-2}$ s$^{-1}$)",
  Vcmax_mass = "$V_{c, max, mass}$ (µmol g$^{-1}$ s$^{-1}$)",
  Vcmax_area = "$V_{c, max, area}$ (µmol m$^{-2}$ s$^{-1}$)",
  Jmax_mass = "$J_{max, mass}$ (µmol g$^{-1}$ s$^{-1}$)",
  Jmax_area = "$J_{max, area}$ (µmol m$^{-2}$ s$^{-1}$)"
)

param_fancy_chr <- c(
  leaf_lifespan = "Leaf~lifespan~(months)",
  Nmass = "N[mass]~(mg ~ g ^ -1)",
  Pmass = "P[mass]~(mg ~ g ^ -1)",
  SLA = "SLA~(m ^ 2 ~ kg ^ -1)",
  Rdmass = "R[list(d,mass)]~(mu * mol ~ g ^ -1 ~ s ^ -1)",
  Vcmax_mass = "V[list(c, max, mass)]~(mu * mol ~ g ^ -1 ~ s ^ -1)",
  Jmax_mass = "J[list(max, mass)]~(mu * mol ~ g ^ -1 ~ s ^ -1)",
  # TODO: Fix these
  Narea = "N[area]~(g ~ m ^ -2)",
  Parea = "P[area]~(g ~ m ^ -2)",
  Rdarea = "R[{list(d, area)}]~(mu * mol ~ m ^ -2 ~ s ^ -1)",
  Vcmax_area = "V[{list(c, max, area)}]~(mu * mol ~ m ^ -2 ~ s ^ -1)",
  Jmax_area = "J[{list(max, area)}]~(mu * mol ~ m ^ -2 ~ s ^ -1)"
)

param_fancy <- sapply(param_fancy_chr, formula)

#param_fancy <- c(
  #leaf_lifespan = "Leaf lifespan (months)",
  #Nmass = expression(N[mass]~(
  #Pmass = expression((P[mass], " (%)")),
  #SLA = expression(("SLA (", m ^ 2 ~ kg ^ -1, ")")),
  #Rdmass = expression((R[{list(d, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  #Jmax_mass = expression((J[{list(max, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  #Vcmax_mass = expression((V[{list(c, max, mass)}], " (", mu * mol ~ g ^ -1 ~ s ^ -1, ")")),
  ## TODO: Fix these
  #Narea = expression(N[mass] ~ (mg ~ mm ^ -2)),
  #Parea = expression(P[mass] ~ (mg ~ mm ^ -2)),
  #Jmax_area = expression(J[{list(max, mass)}] ~ (mu * mol ~ g ^ -1 ~ s ^ -1)),
  #Vcmax_area = expression(V[{list(c, max, mass)}] ~ (mu * mol ~ g ^ -1 ~ s ^ -1))
#)

source("scripts/paultolcolors.R")
pft_inds <- c(
  BlETr = 1,
  BlETe = 3,
  BlDTr = 4,
  BlDTe = 5,
  BlDBo = 6,
  NlETe = 7,
  NlEBo = 8,
  NlD = 9,
  ShE = 13,
  ShDTe = 14,
  ShDBo = 15,
  C3GAr = 16,
  C3GTe = 17,
  C4G = 18
)
pft_colors <- tol21rainbow[pft_inds]
#pft_colors <- tol14rainbow[-c(10, 9)]
