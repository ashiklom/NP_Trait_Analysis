#' Trait names
#'
#' Names of traits used in analyses, useful for factor levels and for quick 
#' selection from data frames.
#'
#' Contains the following:
#' - `mass_params` -- Mass-based traits
#' - `area_params` -- Area-based traits
#' - `both_params` -- "Base" names
#'
#' @name trait_names
NULL

#' @rdname trait_names
#' @export
mass_params <- c("leaf_lifespan", "SLA", "Nmass", "Pmass", "Rdmass", "Vcmax_mass", "Jmax_mass")

#' @rdname trait_names
#' @export
area_params <- gsub("mass$", "area", mass_params)

#' @rdname trait_names
#' @export
both_params <- c("leaf_lifespan", "SLA", "Nmass", "Narea", "Pmass", "Parea",
                 "Rdmass", "Rdarea", "Vcmax_mass", "Vcmax_area",
                 "Jmax_mass", "Jmax_area")

#' Trait labels
#'
#' Various vectors for providing formatted trait labels for figures and tables.
#'
#' Contains the following:
#' - `param_labels` -- Base trait names, without mass/area suffix
#' - `param_simple` -- R expression-formatted trait names with units, as character (used for `parse = TRUE`)
#' - `param_fancy_chr` -- R expression-formatted trait names with units, as character (used for `parse = TRUE`)
#' - `param_fancy` -- R expression-formatted trait names with units, as R `formula` vector
#' - `param_markdown_nounit` -- LaTeX-formatted trait names without units, as character
#' - `param_markdown` -- LaTeX-formatted trait names with units, as character
#' @name trait_labels
NULL

#' @rdname trait_labels 
#' @export
param_labels <- c("Leaf lifespan", "SLA", "N", "P", "Rd", "Vcmax", "Jmax")

#' @rdname trait_labels
#' @export
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

#' @rdname trait_labels  
#' @export
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

#' @rdname trait_labels 
#' @export
param_fancy <- sapply(param_fancy_chr, formula)

#' @rdname trait_labels 
#' @export
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

#' @rdname trait_labels 
#' @export
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
