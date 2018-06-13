library(shiklomanov2017np)
library(drake)
library(knitr)
library(kableExtra)

import::from(here, inhere = here)

get_sample_size_long <- function(try_dat) {
  count_missing <- . %>%
    dplyr::summarize(
      present = sum(!is.na(value)),
      missing = sum(is.na(value))
    )
  try_long <- try_dat %>%
    dplyr::select(pft, dplyr::one_of(both_params)) %>%
    tidyr::gather(trait, value, -pft, na.rm = FALSE)
  by_pft <- try_long %>%
    dplyr::group_by(pft, trait) %>%
    count_missing %>%
    dplyr::ungroup()
  global <- try_long %>%
    dplyr::group_by(trait) %>%
    count_missing %>%
    dplyr::mutate(
      pft = factor("GLOB", pft2abbr) %>%
        forcats::fct_recode("Across-PFT" = "GLOB")
    )
  dplyr::bind_rows(global, by_pft)
}

base_plan <- drake_plan(
  try_data_all = readRDS(inhere("extdata/traits_analysis.rds")),
  try_dat = try_data_all %>%
    dplyr::select(pft = clm45, !!both_params) %>%
    dplyr::mutate(
      pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
    ),
  sample_size_long = get_sample_size_long(try_dat),
  sample_size_wide = sample_size_long %>%
    dplyr::mutate(display = sprintf("%d (%d)", present, missing)) %>%
    dplyr::select(pft, trait, display) %>%
    tidyr::spread(trait, display),
  rmarkdown::render(
    knitr_in("supplementary_information.Rmd"),
    output_file = file_out("supplementary_information.pdf")
  ),
  strings_in_dots = "literals"
)

corr_data <- drake_plan(
  hier_summary_mass = readRDS(file_in("output/hier.mass.clm45.NA.2018-02-12-1154.rds"))[["summary_table"]] %>%
    mutate(mass_area = "mass", modeltype = "hier"),
  hier_summary_area = readRDS(file_in("output/hier.area.clm45.NA.2018-02-12-1227.rds"))[["summary_table"]] %>%
    mutate(mass_area = "area", modeltype = "hier"),
  multi_summary_mass = readRDS(file_in("output/multi.mass.clm45.NA.2018-02-12-1127.rds"))[["summary_table"]] %>%
    mutate(mass_area = "mass", modeltype = "multi"),
  multi_summary_area = readRDS(file_in("output/multi.area.clm45.NA.2018-02-12-1146.rds"))[["summary_table"]] %>%
    mutate(mass_area = "area", modeltype = "multi"),
  strings_in_dots = "literals"
)

corr_proc <- function(dat) {
  if (all(dat$modeltype == "hier")) {
    dat <- filter(dat, group != "global")
  } else if (all(dat$modeltype == "multi")) {
    dat <- mutate(dat, group = "global")
  } else {
    stop("Malformed modeltype.")
  }
  dat <- dat %>%
    filter(variable == "Corr") %>%
    separate(index, c("xvar", "yvar"), sep = "\\.\\.") %>%
    mutate(
      pft = factor(group, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
    ) %>%
    select(mass_area, pft, xvar, yvar, Mean, `2.5%`, `97.5%`)
}

traits_latex <- c(
  leaf_lifespan = "Leaf lifespan", SLA = "SLA",
  Nmass = "$N_\\textrm{mass}$", Narea = "$N_\\textrm{area}$",
  Pmass = "$P_\\textrm{mass}$", Parea = "$P_\\textrm{area}$",
  Rdmass = "$R_\\textrm{d, mass}$", Rdarea = "$R_\\textrm{d, area}$",
  Vcmax_mass = "$V_\\textrm{c, max, mass}$", Vcmax_area = "$V_\\textrm{c, max, area}$",
  Jmax_mass = "$J_\\textrm{max, mass}$", Jmax_area = "$J_\\textrm{max, area}$"
)

get_pairwise_missing <- function(xvar, yvar, trait_data) {
  xvar <- rlang::sym(xvar)
  yvar <- rlang::sym(yvar)
  n_all <- nrow(trait_data)
  pairs_present <- trait_data %>%
    dplyr::filter(!is.na(!!xvar), !is.na(!!yvar))
  n_present <- nrow(pairs_present)
  n_missing <- n_all - n_present
  tibble::tibble(present = n_present, missing = n_missing)
}

get_all_pairwise_missing <- function(trait_data, grid = trait_grid) {
  grid %>%
    dplyr::mutate(
      missing_info = purrr::map2(xvar, yvar, get_pairwise_missing, trait_data = trait_data)
    ) %>%
    tidyr::unnest(missing_info)
}

get_pairwise_missing_bypft <- function(trait_data, grid = trait_grid) {
  trait_data %>%
    dplyr::group_by(pft) %>%
    tidyr::nest() %>%
    dplyr::mutate(missing_data = map(data, get_all_pairwise_missing, grid = grid)) %>%
    tidyr::unnest(missing_data)
}

corr_proc_template <- drake_plan(proc = corr_proc(dataset__))
corr_proc_plan <- plan_analyses(corr_proc_template, data = corr_data)
corr_gathered <- gather_plan(corr_proc_plan, target = "all_corr_data", gather = "rbind")
corr_processed_plan <- drake_plan(
  trait_grid = combn(names(traits_latex), 2) %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::rename(yvar = V1, xvar = V2),
  pairwise_missing_global = get_all_pairwise_missing(try_dat, trait_grid) %>%
    dplyr::mutate(pft = factor("GLOB", pft2abbr)),
  pairwise_missing_pft = get_pairwise_missing_bypft(try_dat, trait_grid),
  pairwise_missing = dplyr::bind_rows(pairwise_missing_global, pairwise_missing_pft),
  corr_processed = all_corr_data %>%
    dplyr::left_join(pairwise_missing) %>%
    dplyr::mutate(
      significant = sign(`2.5%`) == sign(`97.5%`),
      sig_star = dplyr::if_else(significant, "*", ""),
      corr_value = sprintf("%.3f (%.3f, %.3f)%s", Mean, `2.5%`, `97.5%`, sig_star),
      xvar = factor(xvar, names(traits_latex)) %>% forcats::lvls_revalue(unname(traits_latex)),
      yvar = factor(yvar, names(traits_latex)) %>% forcats::lvls_revalue(unname(traits_latex)),
      pft = forcats::fct_recode(pft, "Across-PFT" = "GLOB")
    ) %>%
    ## dplyr::select(pft, yvar, xvar, corr_value) %>%
    dplyr::arrange(pft),
  strings_in_dots = "literals"
)

plan <- rbind(
  base_plan,
  corr_data,
  corr_proc_plan,
  corr_gathered,
  corr_processed_plan
)

dconfig <- drake_config(plan)
make(plan)

if (FALSE) {
  readd(sample_size_long)
}
