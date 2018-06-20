library(shiklomanov2017np)
library(drake)
library(knitr)
library(kableExtra)

import::from(here, inhere = here)

pkgconfig::set_config("drake::strings_in_dots" = "literals")

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
  )
)

corr_data <- drake_plan(
  hier_summary_mass = readRDS(file_in("output/hier.mass.clm45.NA.2018-02-12-1154.rds"))[["summary_table"]] %>%
    mutate(mass_area = "mass", modeltype = "hier"),
  hier_summary_area = readRDS(file_in("output/hier.area.clm45.NA.2018-02-12-1227.rds"))[["summary_table"]] %>%
    mutate(mass_area = "area", modeltype = "hier"),
  multi_summary_mass = readRDS(file_in("output/multi.mass.clm45.NA.2018-02-12-1127.rds"))[["summary_table"]] %>%
    mutate(mass_area = "mass", modeltype = "multi"),
  multi_summary_area = readRDS(file_in("output/multi.area.clm45.NA.2018-02-12-1146.rds"))[["summary_table"]] %>%
    mutate(mass_area = "area", modeltype = "multi")
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
      xvar = factor(xvar, names(traits_latex)),# %>% forcats::lvls_revalue(unname(traits_latex)),
      yvar = factor(yvar, names(traits_latex))# %>% forcats::lvls_revalue(unname(traits_latex)),
      ## pft = forcats::fct_recode(pft, "Across-PFT" = "GLOB")
    ) %>%
    ## dplyr::select(pft, yvar, xvar, corr_value) %>%
    dplyr::arrange(pft)
)

corr_plot <- function(data, ma) {
  data %>%
    tidyr::gather(variable, z_range_95, x_zrange, y_zrange) %>%
    dplyr::mutate(
      variable = stringr::str_remove(variable, "_zrange"),
      variable = dplyr::case_when(
        ma == "mass" & variable == "x" ~ "y",
        ma == "mass" & variable == "y" ~ "x",
        TRUE ~ variable
      )
    ) %>%
    ggplot() +
    aes(x = z_range_95, y = Mean2, color = pft) +
    geom_smooth(color = NA, method = "loess") +
    geom_point() +
    facet_grid(variable ~ .) +
    scale_color_manual(values = c("black", pft_colors)) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 0.8), xlim = c(0, 6.5)) +
    labs(color = "PFT")
}

corr_grid <- function(data) {
  data %>%
    mutate(
      significant = factor(significant, c("TRUE", "FALSE")) %>%
        forcats::lvls_revalue(c("yes", "no"))
    ) %>%
  ggplot() +
    aes(x = present + 0.1, y = Mean ^ 2, color = pft) + # size = significant
    geom_point() +
    geom_smooth(aes(group = 1), method = "loess", color = NA, span = 2) +
    scale_x_log10(breaks = c(1, 100, 10000), minor_breaks = c(10, 1000), limits = c(0.1, 30000)) +
    coord_cartesian(ylim = c(0, 1)) +
    ## scale_size_manual(values = c("yes" = 3, "no" = 1)) +
    scale_color_manual(name = "PFT", values = c("black", pft_colors)) +
    theme_bw()
}

if (FALSE) {


}

text_plot <- function(text) {
  ggplot() +
    geom_text(aes(label = text, x = 0, y = 0)) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )
}

range_plan <- drake_plan(
  mass_area_grid = c(
       1, 1, 1, 1, 1, 1,
    2,    1, 1, 1, 1, 1,
    2, 2,    1, 1, 1, 1,
    2, 2, 2,    1, 1, 1,
    2, 2, 2, 2,    1, 1,
    2, 2, 2, 2, 2,    1,
    2, 2, 2, 2, 2, 2
  ),
  yvar_grid = c(
       1, 1, 1, 1, 1, 1,
    1,    2, 2, 2, 2, 2,
    1, 2,    3, 3, 3, 3,
    1, 2, 3,    4, 4, 4,
    1, 2, 3, 4,    5, 5,
    1, 2, 3, 4, 5,    6,
    1, 2, 3, 4, 5, 6
  ),
  xvar_grid = c(
       2, 3, 4, 5, 6, 7,
    2,    3, 4, 5, 6, 7,
    3, 3,    4, 5, 6, 7,
    4, 4, 4,    5, 6, 7,
    5, 5, 5, 5,    6, 7,
    6, 6, 6, 6, 6,    7,
    7, 7, 7, 7, 7, 7
  ),
  trait_names = c("leaf_lifespan", "SLA", "N", "P", "Rd", "Vcmax", "Jmax"),
  trait_grid_table = tibble::tibble(
    mass_area = mass_area_grid,
    xvar = xvar_grid,
    yvar = yvar_grid
  ) %>%
    dplyr::mutate(
      mass_area = factor(mass_area) %>% forcats::lvls_revalue(c("mass", "area")),
      xvar = factor(xvar, 1:7) %>% forcats::lvls_revalue(trait_names),
      yvar = factor(yvar, 1:7) %>% forcats::lvls_revalue(trait_names),
    ),
  plt_df_orig = corr_processed %>%
    dplyr::group_by(mass_area, xvar, yvar) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      plt = purrr::map(data, corr_grid),
      xvar = stringr::str_remove(as.character(xvar), "_?(mass|area)") %>% factor(trait_names),
      yvar = stringr::str_remove(as.character(yvar), "_?(mass|area)") %>% factor(trait_names)
    ) %>%
    dplyr::ungroup(),
  plt_df = dplyr::left_join(trait_grid_table, plt_df_orig),
  plt_list = plt_df$plt %>%
    append(list(text_plot("Leaf lifespan")), 0) %>%
    append(list(text_plot("SLA")), 8) %>%
    append(list(text_plot("N")), 16) %>%
    append(list(text_plot("P")), 24) %>%
    append(list(text_plot("Rd")), 32) %>%
    append(list(text_plot("Vcmax")), 40) %>%
    append(list(text_plot("Jmax")), 48),
  corr_ss_plot_gg = GGally::ggmatrix(
    plt_list,
    nrow = 7,
    ncol = 7,
    xlab = "Sample size",
    ylab = "Squared pairwise correlation",
    legend = 2,
    progress = FALSE
  ),
  try_long = try_dat %>% tidyr::gather(trait, value, -pft, na.rm = TRUE),
  try_ranges = try_long %>%
    dplyr::group_by(trait) %>%
    dplyr::mutate(z_value = value / mean(value)) %>%
    ## dplyr::mutate(z_value = (value - mean(value)) / sd(value)) %>%
    dplyr::group_by(pft, trait) %>%
    dplyr::summarize(
      z_range_full = max(z_value) - min(z_value),
      z_range_95 = quantile(z_value, 0.975) - quantile(z_value, 0.025)
    ),
  corr_range_data = all_corr_data %>%
    dplyr::left_join(
      try_ranges %>% dplyr::select(yvar = trait, pft, y_zrange = z_range_95)
    ) %>%
    dplyr::left_join(
      try_ranges %>% dplyr::select(xvar = trait, pft, x_zrange = z_range_95)
    ) %>%
    dplyr::mutate(
      Mean2 = Mean ^ 2,
      xvar = factor(xvar, names(traits_latex)),
      yvar = factor(yvar, names(traits_latex))
    ),
  corr_plots = corr_range_data %>%
    dplyr::group_by(mass_area, xvar, yvar) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      ma = mass_area,
      plt = purrr::map2(data, ma, corr_plot),
      xvar = forcats::fct_relabel(xvar, ~stringr::str_remove(., "_?(mass|area)")),
      yvar = forcats::fct_relabel(yvar, ~stringr::str_remove(., "_?(mass|area)"))
    ) %>%
    dplyr::arrange(yvar, mass_area, xvar),
  corr_plot_list = corr_plots$plt %>%
    append(list(text_plot("Leaf lifespan")), 0) %>%
    append(list(text_plot("SLA")), 8) %>%
    append(list(text_plot("N")), 16) %>%
    append(list(text_plot("P")), 24) %>%
    append(list(text_plot("Rd")), 32) %>%
    append(list(text_plot("Vcmax")), 40) %>%
    append(list(text_plot("Jmax")), 48),
  corr_range_plot_gg = GGally::ggmatrix(
    corr_plot_list,
    nrow = 7,
    ncol = 7,
    xlab = "95% CI width of Z-normalized data",
    ylab = "Squared pairwise correlation",
    legend = 2,
    progress = FALSE
  )
)

censor <- function(rxp, pft, pfts, param, value) {
  if_else(grepl(rxp, param) & pfts %in% pft, "---", value)
}

## param_tex <- param_markdown
## ## stringr::str_replace(param_markdown, " \\(", "\\\\(")
## names(param_tex) <- names(param_markdown)

results_plan <- drake_plan(
  results_all = readRDS(file_in("results/mvtraits_results.rds")),
  multi_summary = results_all %>%
    dplyr::filter(model_type %in% c("uni", "multi")) %>%
    dplyr::mutate(
      mu_mean = map(data, c("result", "mu", "Mean")) %>%
        purrr::map(~tibble::as_tibble(as.list(.))),
      mu_lo = map(data, c("result", "mu", "2.5%")) %>%
        purrr::map(~tibble::as_tibble(as.list(.))),
      mu_hi = map(data, c("result", "mu", "97.5%")) %>%
        purrr::map(~tibble::as_tibble(as.list(.)))
    ) %>%
    dplyr::select(model_type:pft, mu_mean:mu_hi) %>%
    tidyr::unnest(.sep = ".") %>%
    tidyr::gather("variable", "value", -(model_type:pft), na.rm = TRUE) %>%
    tidyr::separate(variable, c("stat", "param"), sep = "\\.") %>%
    tidyr::spread(stat, value) %>%
    dplyr::mutate(
      pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr),
      param = factor(param, both_params)
    ),
  hier_prep = results_all %>%
    dplyr::filter(model_type == "hier") %>%
    dplyr::select(model_type:pft_type, data) %>%
    dplyr::mutate(
      mu_vals = purrr::map(data, c("result", "mu_group")),
      pft = purrr::map(mu_vals, names),
      mu_df = purrr::map2(pft, mu_vals, ~tibble::tibble(pft = ..1, dat = ..2))
    ) %>%
    dplyr::select(-data, -mu_vals, -pft) %>%
    tidyr::unnest(),
  hier_summary = hier_prep %>%
    dplyr::mutate(
      mu_mean = purrr::map(dat, c("Mean")) %>%
        purrr::map(~tibble::as_tibble(as.list(.))),
      mu_lo = purrr::map(dat, c("2.5%")) %>%
        purrr::map(~tibble::as_tibble(as.list(.))),
      mu_hi = purrr::map(dat, c("97.5%")) %>%
        purrr::map(~tibble::as_tibble(as.list(.)))
    ) %>%
    dplyr::select(model_type:pft, mu_mean:mu_hi) %>%
    tidyr::unnest(.sep = ".") %>%
    tidyr::gather("variable", "value", -(model_type:pft), na.rm = TRUE) %>%
    tidyr::separate(variable, c("stat", "param"), sep = "\\.") %>%
    tidyr::spread(stat, value) %>%
    dplyr::mutate(
      pft = dplyr::if_else(is.na(pft), "global", pft),
      pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr),
      param = factor(param, both_params)
    ),
  # Absolute comparison of means
  means_dat = multi_summary %>%
    dplyr::filter(pft != "GLOB", !is.na(pft)) %>%
    dplyr::bind_rows(hier_summary) %>%
    dplyr::mutate(
      model_type = factor(model_type, c("uni", "multi", "hier")) %>%
        forcats::lvls_revalue(c("univariate", "multivariate", "hierarchical")),
      mass_area = factor(mass_area, c("mass", "area"))
    ) %>%
    dplyr::mutate_at(
      c("mu_mean", "mu_lo", "mu_hi"),
      ~10 ^ .
    ) %>%
    dplyr::filter(
      !(param %in% c("leaf_lifespan", "SLA") & mass_area == "area")
    ),
  table_dat = means_dat %>%
    dplyr::filter(
      model_type == "hierarchical",
      !(param %in% c("SLA", "leaf_lifespan") & mass_area == "area")
    ) %>%
    dplyr::mutate(
      mu_lo = dplyr::if_else(param %in% c("Nmass", "Pmass"), mu_lo * 100, mu_lo),
      mu_hi = dplyr::if_else(param %in% c("Nmass", "Pmass"), mu_hi * 100, mu_hi),
      mu_mean = dplyr::if_else(param %in% c("Nmass", "Pmass"), mu_mean * 100, mu_mean),
      lo_f = formatC(mu_lo, digits = 3),
      mid_f = formatC(mu_mean, digits = 3),
      hi_f = formatC(mu_hi, digits = 3),
      value = sprintf("%s (%s,%s)", mid_f, lo_f, hi_f),
      value = censor("Vcmax|Jmax", "C4G", pft, param, value),
      value = censor("Rdmass", "ShDBo", pft, param, value),
      value = censor("Jmax_mass", c("NlEBo", "NlD"), pft, param, value),
      value = censor("Jmax_area", c("NlD"), pft, param, value)
    ) %>%
    dplyr::select(pft, param, value) %>%
    dplyr::mutate(
      param = factor(param, both_params) %>% 
        forcats::lvls_revalue(param_markdown_nounit[both_params]),
      ) %>%
    tidyr::spread(param, value) %>%
    dplyr::arrange(pft)
)

plan <- rbind(
  base_plan,
  corr_data,
  corr_proc_plan,
  corr_gathered,
  corr_processed_plan,
  range_plan,
  results_plan
)

dconfig <- drake_config(plan)
make(plan)

if (FALSE) {
  try_long <- readd(try_long)
  try_ranges <- readd(try_ranges)
  readd(corr_range_data) %>% glimpse()
  readd(corr_plots)$plt[[1]]

  clean(corr_plots)

  readd(sample_size_long)

  corr_results <- readd(corr_results)
 
  plan$target

  readd(corr_range_plot_gg)
}
