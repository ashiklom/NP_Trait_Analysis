library(shiklomanov2017np)

all_results <- readRDS(here::here("results", "mvtraits_results.rds"))

all_tidy_eigen <- function(results, mass_area) {
  mass_area <- match.arg(mass_area, c("mass", "area"))
  col_names <- switch(
    mass_area,
    mass = shiklomanov2017np::mass_params,
    area = shiklomanov2017np::area_params
  )
  r <- results %>%
    filter(model_type == "hier",
           mass_area == !!mass_area) %>%
    pull(data) %>%
    pluck(1, "result")

  glob_eig <- r %>%
    pluck("Corr_global", "Mean") %>%
    tidy_eigen(col_names, "global")

  pft_eig <- r %>%
    pluck("Corr_group") %>%
    map("Mean") %>%
    imap(~tidy_eigen(.x, col_names, .y))

  all_vals <- map_dfr(pft_eig, "values") %>%
    bind_rows(glob_eig$values, .) %>%
    mutate(pft = forcats::fct_inorder(pft),
           mass_area = !!mass_area) %>%
    group_by(pft) %>%
    mutate(relative_ev = eigvalue / sum(eigvalue)) %>%
    ungroup()

  all_vecs <- map_dfr(pft_eig, "vectors") %>%
    bind_rows(glob_eig$vectors, .) %>%
    mutate(param = factor(param, shiklomanov2017np::both_params),
           pft = forcats::fct_inorder(pft),
           mass_area = !!mass_area)

  list(
    values = all_vals,
    vectors = all_vecs
  )
}

tidy_eigen <- function(corrmat, col_names, pft) {
  eig <- eigen(corrmat[col_names, col_names])
  ## value <- cumsum(eig$values) / sum(eig$values)
  EVnames <- paste0("EV", seq_along(col_names))
  vecmat <- eig$vectors %>%
    `rownames<-`(col_names) %>%
    `colnames<-`(EVnames)
  # Directionality of eigenvectors doesn't really matter, as long as its
  # mutually consistent. So force the first row (leaf lifespan) to be positive
  # and change the sign of all the other vectors accordingly.
  for (i in seq_len(ncol(vecmat))) {
    vecmat[, i] <- vecmat[, i] * sign(vecmat[2, i])
  }
  vecs <- vecmat %>%
    tibble::as_tibble(rownames = "param") %>%
    dplyr::mutate(pft = !!pft)
  value_df <- tibble::tibble(pft = pft, EV = EVnames, eigvalue = eig$values)
  list(
    values = value_df,
    vectors = vecs
  )
}

ev1_plot <- function(dat) {
  dat %>%
    group_by(param) %>%
    mutate(S = var(EV1)) %>%
    ungroup() %>%
    mutate(pft = forcats::fct_recode(pft, !!!abbr2pft),
           param = forcats::fct_reorder(param, S)) %>%
    ggplot() +
    aes(x = param, y = EV1, color = pft, group = pft) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    geom_point() +
    scale_color_manual(values = c("black", pft_colors)) +
    guides(color = guide_legend(title = "PFT",
                                nrow = 3,
                                override.aes = list(size = 3))) +
    cowplot::theme_cowplot() +
    theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = rel(0.8)))
}

mass_l <- all_tidy_eigen(all_results, "mass")
mass_v <- mass_l$vectors

area_l <- all_tidy_eigen(all_results, "area")
area_v <- area_l$vectors

mass_plt <- ev1_plot(mass_v) + ggtitle("Mass-normalized") +
  theme(legend.justification = "center")
area_plt <- ev1_plot(area_v) + ggtitle("Area-normalized")
leg <- cowplot::get_legend(mass_plt)
both_plt <- cowplot::plot_grid(
  cowplot::plot_grid(mass_plt + guides(color = FALSE),
                     area_plt + guides(color = FALSE),
                     nrow = 1),
  leg, nrow = 2,
  rel_heights = c(0.8, 0.2)
)

figdir <- here::here("figures", "ecoapp-revision")
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(figdir, "eigenvector.png"), both_plt,
       width = 12.4, height = 6.4)

##################################################
## Old code
## use_ev <- c("EV1", "EV2", "EV3")

## # First three
## plt1 <- long_vecs %>%
##   filter(EV %in% use_ev) %>%
##   ggplot() +
##   aes(x = param, y = value, color = pft, group = pft) +
##   geom_line() +
##   geom_hline(yintercept = 0, linetype = "dashed") +
##   facet_wrap(vars(EV), scales = "free_y") +
##   scale_color_manual(values = c("black", pft_colors)) +
##   guides(color = guide_legend(nrow = 3,
##                               override.aes = list(size = 3))) +
##   theme(legend.position = "bottom",
##         axis.title.x = element_blank())

## ev1_plot_data <- long_vecs %>%
##   filter(EV == "EV1") %>%
##   group_by(param) %>%
##   mutate(S = var(value)) %>%
##   ungroup() %>%
##   mutate(param = forcats::fct_reorder(param, S))
## # Just EV1 (LES)
## ggplot(ev1_plot_data) +
##   aes(x = param, y = value, color = pft, group = pft) +
##   geom_line() +
##   geom_hline(yintercept = 0, linetype = "dashed") +
##   scale_color_manual(values = c("black", pft_colors)) +
##   guides(color = guide_legend(nrow = 3,
##                               override.aes = list(size = 3))) +
##   theme(legend.position = "bottom",
##         axis.title.x = element_blank())

## plt_eigvals <- ggplot(all_vals) +
##   aes(x = EV, y = relative_ev, color = pft, group = pft) +
##   geom_line() +
##   scale_color_manual(values = c("black", pft_colors)) +
##   guides(color = FALSE)
## leg <- cowplot::get_legend(plt_ev1)
## plots <- cowplot::plot_grid(plt_ev1 + guides(color = FALSE) +
##                               ggtitle("Eigenvector 1: Leaf economic spectrum"),
##                             plt_eigvals + ggtitle("Relative eigenvalues"), nrow = 1)
## cowplot::plot_grid(plots, leg, nrow = 2, rel_heights = c(0.8, 0.2))

## ggsave("figures/candidate_ev_mass.png")


## glob_value <- cumsum(glob_eig$values) / sum(glob_eig$values)
## glob_vec <- glob_eig$vectors %>%
##   `rownames<-`(mass_params) %>%
##   `colnames<-`(paste0("PC", seq_along(mass_params))) %>%
##   as_tibble(rownames = "trait") %>%
##   mutate(pft = "global")
