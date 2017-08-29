ellipse_matrix <- function(dat) {
  params <- dat %>% pull(param) %>% levels()
  nparams <- length(params)
  param_df <- tibble(xparam = params, yparam = params) %>%
    expand(xparam, yparam)

  pfts <- dat %>% pull(pft) %>% levels()
  npfts <- length(pfts)
  pft_colors <- c('black', RColorBrewer::brewer.pal(npfts - 1, "Paired"))
  names(pft_colors) <- pfts

  custom_ellipse <- function(...) {
    single_ellipse(...) +
      guides(color = FALSE) +
      scale_color_manual(values = pft_colors) +
      theme_bw() +
      theme(axis.title = element_blank())
  }

  plots <- param_df %>%
    mutate(ellipse_plot = ifelse(
      xparam == yparam,
      map(xparam, grid::textGrob),
      map2(xparam, yparam, custom_ellipse, dat = dat)
    ))

  plots_layout <- plots %>%
    mutate_if(is_character, factor, levels = params) %>%
    mutate(x = as.integer(xparam),
           y = as.integer(yparam),
           i = x + (y - 1) * nparams)

  plots_list <- rep(list(grid::textGrob("")), nparams ^ 2)
  for (i in seq_len(nrow(plots_layout))) {
    plots_list[[plots_layout[[i, 'i']]]] <- plots_layout[[i, 'ellipse_plot']]
  }

  gridExtra::arrangeGrob(grobs = plots_list, nrow = nparams, ncol = nparams)
}
