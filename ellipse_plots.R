library(tidyverse)
library(magrittr)

figures_dir <- 'figures'
dir.create(figures_dir, showWarnings = FALSE)

all_summaries <- readRDS('results/summaries_processed.rds')

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

single_ellipse <- function(dat, x_param, y_param) {
  xy_param <- c(x_param, y_param)

  dat_sub <- dat %>%
    select(variable, pft, Mean, param, xparam, yparam) %>%
    filter(param %in% xy_param | (xparam %in% xy_param & yparam %in% xy_param)) %>%
    group_by(pft) %>%
    nest()

  df2covar <- function(df) {
    df %>%
      filter(variable == 'Sigma') %$%
      mvtraits::dfcol2mat(Mean, as.character(xparam), as.character(yparam), xy_param)
  }

  ellipse_axes <- function(mat) {
    eig <- eigen(mat)
    max_val <- max(eig$values)
    imax <- which(eig$values == max_val)
    max_vec <- eig$vectors[,imax]
    min_val <- min(eig$values)
    imin <- which(eig$values == min_val)
    min_vec <- eig$vectors[,imin]
    angle <- atan2(max_vec[2], max_vec[1])
    theta <- seq(0, 2 * pi, 0.01)
    chisqval <- 1 #2.4477
    a <- chisqval * sqrt(max_val)
    b <- chisqval * sqrt(min_val)
    ellipse_x <- a * cos(theta)
    ellipse_y <- b * sin(theta)
    R <- cbind(c(cos(angle), -sin(angle)), c(sin(angle), cos(angle)))
    r_ellipse <- cbind(ellipse_x, ellipse_y) %*% R
    tibble(
      angle = angle,
      radius = sqrt(max_val),
      ellipse_x = list(r_ellipse[,1]),
      ellipse_y = list(r_ellipse[,2])
    )
  }

  get_mean <- function(dat, pparam) {
    dat %>%
      filter(param == pparam, variable == 'mu') %>%
      pull(Mean)
  }

  dat_sub_2 <- dat_sub %>%
    mutate(covar = map(data, df2covar),
           axes = map(covar, ellipse_axes),
           mean_x = map_dbl(data, get_mean, pparam = x_param),
           mean_y = map_dbl(data, get_mean, pparam = y_param))

  dat_sub_3 <- dat_sub_2 %>%
    unnest(axes)

  dat_global <- dat_sub_3 %>%
    filter(pft == 'global') %>%
    unnest(ellipse_x, ellipse_y) %>%
    select(mean_x, mean_y, ellipse_x, ellipse_y)

  ggplot(dat_sub_3) +
    aes(x = mean_x, y = mean_y, color = pft) +
    geom_polygon(
      data = dat_global,
      mapping = aes(x = mean_x + ellipse_x, y = mean_y + ellipse_y),
      fill = "grey60",
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    geom_spoke(aes(angle = angle, radius = radius)) +
    geom_spoke(aes(angle = angle + pi, radius = radius)) +
    geom_point() +
    labs(x = x_param, y = y_param)
}

ellipses <- all_summaries %>%
  filter(model_type == 'hier') %>%
  mutate(
    ellipse_plot = map(data, ellipse_matrix),
    fname = paste(model_type, area_mass, pft_scheme, 'pdf', sep = '.'),
    fname = file.path(figures_dir, fname)
  )

ellipses %$%
  walk2(fname, ellipse_plot, ggsave)
