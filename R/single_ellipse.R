#' @export
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
    filter(pft == 'GLOB') %>%
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
