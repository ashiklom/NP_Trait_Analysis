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
