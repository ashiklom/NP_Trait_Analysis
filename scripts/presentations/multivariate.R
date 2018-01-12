library(mvtraits)
set.seed(12345)

between <- function(x, a, b) {
  x >= a & x <= b
}

around <- function(x, z, tol = 0.05) {
  between(x, z - tol, z + tol)
}

multiplot <- function(samps, ellipse,
                      bval = 0.75,
                      ctrl = c("margin", "condition")) {
  stopifnot(colnames(samps) == c("Trait A", "Trait B"))
  sub <- around(samps[, "Trait B"], bval)
  samps_sub <- samps[sub, ]

  dens_a <- density(samps[, "Trait A"])
  dens_b <- density(samps[, "Trait B"])

  marg_dens_a <- density(samps_sub[, "Trait A"])
  quants_a <- quantile(samps_sub[, "Trait A"], c(0.01, 0.99))

  zones <- matrix(c(2, 4, 1, 3), nrow = 2, byrow = TRUE)
  ratios <- c(3/4, 1/4)
  grey_alpha <- rgb(0, 0, 0, 0.25)
  red_alpha <- rgb(1, 0, 0, 0.25)
  layout(zones, widths = ratios, heights = rev(ratios))
  ###
  xlims <- c(-4.25, 4.25)
  ylims <- c(-4.25, 4.25)
  par(mar = c(4, 4, 1, 1), mgp = c(2, 0.75, 0))
  plot(`Trait B` ~ `Trait A`, data = samps, pch = ".",
       xlim = xlims, ylim = ylims)
  polygon(ellipse$ellipse_x, ellipse$ellipse_y)
  if ("condition" %in% ctrl) {
    abline(h = bval, col = "blue", lty = "dashed", lwd = 1.5)
  }
  if ("margin" %in% ctrl) {
    abline(v = quants_a, col = "red", lty = "dashed", lwd = 1.5)
  }

  ###
  par(mar = c(0, 4, 1, 1))
  plot(dens_a$x, marg_dens_a$y, type = "n", axes = FALSE,
       main = NA, xlab = NA, ylab = NA, xlim = xlims)
  polygon(dens_a$x, dens_a$y, col = grey_alpha)
  if ("margin" %in% ctrl) {
    polygon(marg_dens_a$x, marg_dens_a$y, col = red_alpha)
  }

  ###
  par(mar = c(4, 0, 1, 1))
  plot(dens_b$y, dens_b$x, type = "n", axes = FALSE,
       main = NA, xlab = NA, ylab = NA, ylim = ylims)
  polygon(dens_b$y, dens_b$x, col = "grey")
  if ("condition" %in% ctrl) {
    abline(h = bval, col = "blue", lty = "dashed", lwd = 1.5)
  }

  ###
  if ("margin" %in% ctrl) {
    par(mar = c(0, 0, 0, 0))
    plot(0, 0, type = "n", axes = FALSE, main = NA, xlab = NA, ylab = NA)
    legend("center", pch = 19, col = c(grey_alpha, red_alpha),
           legend = c("Marginal", "Conditional"))
  }
  ### store plot

}

mu <- c(0, 0)
cov <- matrix(c(1, 0.90, 0.90, 1), nrow = 2, byrow = TRUE)
cov_diag <- diag(diag(cov))

ellipse <- tidyr::unnest(ellipse_axes(cov, prob = 0.95))
ellipse_diag <- tidyr::unnest(ellipse_axes(cov_diag, prob = 0.95))

samps <- random_mvnorm(25000, mu, cov)
samps_diag <- random_mvnorm(25000, mu, cov_diag)
colnames(samps) <- colnames(samps_diag) <- c("Trait A", "Trait B")

w <- 128 - 30
h <- 96 - 20
r <- 500
p <- 10

dir.create("figures/multivariate_conceptual", showWarnings = FALSE)
png("figures/multivariate_conceptual/no_covariance_%01d.png", 
    width = w, height = h, units = "mm", res = r, pointsize = p)
multiplot(samps_diag, ellipse_diag, ctrl = NA)
multiplot(samps_diag, ellipse_diag, ctrl = "condition")
multiplot(samps_diag, ellipse_diag)
dev.off()

png("figures/multivariate_conceptual/covariance_%01d.png", 
    width = w, height = h, units = "mm", res = r, pointsize = p)
multiplot(samps, ellipse, ctrl = NA)
multiplot(samps, ellipse, ctrl = "condition")
multiplot(samps, ellipse)
dev.off()
