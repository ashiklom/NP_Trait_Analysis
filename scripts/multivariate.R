library(mvtraits)

mu <- c(0, 0)
cov <- matrix(c(1, 0.90, 0.90, 1), nrow = 2, byrow = TRUE)

ellipse <- tidyr::unnest(ellipse_axes(cov, chisqval = 2.4477))

samps <- random_mvnorm(25000, mu, cov)
colnames(samps) <- c('Trait A', 'Trait B')

between <- function(x, a, b) {
  x >= a & x <= b
}

around <- function(x, z, tol = 0.05) {
  between(x, z - tol, z + tol)
}

bval <- 0.75

sub <- around(samps[,'Trait B'], bval)
samps_sub <- samps[sub,]

dens_a <- density(samps[,'Trait A'])
dens_b <- density(samps[,'Trait B'])

marg_dens_a <- density(samps_sub[,'Trait A'])
quants_a <- quantile(samps_sub[,'Trait A'], c(0.01, 0.99))

zones <- matrix(c(2, 4, 1, 3), nrow = 2, byrow = TRUE)
ratios <- c(3/4, 1/4)
grey_alpha <- rgb(0, 0, 0, 0.25)
red_alpha <- rgb(1, 0, 0, 0.25)
layout(zones, widths = ratios, heights = rev(ratios))
###
par(mar = c(4, 4, 1, 1), mgp = c(2, 0.75, 0))
plot(`Trait B` ~ `Trait A`, data = samps, pch = ".", cex.lab = 1.3)
abline(h = bval, col = "blue", lty = "dashed")
abline(v = quants_a, col = "red", lty = "dashed")
polygon(ellipse$ellipse_x, ellipse$ellipse_y)
###
par(mar = c(0, 4, 1, 1))
plot(dens_a$x, marg_dens_a$y, type = 'n', axes = FALSE, main = NA, xlab = NA, ylab = NA)
polygon(dens_a$x, dens_a$y, col = grey_alpha)
polygon(marg_dens_a$x, marg_dens_a$y, col = red_alpha)
###
par(mar = c(4, 0, 1, 1))
plot(dens_b$y, dens_b$x, type = 'n', axes = FALSE, main = NA, xlab = NA, ylab = NA)
polygon(dens_b$y, dens_b$x, col = "grey")
abline(h = bval, col = "blue", lty = "dashed")
###
par(mar = c(0,0,0,0))
plot(0, 0, type = 'n', axes = FALSE, main = NA, xlab = NA, ylab = NA)
legend("center", pch = 19, col = c(grey_alpha, red_alpha), legend = c("Marginal", "Conditional"), cex = 1.5)
