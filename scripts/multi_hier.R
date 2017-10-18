library(mvtraits)

mu_global <- c(x = 0, y = 0)
cov_global <- matrix(c(1, 0.9, 0.9, 1), nrow = 2)
dimnames(cov_global) <- rep(list(names(mu_global)), 2)
slope <- cov_global[2,1] * cov_global[2,2] / cov_global[1,1]
mu_group_x <- c(-0.75, 0, 0.75)
mu_group <- as.list(data.frame(rbind(mu_group_x, slope * mu_group_x)))
mu_group_mat <- Reduce(rbind, mu_group)

cor2cov <- function(cor, sdvec) {
    sddiag <- diag(sdvec)
    sddiag %*% cor %*% sddiag
}

sd_group <- c(0.2, 0.2)
cor_group_same <- lapply(list(a = cov_global, b = cov_global, c = cov_global), cov2cor)
cor_group_diff <- list(
    a = matrix(c(1, 0.9, 0.9, 1), nrow = 2),
    b = matrix(c(1, 0, 0, 1), nrow = 2),
    c = matrix(c(1, -0.9, -0.9, 1), nrow = 2)
)

cov_group_same <- lapply(cor_group_same, cor2cov, sdvec = sd_group)
cov_group_diff <- lapply(cor_group_diff, cor2cov, sdvec = sd_group)

n <- 1000
samps_list_same <- Map(random_mvnorm, mu = mu_group, Sigma = cov_group_same, n = n)
samps_list_diff <- Map(random_mvnorm, mu = mu_group, Sigma = cov_group_diff, n = n)

groups <- rep(1:3, each = n)
samps_same <- do.call(rbind, samps_list_same)
samps_diff <- do.call(rbind, samps_list_diff)
samps_diff_miss <- samps_diff
samps_diff_miss[sample.int(6000, 2000)] <- NA
samps_diff_miss[groups == 3, 2] <- NA

devtools::load_all("../../mvtraits")
fit_same <- fit_mvnorm_hier(dat = samps_diff_miss, groups = groups, parallel = FALSE)

fit_same_corr <- add_correlations(fit_same, hier = TRUE, ngroups = 3)
fit_same_mcmc <- results2mcmclist(fit_same_corr, type = "hier")
corr_names <- sprintf("Corr..group%02d..par02..par01", c(1:3))
plot(fit_same_mcmc[,corr_names])

#my_ellipse <- function(cov, mu, group, chisqval = 2.4477) {
    #ellipse_axes(cov, chisqval = chisqval) %>%
        #tidyr::unnest() %>%
        #dplyr::select(ellipse_x, ellipse_y) %>%
        #dplyr::mutate(ellipse_x = ellipse_x + mu[1], ellipse_y = ellipse_y + mu[2]) %>%
        #dplyr::mutate(group = group)
#}

#ellipse_same_list <- Map(my_ellipse, cov_group_same, mu_group, as.list(1:3))
#ellipse_diff_list <- Map(my_ellipse, cov_group_diff, mu_group, as.list(1:3))
#ellipse_global <- my_ellipse(cov_global, mu_global, group = "black", chisqval = 1.5)

my_poly <- function(l, ...) {
    with(l, lines(ellipse_x, ellipse_y, col = group, ...))
}

plot(samps_same[,1], samps_same[,2], col = groups, pch = '.', xlab = "Trait A", ylab = "Trait B")
points(mu_group_mat[,1], mu_group_mat[,2], col = 1:3, pch = 19, cex = 2)
purrr::walk(ellipse_same_list, my_poly)
my_poly(ellipse_global, lty = "dashed")

plot(samps_diff[,1], samps_diff[,2], col = groups, pch = '.', xlab = "Trait A", ylab = "Trait B")
points(mu_group_mat[,1], mu_group_mat[,2], col = 1:3, pch = 19, cex = 2)
purrr::walk(ellipse_diff_list, my_poly)
my_poly(ellipse_global, lty = "dashed")
