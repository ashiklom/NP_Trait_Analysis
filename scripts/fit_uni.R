library(runjags)
library(shiklomanov2017np)

try_data <- readRDS("extdata/traits_analysis.rds")
source("scripts/informative_prior.R")

jags_model <- "
model{
  ## Convert parameters
  tau0 <- 1 / sqrt(sigma0)
  sigma <- 1 / sqrt(tau)

  ## Priors
  mu ~ dnorm(mu0, tau0)
  tau ~ dgamma(a0, b0)

  ## Data model
  for (i in 1:n) {
    x[i] ~ dnorm(mu, tau)
  }
}
"

jags_prior <- "
model{
  ## Convert parameters
  tau0 <- 1 / sqrt(sigma0)
  sigma <- 1 / sqrt(tau)

  ## Priors
  mu ~ dnorm(mu0, tau0)
  tau ~ dgamma(a0, b0)
}
"

pft_type <- "clm45"
pft_type_q <- rlang::sym(pft_type)
pfts <- levels(try_data[[pft_type]])

for (pft in pfts) {
  dat_sub <- try_data %>%
    filter(UQ(pft_type_q) == UQ(pft))
  for (ma in c("mass", "area")) {
    mu_mean <- mu_lo <- mu_hi <- numeric()
    sigma_mean <- sigma_lo <- sigma_hi <- numeric()
    pars <- switch(ma, mass = mass_params, area = area_params)
    for (trait in pars) {
      message("PFT: ", pft)
      message("Mass/area: ", ma)
      message("Trait: ", trait)
      prior_sub <- prior_df %>%
        filter(param == trait)
      x <- dat_sub[[trait]] %>%
        log10() %>%
        na.omit()
      nx <- length(x)
      jags_data <- list(
        mu0 = prior_sub[["mean"]],
        sigma0 = prior_sub[["stdev"]],
        a0 = 1,
        b0 = 1
      )
      if (nx > 0) {
        jags_data$x <- x
        jags_data$n <- length(x)
        model <- jags_model
      } else {
        model <- jags_prior
      }
      fit <- autorun.jags(model, c("mu", "sigma"), jags_data)
      fitsum <- summary(fit)

      mu_mean[trait] <- fitsum["mu", "Mean"]
      mu_lo[trait] <- fitsum["mu", "Lower95"]
      mu_hi[trait] <- fitsum["mu", "Upper95"]
      sigma_mean[trait] <- fitsum["sigma", "Mean"]
      sigma_lo[trait] <- fitsum["sigma", "Lower95"]
      sigma_hi[trait] <- fitsum["sigma", "Upper95"]
    }
    result <- list(
      stats = list(
        mu = list(Mean = mu_mean, `2.5%` = mu_lo, `97.5%` = mu_hi),
        sigma = list(Mean = sigma_mean, `2.5%` = sigma_lo, `97.5%` = sigma_hi)
      )
    )
    fname <- sprintf("output/uni.%s.%s.%s.rds", ma, pft_type, pft)
    saveRDS(result, fname)
  }
}
