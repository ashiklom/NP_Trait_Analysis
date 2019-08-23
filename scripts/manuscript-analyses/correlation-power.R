library(pwr)
out <- pwr.r.test(14, power = 0.95, sig.level = 0.05, alternative = "greater")

power_pft <- function(npft) {
  out <- numeric(length(npft))
  for (i in seq_along(npft)) {
    out[i] <- pwr.r.test(npft[[i]], power = 0.95, sig.level = 0.05, alternative = "greater")$r
  }
  out
}

curve(power_pft(x), 10, 100)
