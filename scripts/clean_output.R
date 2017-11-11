library(mvtraits)

try_dat <- readRDS("extdata/traits_analysis.rds")
pfts <- levels(try_dat$clm45)
group_names <- sprintf("group%02d", seq_along(pfts))

fname <- "output/hier.area.clm45.NA.2017-10-25-1632.rds"

out <- readRDS(fname)

summary_table <- out$summary_table

### Fix names
group_vars <- c("mu_group", "Sigma_group", "Corr_group")
clean_out$means[group_vars] <- lapply(
  clean_out$means[group_vars],
  "names<-",
  pfts
)
names(out)[2] <- "stats"
saveRDS(out, fname)

