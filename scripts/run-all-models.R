library(shiklomanov2017np)

for (m in c("mass", "area")) {
  for (pft in abbr2pft[-1]) {
    if (pft == "global") pft <- NA
    callr::rscript(
      "scripts/run_model.R",
      cmdargs = c("multi", m, "clm45", pft)
    )
  }
}
