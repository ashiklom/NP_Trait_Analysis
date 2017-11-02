library(tidyverse)

arg <- commandArgs(trailingOnly = TRUE)

outdir <- arg[1]
if (is.na(outdir)) {
  outdir <- "output"
}

cachedir <- arg[2]
if (is.na(cachedir)) {
  cachedir <- ".cache"
}

dir.create(cachedir, showWarnings = FALSE)
cachefile <- file.path(cachedir, "mvtraits_results.rds")

parse_filetag <- function(fname_list) {
  l <- lapply(fname_list, strsplit, split = "\\.")
  l <- lapply(l, unlist)
  d <- do.call(rbind, l)
  d <- cbind(d, fname_list)
  colnames(d) <- c("model_type", "mass_area", "pft_type",
                   "pft", "date", "ext", "fname")
  as_tibble(d)
}

outfiles <- dir(outdir) %>%
  parse_filetag() %>%
  arrange(desc(date)) %>%
  distinct(model_type, mass_area, pft_type, pft, .keep_all = TRUE) %>%
  mutate(fname = file.path(outdir, fname))

read_output <- function(fname) {
  readRDS(fname)[["stats"]]
}

dat_list <- sapply(outfiles$fname, read_output, simplify = FALSE, USE.NAMES = TRUE)
saveRDS(dat_list, cachefile)
