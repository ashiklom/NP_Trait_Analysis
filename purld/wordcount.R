## ----wordcount, echo = FALSE---------------------------------------------
fnames <- c(
  introduction = "01_introduction.md",
  methods = "02_methods.Rmd",
  results = "03_results.Rmd",
  discussion = "04_discussion.md",
  conclusions = "05_conclusions.md"
)
files <- file.path(
  "~/Projects/new-phytologist-traits/np-trait-analysis/manuscript-md",
  fnames
)
names(files) <- names(fnames)

getwc <- function(f) {
  ct <- withr::with_options(
    list(knitr.table.format = "pandoc"),
    wordcountaddin::text_stats(f)
  )
  wline <- ct[3]
  wlist <- strsplit(wline, split = "[[:space:]]+")
  as.numeric(wlist[[1]][3])
}

wc_table <- data.frame(wordcount = vapply(files, getwc, numeric(1)))
wc_total <- rbind(wc_table, sum(wc_table$wordcount))
wc_total$percent <- 100 * wc_total$wordcount /
  sum(wc_total$wordcount[-nrow(wc_total)])
rownames(wc_total)[nrow(wc_total)] <- "total"
knitr::kable(wc_total, digits = 1)
