library(shiklomanov2017np)

## ----boxprep-------------------------------------------------------------
outdir <- here::here("output")
hier_mass_file <- tail(dir(outdir, "hier.mass.clm45", full.names = TRUE), 1)
hier_area_file <- tail(dir(outdir, "hier.area.clm45", full.names = TRUE), 1)
multi_mass_file <- tail(dir(outdir, "multi.mass.clm45", full.names = TRUE), 1)
multi_area_file <- tail(dir(outdir, "multi.area.clm45", full.names = TRUE), 1)

hier_summary_mass <- readRDS(hier_mass_file)[["summary_table"]] %>%
  mutate(mass_area = "mass")
hier_summary_area <- readRDS(hier_area_file)[["summary_table"]] %>%
  mutate(mass_area = "area")
multi_summary_mass <- readRDS(multi_mass_file)[["summary_table"]] %>%
  mutate(mass_area = "mass")
multi_summary_area <- readRDS(multi_area_file)[["summary_table"]] %>%
  mutate(mass_area = "area")

proc <- function(dat, hier = TRUE) {
  if (hier) {
    dat <- filter(dat, group != "global")
  } else {
    dat <- mutate(dat, group = "global")
  }
  dat <- dat %>%
    filter(variable == "Corr") %>%
    separate(index, c("xvar", "yvar"), sep = "\\.\\.") %>%
    mutate(
      pft = factor(group, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
    ) %>%
    select(mass_area, pft, xvar, yvar, Mean, `2.5%`, `97.5%`)
}

hsm <- proc(hier_summary_mass)
hsa <- proc(hier_summary_area)
msm <- proc(multi_summary_mass, FALSE)
msa <- proc(multi_summary_area, FALSE)
hs <- bind_rows(hsm, hsa, msm, msa)

## ----corrbars, fig.width=7, fig.height=7, fig.cap='(ref:corrbarscap)'----
.zzz <- close.screen(all.screens = TRUE)
corr_bar <- function(dat, xvar, yvar, mass_area) {
  dp <- dat %>%
    filter(
      mass_area == !!mass_area,
      (xvar == !!xvar & yvar == !!yvar) | 
        (xvar == !!yvar & yvar == !!xvar),
    ) %>%
    arrange(pft) %>%
    mutate(color = c("grey25", pft_colors))
  x <- barplot(dp$Mean, col = dp$color, border = dp$color,
               ylim = c(-1, 1), axes = FALSE)
  arrows(x, dp[["2.5%"]], x, dp[["97.5%"]],
         length = 0.01, angle = 90, code = 3)
  box()
}

if (!interactive()) {
  pdf(
    here("figures", "manuscript", "correlation_boxplot.pdf"),
    width = 7,
    height = 7
  )
}

nv <- length(mass_params)
main_screens <- split.screen(rbind(c(0, 1, 0.2, 1), c(0, 1, 0, 0.2)))
par(mar = rep(0.1, 4), oma = c(0, 8, 1, 5), cex = 0.4)
screens <- split.screen(c(nv, nv), screen = main_screens[1])
for (i in seq_len(nv)) {
  for (j in seq_len(nv)) {
    k <- j + nv * (i - 1)
    screen(screens[k])
    if (i == j) {
      # Label
      plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
      box()
      text(x = 0.5, y = 0.5, labels = param_labels[i], cex = 2)
    } else {
      if (j > i) {
        # Upper triangle
        mass_area <- "area"
        xvar <- area_params[j]
        yvar <- area_params[i]
      } else {
        # Lower triangle
        mass_area <- "mass"
        xvar <- mass_params[j]
        yvar <- mass_params[i]
      }
      corr_bar(hs, xvar, yvar, mass_area)
    }
    if (i == 2 && j == 1) {
      axis(side = 2)
    }
    if (i == 1 && j == nv) {
      axis(side = 4)
    }
  }
}
## Draw legend
screen(main_screens[2])
par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
legend("center", legend = levels(hs$pft), fill = c("grey25", pft_colors),
       ncol = 5, bty = "n")
close.screen(all.screens = TRUE)
mtext("Mass-normalized", side = 2, cex = 0.7, line = 1)
mtext("Mean and 95% CI of pairwise correlation", side = 2, cex = 1, line = 2)
mtext("Area-normalized", side = 4, cex = 0.7, line = 0)
if (!interactive()) {
  dev.off()
}
