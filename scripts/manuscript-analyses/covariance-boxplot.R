library(shiklomanov2017np)

outdir <- here::here("output")
hier_mass_file <- tail(dir(outdir, "hier.mass.clm45", full.names = TRUE), 1)
hier_area_file <- tail(dir(outdir, "hier.area.clm45", full.names = TRUE), 1)
multi_mass_file <- tail(dir(outdir, "multi.mass.clm45.NA", full.names = TRUE), 1)
multi_area_file <- tail(dir(outdir, "multi.area.clm45.NA", full.names = TRUE), 1)

hier_mass <- readRDS(hier_mass_file)
samples <- hier_mass$samples[[1]]

rma_slope <- function(m, pft, yvar, xvar) {
  sdy_col <- paste("Sigma", pft, yvar, yvar, sep = "..")
  sdx_col <- paste("Sigma", pft, xvar, xvar, sep = "..")
  mcol <- paste("Sigma", pft, xvar, yvar, sep = "..")
  sign(m[, mcol]) * sqrt(m[, sdy_col] / m[, sdx_col])
}

all_rma_slopes <- function(samples) {
  sigma_cols <- grep("^Sigma..", colnames(samples), value = TRUE)
  chars <- do.call(rbind, strsplit(sigma_cols, "..", fixed = TRUE))
  pfts <- unique(chars[, 2])
  traits <- unique(chars[, 3])
  xyvars <- combn(traits, 2)
  yvars <- xyvars[1, ]
  xvars <- xyvars[2, ]
  l <- list()
  for (p in pfts) {
    l <- c(l, purrr::pmap(list(yvar = yvars, xvar = xvars),
                          rma_slope,
                          pft = p,
                          m = samples))
  }
  rma_cols <- gsub("Sigma", "RMA", sigma_cols)[chars[, 3] != chars[, 4]]
  out <- do.call(cbind, l)
  colnames(out) <- rma_cols
  out
}

rma_result <- lapply(hier_mass$samples, all_rma_slopes) %>%
  do.call(what = rbind)

rma_means <- colMeans(rma_result)
rma_q <- t(apply(rma_result, 2, quantile, c(0.025, 0.975))) %>%
  tibble::as_tibble(rownames = "colname") %>%
  dplyr::rename(lo = `2.5%`, hi = `97.5%`)
rma_df <- tibble::enframe(rma_means, "colname", "Mean") %>%
  dplyr::left_join(rma_q, "colname")

rma_split <- rma_df %>%
  tidyr::separate(colname, c("stat", "pft", "xvar", "yvar"),
                  sep = "\\.\\.") %>%
  dplyr::mutate(
    yvar = factor(yvar, mass_params),
    xvar = factor(xvar, mass_params),
    pft = forcats::fct_inorder(pft)
  )

rma_split %>%
  filter(sign(lo) == sign(hi)) %>%
  ggplot() +
  aes(x = pft, y = Mean, ymin = lo, ymax = hi, fill = pft) +
  geom_col() +
  geom_errorbar() +
  facet_grid(vars(yvar), vars(xvar), scales = "free_y") +
  scale_fill_manual(values = pft_colors) +
  theme(axis.text.x = element_blank())

hier_summary_mass <- readRDS(hier_mass_file)[["summary_table"]] %>%
  mutate(mass_area = "mass")
hier_summary_area <- readRDS(hier_area_file)[["summary_table"]] %>%
  mutate(mass_area = "area")
## multi_summary_mass <- readRDS(multi_mass_file)[["summary_table"]] %>%
##   mutate(mass_area = "mass")
## multi_summary_area <- readRDS(multi_area_file)[["summary_table"]] %>%
##   mutate(mass_area = "area")

proc <- function(dat, hier = TRUE, variable = c("Corr", "Sigma")) {
  variable <- match.arg(variable)
  ## if (hier) {
  ##   dat <- filter(dat, group != "global")
  ## } else {
  ##   dat <- mutate(dat, group = "global")
  ## }
  dat <- dat %>%
    filter(variable == !!variable) %>%
    separate(index, c("xvar", "yvar"), sep = "\\.\\.") %>%
    mutate(
      pft = factor(group, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
    ) %>%
    select(mass_area, pft, xvar, yvar, Mean, `2.5%`, `97.5%`)
}

hsm <- proc(hier_summary_mass, variable = "Sigma")
hsa <- proc(hier_summary_area, variable = "Sigma")
## msm <- proc(multi_summary_mass, FALSE, variable = "Sigma")
## ## msa <- proc(multi_summary_area, FALSE, variable = "Sigma")
## hs <- bind_rows(hsm, hsa, msm, msa)
hs <- bind_rows(hsm, hsa)

sigbar <- function(xvar, yvar, mass_area) {
  hss <- hs %>% filter(mass_area == !!mass_area)
  # Variance in X and Y
  varx <- hss %>%
    filter(xvar == !!xvar & yvar == !!xvar) %>%
    arrange(pft)
  vary <- hss %>%
    filter(xvar == !!yvar & yvar == !!yvar) %>%
    arrange(pft)
  sxy <- hss %>%
    filter((xvar == !!xvar & yvar == !!yvar) |
             (xvar == !!yvar & yvar == !!xvar)) %>%
    arrange(pft)
  vcols <- c("Mean", "2.5%", "97.5%")
  rma_slope <- sxy %>%
    mutate(color = c("grey25", pft_colors))
  rma_slope[, vcols] <- sign(sxy[, vcols]) *
    sqrt(varx[, vcols]) / sqrt(vary[, vcols])
  rma_slope[["significant"]] <-  sign(rma_slope[["2.5%"]]) == sign(rma_slope[["97.5%"]])
  ggplot(rma_slope) +
    aes(x = pft, y = Mean, ymin = `2.5%`, ymax = `97.5%`,
        fill = color) +
    geom_col() +
    geom_text(label = "**", size = 8, data = filter(rma_slope, significant)) +
    ## geom_errorbar() +
    ## geom_col(aes(alpha = significant)) +
    ## scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
    scale_fill_identity() +
    theme_classic()
}

sigbar("Nmass", "Rdmass", "mass")

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

pdf(
  here("figures", "manuscript", "covariance_boxplot.pdf"),
  width = 7,
  height = 7
)

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

dev.off()
