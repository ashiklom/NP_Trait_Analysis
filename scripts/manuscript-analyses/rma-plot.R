library(shiklomanov2017np)

outdir <- here::here("output")
hier_mass_file <- tail(dir(outdir, "hier.mass.clm45", full.names = TRUE), 1)
hier_area_file <- tail(dir(outdir, "hier.area.clm45", full.names = TRUE), 1)

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

tidy_rma <- function(output, mass_area) {
  raw_result <- lapply(output$samples, all_rma_slopes) %>%
    do.call(what = rbind)
  raw_means <- colMeans(raw_result)
  rma_q <- t(apply(raw_result, 2, quantile, c(0.025, 0.975))) %>%
    tibble::as_tibble(rownames = "colname") %>%
    dplyr::rename(lo = `2.5%`, hi = `97.5%`)
  rma_df <- tibble::enframe(raw_means, "colname", "Mean") %>%
    dplyr::left_join(rma_q, "colname")
  param <- switch(
    mass_area,
    mass = shiklomanov2017np::mass_params,
    area = shiklomanov2017np::area_params
  )
  rma_split <- rma_df %>%
    tidyr::separate(colname, c("stat", "pft", "xvar", "yvar"),
                    sep = "\\.\\.") %>%
    dplyr::mutate(
      mass_area = !!mass_area,
      yvar = factor(yvar, param),
      xvar = factor(xvar, param),
      pft = forcats::fct_inorder(pft)
    )
  rma_split
}

rma_mass <- readRDS(hier_mass_file) %>% tidy_rma("mass")
rma_area <- readRDS(hier_area_file) %>% tidy_rma("area")
rma_both <- bind_rows(rma_mass, rma_area) %>%
  mutate(
    xvar2 = stringr::str_remove(xvar, "_?(mass|area)") %>%
      forcats::fct_inorder(),
    yvar2 = stringr::str_remove(yvar, "_?(mass|area)") %>%
      forcats::fct_inorder(),
    pft = forcats::fct_recode(pft, !!!abbr2pft),
    significant = sign(lo) == sign(hi),
    sigstar = if_else(significant, "*", NA_character_)
  )

plt <- ggplot(rma_both) +
  aes(x = pft, y = "", fill = Mean, label = sigstar) +
  geom_tile() +
  geom_text(size = 6) +
  facet_nested(yvar2 + xvar2 ~ mass_area, switch = "y") +
  labs(
    x = "Plant functional type",
    y = "Trait pair",
    fill = "RMA Slope"
  ) +
  scale_fill_gradient2(low = "red4", mid = "white", high = "blue4") +
  cowplot::theme_cowplot() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.spacing.y = unit(0.05, "lines"),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 180)
  )

ggsave("figures/manuscript/rma-slope-tiles.pdf", plt,
       width = 13.5, height = 7.9)
