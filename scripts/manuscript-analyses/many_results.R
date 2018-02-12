library(shiklomanov2017np)

## ----prepresults---------------------------------------------------------
cachefile <- here::here(".cache/mvtraits_results.rds")
results_all <- readRDS(cachefile)

max_ss <- max(sample_size$sample_size)

multi_summary <- results_all %>%
  filter(model_type %in% c("uni", "multi")) %>%
  mutate(
    mu_mean = map(data, c("mu", "Mean")) %>%
      map(~as_tibble(as.list(.))),
    mu_lo = map(data, c("mu", "2.5%")) %>%
      map(~as_tibble(as.list(.))),
    mu_hi = map(data, c("mu", "97.5%")) %>%
      map(~as_tibble(as.list(.)))
      ) %>%
  select(model_type:pft, mu_mean:mu_hi) %>%
  unnest(.sep = ".") %>%
  gather("variable", "value", -(model_type:pft), na.rm = TRUE) %>%
  separate(variable, c("stat", "param"), sep = "\\.") %>%
  spread(stat, value) %>%
  mutate(
    pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr),
    param = factor(param, both_params)
  )

hier_prep <- results_all %>%
  filter(model_type == "hier") %>%
  select(model_type:pft_type, data) %>%
  mutate(
    mu_vals = map(data, "mu_group"),
    pft = map(mu_vals, names),
    mu_df = map2(pft, mu_vals, ~tibble(pft = ..1, dat = ..2))
  ) %>%
  select(-data, -mu_vals, -pft) %>%
  unnest()

hier_summary <- hier_prep %>%
  mutate(
    mu_mean = map(dat, c("Mean")) %>%
      map(~as_tibble(as.list(.))),
    mu_lo = map(dat, c("2.5%")) %>%
      map(~as_tibble(as.list(.))),
    mu_hi = map(dat, c("97.5%")) %>%
      map(~as_tibble(as.list(.)))
      ) %>%
  select(model_type:pft, mu_mean:mu_hi) %>%
  unnest(.sep = ".") %>%
  gather("variable", "value", -(model_type:pft), na.rm = TRUE) %>%
  separate(variable, c("stat", "param"), sep = "\\.") %>%
  spread(stat, value) %>%
  mutate(
    pft = if_else(is.na(pft), "global", pft),
    pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr),
    param = factor(param, both_params)
  )

# Absolute comparison of means
means_dat <- multi_summary %>%
  filter(pft != "GLOB", !is.na(pft)) %>%
  bind_rows(hier_summary) %>%
  left_join(sample_size) %>%
  mutate(
    model_type = factor(model_type, c("uni", "multi", "hier")) %>%
      forcats::lvls_revalue(c("univariate", "multivariate", "hierarchical")),
    mass_area = factor(mass_area, c("mass", "area")),
  ) %>%
  mutate_at(
    c("mu_mean", "mu_lo", "mu_hi"),
    ~10 ^ .
  ) %>%
  filter(
    !(param %in% c("leaf_lifespan", "SLA") & mass_area == "area")
  )

## ----meanvaluetable, results = "asis"------------------------------------
## Write summary table
table_cap_mass <- paste(
  "Mean and 95\\% confidence interval of trait estimates",
  "for mass-normalized traits from the hierarchical model."
)
table_cap_area <- gsub("mass", "area", table_cap_mass)

censor <- function(rxp, pft, pfts, param, value) {
  if_else(grepl(rxp, param) & pfts %in% pft, "---", value)
}

table_dat <- means_dat %>%
  filter(
    model_type == "hierarchical",
    !(param %in% c("SLA", "leaf_lifespan") & mass_area == "area")
  ) %>%
  mutate(
    mu_lo = if_else(param == "Rdmass", mu_lo * 1000, mu_lo),
    mu_mean = if_else(param == "Rdmass", mu_mean * 1000, mu_mean),
    mu_hi = if_else(param == "Rdmass", mu_hi * 1000, mu_hi),
    mu_lo = if_else(param == "Rdarea", mu_lo * 1000, mu_lo),
    mu_mean = if_else(param == "Rdarea", mu_mean * 1000, mu_mean),
    mu_hi = if_else(param == "Rdarea", mu_hi * 1000, mu_hi),
    lo_f = formatC(mu_lo, digits = 3),
    mid_f = formatC(mu_mean, digits = 3),
    hi_f = formatC(mu_hi, digits = 3),
    value = sprintf("%s (%s,%s)", mid_f, lo_f, hi_f),
    value = censor("Vcmax|Jmax", "C4G", pft, param, value),
    value = censor("Rdmass", "ShDBo", pft, param, value),
    value = censor("Jmax_mass", c("NlEBo", "NlD"), pft, param, value),
    value = censor("Jmax_area", c("NlD"), pft, param, value)
  ) %>%
  select(pft, param, value) %>%
  mutate(
    param = factor(param, both_params) %>% 
      forcats::lvls_revalue(param_markdown_nounit[both_params]),
  )%>%
  spread(param, value) %>%
  arrange(pft)

table_dat %>%
  select(-matches("area")) %>%
  kable(caption = table_cap_mass, format = "latex",
        escape = FALSE, booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = c("scale_down"))
table_dat %>%
  select(-matches("mass")) %>%
  kable(caption = table_cap_area, format = "latex",
        escape = FALSE, booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = c("scale_down"))

## ----meancompare, fig.width=7, fig.height=7, fig.cap='(ref:mccap)'-------
pclip <- function(p, pname, value, lim, hi = TRUE) {
  f <- ifelse(hi, `>`, `<`)
  if_else(p == pname & f(value, lim), lim, value)
}

plot_dat <- means_dat %>%
  mutate(
    #too_few = sample_size < 2,
    #mu_hi = if_else(too_few, NA_real_, mu_hi),
    #mu_lo = if_else(too_few, NA_real_, mu_lo),
    #mu_mean = if_else(too_few, NA_real_, mu_mean),
    irrelevant = grepl("Vcmax|Jmax", param) & pft == "C4G",
    mu_hi = if_else(irrelevant, NA_real_, mu_hi),
    mu_lo = if_else(irrelevant, NA_real_, mu_lo),
    mu_mean = if_else(irrelevant, NA_real_, mu_mean),
    ## TODO: Fix these in the prior and re-run
    mu_hi = pclip(param, "Vcmax_mass", mu_hi, 1.5),
    mu_hi = pclip(param, "Vcmax_area", mu_hi, 100),
    mu_hi = pclip(param, "Jmax_mass", mu_hi, 1.8),
    mu_hi = pclip(param, "Jmax_area", mu_hi, 150),
    mu_mean = pclip(param, "Jmax_area", mu_mean, 150),
    mu_hi = pclip(param, "Rdmass", mu_hi, 0.03),
    mu_mean = pclip(param, "Rdmass", mu_mean, 0.03),
    mu_lo = pclip(param, "Rdmass", mu_lo, 0.03),
    mu_hi = pclip(param, "Rdarea", mu_hi, 0.0025),
    mu_mean = pclip(param, "Rdarea", mu_mean, 0.0025),
    mu_lo = pclip(param, "Rdarea", mu_lo, 0.0025),
    mu_hi = pclip(param, "Parea", mu_hi, 0.285),
    #mu_lo = pclip(param, "Jmax_area", mu_lo, 40, FALSE),
    param = forcats::lvls_revalue(param, param_fancy_chr[both_params])
  )

p_means <- ggplot(plot_dat) +
  aes(x = interaction(model_type, pft),
      y = mu_mean, ymin = mu_lo, ymax = mu_hi,
      color = pft, shape = model_type) +
  geom_pointrange(size = 0.3) +
  facet_wrap(~param, scales = "free", ncol = 2,
              labeller = label_parsed) +
  scale_y_continuous() +
  scale_color_manual(values = pft_colors) +
  guides(
    shape = guide_legend(title = "Model type"),
    color = guide_legend(title = "PFT")
  ) +
  ylab("Trait estimate mean and 95% CI") +
  xlab("Model type and PFT") +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )
p_means

## ----"civssamplesize", fig.width=4.4, fig.height=3, fig.cap='(ref:sscap)'----
## Relative uncertainty
cv_dat <- means_dat %>%
  mutate(
    cv = abs((mu_hi - mu_lo) / mu_mean)
  ) %>%
  filter(sample_size > 1)

cv_mod <- cv_dat %>%
  group_by(model_type) %>%
  nest() %>%
  mutate(
    cvfit = map(data, ~lm(log10(cv) ~ log10(sample_size), data = .))
  )

cv_coef <- cv_mod %>%
  mutate(
    coef = map(cvfit, broom::tidy)
  ) %>%
  unnest(coef) %>%
  select(model_type, term, estimate) %>%
  spread(term, estimate) %>%
  "names<-"(c("model_type", "intercept", "slope")) %>%
  mutate(
    eqn = sprintf("'%s: '~log[10](y) == %0.2f - %0.2f~log[10](x)", model_type, intercept, abs(slope))
  )

cv_pred <- cv_mod %>%
  mutate(
    grid = map(data, modelr::data_grid, sample_size = seq(5, max_ss)),
    cvpred = map2(grid, cvfit, modelr::add_predictions)
  ) %>%
  unnest(cvpred) %>%
  mutate(cv = 10 ^ pred)
  
x_lims <- range(cv_pred$sample_size)
y_lims <- range(cv_pred$cv)

relative_ci <- ggplot(cv_dat) +
  aes(x = sample_size, y = cv, color = model_type) +
  geom_point(size = 0.5) +
  geom_line(data = cv_pred) +
  annotate("text", x = rep(2500, 3), y = c(1.4, 1.25, 1.1), 
           label = cv_coef$eqn, parse = TRUE, 
           size = 2, vjust = "right") +
  scale_x_continuous(trans = "log10") +
  coord_cartesian(xlim = x_lims, ylim = y_lims) +
  xlab("Sample size") +
  ylab("Relative width of 95% CI") +
  scale_color_brewer(type = "qual", palette = 2) +
  guides(color = guide_legend(title = "Model type")) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.75),
    panel.grid = element_blank()
  )
relative_ci

## ----stickpairs, fig.width = 5, fig.height = 5, fig.cap = '(ref:stickcap)'----
suppressPackageStartupMessages({
  library(mvtraits)
})
results_sub <- results_all %>%
  filter((model_type == "multi" & pft == "global") |
         (model_type == "hier"))
dat_list <- results_sub$data
names(dat_list) <- with(results_sub, paste(model_type, mass_area, sep = "_"))

stickplot_pairs(
  #mu_global_lower = dat_list$hier_mass$mu_global,
  #Sigma_global_lower = dat_list$hier_mass$Sigma_global,
  mu_global_lower = dat_list$multi_mass$mu,
  Sigma_global_lower = dat_list$multi_mass$Sigma,
  mu_group_lower = dat_list$hier_mass$mu_group,
  Sigma_group_lower = dat_list$hier_mass$Sigma_group,
  vars_lower = mass_params,
  group_names = pft2abbr[-1],
  vars_label = param_labels,
  unlog_axes = TRUE,
  #mu_global_upper = dat_list$hier_area$mu_global,
  #Sigma_global_upper = dat_list$hier_area$Sigma_global,
  mu_global_upper = dat_list$multi_area$mu,
  Sigma_global_upper = dat_list$multi_area$Sigma,
  mu_group_upper = dat_list$hier_area$mu_group,
  Sigma_group_upper = dat_list$hier_area$Sigma_group,
  vars_upper = area_params,
  col_group = pft_colors,
  cex_g = 1.2,
  lwd_g = 1,
  lty_g = 1,
  lwd_s = 1.3,
  par_plot = list(oma = c(0, 5, 3, 5), cex = 0.4),
  par_legplot = list(mar = c(0, 0, 0, 0)),
  par_legend = list(ncol = 5, x = "center", cex = 0.6),
  reorder_legend = TRUE
)
mtext("Mass-normalized", side = 2, cex = 0.7, line = 3)
mtext("Area-normalized", side = 4, cex = 0.7, line = 1)