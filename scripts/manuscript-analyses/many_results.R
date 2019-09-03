# Chunk name: samplesize
# Description: Figure of the sample sizes of each PFT

## ----options, echo=FALSE, message=FALSE----------------------------------
library(shiklomanov2017np)

manuscript_fig_dir <- here("figures", "manuscript")
dir.create(manuscript_fig_dir, showWarnings = FALSE)

try_sub <- try_data()

sample_size <- try_sub %>%
  select(pft, one_of(both_params)) %>%
  mutate(
    pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
  ) %>%
  group_by(pft) %>%
  summarize_all(~sum(!is.na(.))) %>%
  gather("param", "sample_size", -pft) %>%
  mutate(param = factor(param, both_params))

## ----prepresults---------------------------------------------------------
resultsfile <- here("results", "mvtraits_results.rds")
results_all <- readRDS(resultsfile)

max_ss <- max(sample_size$sample_size)

multi_summary <- results_all %>%
  filter(model_type %in% c("uni", "multi")) %>%
  mutate(
    mu_mean = map(data, c("result", "mu", "Mean")) %>%
      map(~as_tibble(as.list(.))),
    mu_lo = map(data, c("result", "mu", "2.5%")) %>%
      map(~as_tibble(as.list(.))),
    mu_hi = map(data, c("result", "mu", "97.5%")) %>%
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
    mu_vals = map(data, c("result", "mu_group")),
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
    ## value = censor("Rdmass", "ShDBo", pft, param, value),
    ## value = censor("Jmax_mass", c("NlEBo", "NlD"), pft, param, value),
    ## value = censor("Jmax_area", c("NlD"), pft, param, value)
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
#table_dat %>%
  #select(-matches("mass")) %>%
  #kable(caption = table_cap_area, format = "latex",
        #escape = FALSE, booktabs = TRUE) %>%
  #kableExtra::kable_styling(latex_options = c("scale_down"))

## ----meancompare, fig.width=7, fig.height=7, fig.cap='(ref:mccap)'-------
pclip <- function(p, pname, value, lim, hi = TRUE) {
  f <- ifelse(hi, `>`, `<`)
  if_else(p == pname & f(value, lim), lim, value)
}

model_type_levels <- c(levels(means_dat$model_type), "CLM 4.5")

clm_dat <- clm45_table81 %>%
  mutate(
    Nmass = 0.46 / CN_leaf,         # g N g-1 leaf
    Narea = Nmass / SLA,            # g N m-2 leaf
    Vcmax_mass = Vcmax_25 * SLA,    # umol g-1 s-1
    SLA = SLA * 1000,               # m2 kg-1
    model_type = factor("CLM 4.5", model_type_levels),
    pft = factor(pft, abbr2pft) %>% lvls_revalue(pft2abbr)
  ) %>%
  select(pft, model_type, SLA, Nmass, Narea, Vcmax_mass, Vcmax_area = Vcmax_25) %>%
  gather(key = "param", value = "mu_mean", -pft, -model_type) %>%
  mutate(
    param = factor(param, both_params),
    mu_hi = mu_mean,
    mu_lo = mu_mean
  )

param_fancy_2 <- c(
  leaf_lifespan = "atop(Leaf~lifespan, (months))",
  Nmass = "atop(N[mass], (mg ~ g ^ -1))",
  Pmass = "atop(P[mass], (mg ~ g ^ -1))",
  SLA = "atop(SLA, (m ^ 2 ~ kg ^ -1))",
  Rdmass = "atop(R[list(d,mass)], (mu * mol ~ g ^ -1 ~ s ^ -1))",
  Vcmax_mass = "atop(V[list(c, max, mass)], (mu * mol ~ g ^ -1 ~ s ^ -1))",
  Jmax_mass = "atop(J[list(max, mass)], (mu * mol ~ g ^ -1 ~ s ^ -1))",
  # TODO: Fix these
  Narea = "atop(N[area], (g ~ m ^ -2))",
  Parea = "atop(P[area], (g ~ m ^ -2))",
  Rdarea = "atop(R[{list(d, area)}], (mu * mol ~ m ^ -2 ~ s ^ -1))",
  Vcmax_area = "atop(V[{list(c, max, area)}], (mu * mol ~ m ^ -2 ~ s ^ -1))",
  Jmax_area = "atop(J[{list(max, area)}], (mu * mol ~ m ^ -2 ~ s ^ -1))"
)
with_clm <- means_dat %>%
  mutate(model_type = factor(model_type, model_type_levels)) %>%
  bind_rows(clm_dat)

saveRDS(with_clm, here::here("extdata", "tidy-means.rds"))

plot_dat <- with_clm %>%
  mutate(
    # Convert Nmass and Pmass to correct units
    mu_mean = if_else(param %in% c("Nmass", "Pmass"), mu_mean * 1000, mu_mean),
    mu_hi = if_else(param %in% c("Nmass", "Pmass"), mu_hi * 1000, mu_hi),
    mu_lo = if_else(param %in% c("Nmass", "Pmass"), mu_lo * 1000, mu_lo),
    irrelevant = grepl("Vcmax|Jmax", param) & pft == "C4G",
    mu_hi = if_else(irrelevant, NA_real_, mu_hi),
    mu_lo = if_else(irrelevant, NA_real_, mu_lo),
    mu_mean = if_else(irrelevant, NA_real_, mu_mean),
    ## TODO: Fix these in the prior and re-run
    mu_hi = pclip(param, "Rdmass", mu_hi, 50),
    mu_hi = pclip(param, "Rdarea", mu_hi, 4),
    mu_hi = pclip(param, "Vcmax_mass", mu_hi, 5),
    mu_mean = pclip(param, "Vcmax_mass", mu_mean, 5),
    mu_hi = pclip(param, "Vcmax_area", mu_hi, 120),
    mu_hi = pclip(param, "Jmax_mass", mu_hi, 4),
    mu_hi = pclip(param, "Jmax_area", mu_hi, 300),
    param = forcats::lvls_revalue(param, param_fancy_2[both_params])
  )
p_means <- ggplot(filter(plot_dat, model_type %in% c("hierarchical", "CLM 4.5"))) +
  aes(
    x = pft,
    y = mu_mean,
    ymin = mu_lo,
    ymax = mu_hi,
    color = pft,
    shape = model_type
  ) +
  geom_pointrange(size = 0.3) +
  facet_wrap(~param, scales = "free", ncol = 2,
             labeller = label_parsed,
             strip.position = "left") +
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_color_manual(values = pft_colors) +
  scale_shape_manual(values = c(
    "univariate" = 17,
    "multivariate" = 15,
    "hierarchical" = 20,
    "CLM 4.5" = 4
  )) +
  guides(
    shape = guide_legend(title = "Model type",
                         nrow = 1,
                         override.aes = list(linetype = 0)),
    color = guide_legend(title = "PFT",
                         byrow = TRUE,
                         ncol = 8,
                         label.position = "bottom",
                         keywidth = 2.5)
  ) +
  ylab("Trait estimate mean and 95% CI") +
  xlab("Plant functional type") +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_text(),
    axis.title.y.right = element_text(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    strip.placement = "outside",
    strip.background = element_blank(),
  )
if (interactive()) {
  p_means
}
ggsave(file.path(manuscript_fig_dir, "mean_comparison.pdf"), p_means,
       width = 6.44, height = 8.1)

p_means_all <- ggplot(plot_dat) +
  aes(
    x = interaction(model_type, pft),
    y = mu_mean,
    ymin = mu_lo,
    ymax = mu_hi,
    color = pft,
    shape = model_type
  ) +
  geom_pointrange(size = 0.3) +
  facet_wrap(~param, scales = "free", ncol = 2,
             labeller = label_parsed,
             strip.position = "left") +
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_color_manual(values = pft_colors) +
  scale_shape_manual(values = c(
    "univariate" = 17,
    "multivariate" = 15,
    "hierarchical" = 20,
    "CLM 4.5" = 4
  )) +
  guides(
    shape = guide_legend(title = "Model type",
                         nrow = 1,
                         override.aes = list(linetype = 0)),
    color = guide_legend(title = "PFT",
                         byrow = TRUE,
                         ncol = 8,
                         label.position = "bottom",
                         keywidth = 2.5)
  ) +
  ylab("Trait estimate mean and 95% CI") +
  xlab("Plant functional type and model type") +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_text(),
    axis.title.y.right = element_text(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    strip.placement = "outside",
    strip.background = element_blank(),
  )
if (interactive()) p_means_all
ggsave(file.path(manuscript_fig_dir, "mean_comparison_all.pdf"), p_means_all,
       width = 8.9, height = 9.375)

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
if (interactive())
  relative_ci

ggsave(file.path(manuscript_fig_dir, "relative_ci_model.pdf"),
       width = 4.4, height = 3)

## ----stickpairs, fig.width = 5, fig.height = 5, fig.cap = '(ref:stickcap)'----
results_sub <- results_all %>%
  filter((model_type == "multi" & pft == "global") |
         (model_type == "hier"))
dat_list <- map(results_sub$data, "result")
names(dat_list) <- with(results_sub, paste(model_type, mass_area, sep = "_"))

if (!interactive()) pdf(file.path(manuscript_fig_dir, "stick_pairs.pdf"),
                        width = 5, height = 5)
stickplot_pairs(
  mu_global_lower = dat_list$multi_mass$mu,
  Sigma_global_lower = dat_list$multi_mass$Sigma,
  mu_group_lower = dat_list$hier_mass$mu_group,
  Sigma_group_lower = dat_list$hier_mass$Sigma_group,
  vars_lower = mass_params,
  group_names = pft2abbr[-1],
  vars_label = param_labels,
  unlog_axes = TRUE,
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
if (!interactive()) dev.off()
