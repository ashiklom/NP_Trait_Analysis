library(mvtraits)
library(tidyverse)

source("scripts/pft_abbreviations.R")
cachefile <- ".cache/mvtraits_results.rds"
results_all <- readRDS(cachefile)
try_data <- readRDS("extdata/traits_analysis.rds")
try_sub <- try_data %>%
  select(pft = clm45, Jmax_area:Vcmax_mass,
         -Latitude, -Longitude, -LMA)

n_species <- try_data %>%
  distinct(clm45, AccSpeciesID) %>%
  select(pft = clm45, species = AccSpeciesID) %>%
  count(pft) %>%
  mutate(
    pft = as.character(pft),
    label = pft2abbr[pft],
    pft = tools::toTitleCase(gsub("_", " ", pft))
  ) %>%
  select(`Label` = label, `PFT` = pft, `Number of species` = n)
species_table <- "manuscript-md/pftdefinitions.md"
species_caption <- paste("Names, labels, and species counts",
                         "for plant functional types (PFTs)",
                         "used in this analysis.",
                         "{#tbl:pfts}")
if (FALSE) {
  sink(file = species_table)
  pander::pandoc.table(n_species, caption = species_caption, split.tables = Inf)
  sink(NULL)
}

sample_size <- try_sub %>%
  select(pft, one_of(both_params)) %>%
  mutate(
         pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
         ) %>%
  group_by(pft) %>%
  summarize_all(~sum(!is.na(.))) %>%
  gather("param", "sample_size", -pft) %>%
  mutate(param = factor(param, both_params))

ss_plot <- sample_size %>%
  mutate(param = forcats::lvls_revalue(param, param_simple[both_params])) %>%
  ggplot() + 
  aes(x = pft, y = sample_size, fill = pft) + 
  geom_col() +
  scale_y_log10(breaks = c(10, 100, 1000, 10000)) +
  facet_wrap(~param, ncol = 2, labeller = label_parsed) +
  scale_fill_manual(values = pft_colors) +
  xlab("Plant functional type") +
  ylab("Sample size") +
  guides(fill = guide_legend(title = "PFT")) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
#ss_plot
ggsave("figures/sample_size_bars.pdf", width = 7, height = 7)

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
    pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr),
    param = factor(param, both_params)
  )

# Absolute comparison of means
means_dat <- multi_summary %>%
  filter(pft != "GLOB") %>%
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

## Write summary table
table_file <- "manuscript-md/valuetable.md"
table_cap <- paste("Mean and 95% confidence interval of trait estimates",
                   "from the hierarchical model")
table_dat <- means_dat %>%
  filter(
    model_type == "hierarchical",
    !(param %in% c("SLA", "leaf_lifespan") & mass_area == "area")
  ) %>%
  mutate(
    lo_f = formatC(mu_lo, digits = 3),
    mid_f = formatC(mu_mean, digits = 3),
    hi_f = formatC(mu_hi, digits = 3),
    value = sprintf("%s (%s - %s)", mid_f, lo_f, hi_f)
  ) %>%
  select(pft, param, value) %>%
  mutate(
    param = factor(param, both_params) %>% 
      forcats::lvls_revalue(param_markdown[both_params]),
  )%>%
  spread(param, value) %>%
  arrange(pft)
if (FALSE) {
  sink(file = table_file)
  pander::pandoc.table(table_dat, caption = table_cap, split.tables = 80)
  sink(NULL)
}

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
    mu_hi = pclip(param, "Vcmax_mass", mu_hi, 100),
    mu_lo = pclip(param, "Vcmax_mass", mu_lo, 15, FALSE),
    mu_hi = pclip(param, "Vcmax_area", mu_hi, 2),
    mu_hi = pclip(param, "Jmax_mass", mu_hi, 2.0),
    mu_hi = pclip(param, "Jmax_area", mu_hi, 175),
    mu_lo = pclip(param, "Jmax_area", mu_lo, 40, FALSE),
    mu_hi = pclip(param, "Rdmass", mu_hi, 0.03),
    mu_hi = pclip(param, "Rdarea", mu_hi, 1.7),
    mu_lo = pclip(param, "Rdarea", mu_lo, 0.25, FALSE),
    mu_hi = pclip(param, "Parea", mu_hi, 0.275),
    param = forcats::lvls_revalue(param, param_fancy_chr[both_params])
  )

p_means <- ggplot(plot_dat) +
  aes(x = interaction(model_type, pft),
      y = mu_mean, ymin = mu_lo, ymax = mu_hi,
      color = pft, shape = model_type) +
  geom_pointrange(size = 0.15) +
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
    text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )
#p_means
ggsave("figures/mean_comparison.pdf", p_means, width = 7, height = 7)

  ## Relative uncertainty
cv_dat <- plot_dat %>%
  mutate(
    cv = abs((mu_hi - mu_lo) / mu_mean)
  ) %>%
  filter(sample_size > 3)

cv_mod <- cv_dat %>%
  group_by(model_type) %>%
  nest() %>%
  mutate(
    cvfit = map(data, ~lm(log10(cv) ~ log10(sample_size), data = .))
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
#relative_ci

ggsave("figures/relative_ci_sample_size.pdf", relative_ci,
       width = 4.4, height = 3)
