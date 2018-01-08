# Chunk name: samplesize
# Description: Figure of the sample sizes of each PFT

## ----options, echo=FALSE, message=FALSE----------------------------------
library(shiklomanov2017np)
try_data <- readRDS("extdata/traits_analysis.rds")

try_sub <- try_data %>%
  select(pft = clm45, one_of(both_params),
         -Latitude, -Longitude, -LMA)

sample_size <- try_sub %>%
  select(pft, one_of(both_params)) %>%
  mutate(
    pft = factor(pft, abbr2pft) %>% forcats::lvls_revalue(pft2abbr)
  ) %>%
  group_by(pft) %>%
  summarize_all(~sum(!is.na(.))) %>%
  gather("param", "sample_size", -pft) %>%
  mutate(param = factor(param, both_params))

## ----"samplesize", fig.cap='(ref:ssfigcap)', fig.width=7, fig.height=7----
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
ss_plot
