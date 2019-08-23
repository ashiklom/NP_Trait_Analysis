library(shiklomanov2017np)

manuscript_fig_dir <- here("figures", "manuscript")
dir.create(manuscript_fig_dir, showWarnings = FALSE)

resultsfile <- here("results", "mvtraits_results.rds")
results_all <- readRDS(resultsfile)

results_hier <- results_all %>%
  filter(model_type == "hier")

rh_list <- map(results_hier[["data"]], "result")

xx <- rh_list %>%
  pluck(2, "Sigma_global", list("Mean", )) %>%
  .["SLA", "Nmass"]

x <- rh_list %>%
  pluck(2, "Sigma_group") %>%
  map("Mean") %>%
  map(~.["SLA", "Nmass"]) %>%
  bind_rows() %>%
  gather("pft", "sigma")

ggplot(x) +
  aes(x = pft, y = sigma) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

rh_list
