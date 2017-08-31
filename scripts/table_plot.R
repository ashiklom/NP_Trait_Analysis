library(nptraits)
library(magrittr)

try_data <- readRDS("extdata/traits_analysis.rds")

try_sub <- try_data %>%
    select(-ObservationID, -AccSpeciesID, -LMA) %>%
    select(jules1:custom, leaf_lifespan, SLA, Nmass, Narea, Pmass, Parea, Rdmass, Rdarea, Vcmax_mass, Vcmax_area, Jmax_mass, Jmax_area)

count_nas <- function(dat, pft) {
    dat %>%
        mutate(pft_scheme = pft) %>%
        rename_(.dots = list(pft = pft)) %>%
        group_by(pft_scheme, pft) %>%
        summarize_if(is.double, ~sum(!is.na(.))) %>%
        mutate(pft = as.character(pft))
}

na_counts <- map_df(pft_schemes, count_nas, dat = try_sub)

na_long <- na_counts %>%
    gather(variable, value, -pft, -pft_scheme) %>%
    mutate(variable = factor(variable) %>% forcats::fct_inorder())

na_nested <- na_long %>%
    group_by(pft_scheme) %>%
    nest()

missing_plot <- function(dat, pft_scheme, palette = "Paired") {
    dat %>%
        mutate(pft = factor(pft, pft_levels[[pft_scheme]]) %>% "levels<-"(names(pft_levels[[pft_scheme]]))) %>%
        ggplot() +
            aes(x = variable, y = value, color = pft) +
            geom_point(position = position_dodge(width = 0.5), size = 2) +
            scale_y_log10() +
            labs(x = 'Trait', y = 'Number of non-missing observations', title = pft_scheme) +
            theme(legend.position = 'bottom',
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_color_brewer(palette = palette)
}

na_plots <- na_nested %>%
    mutate(colors = c('Set1', 'Set1', 'Paired', 'Paired'),
           plot = pmap(list(data, pft_scheme, colors), missing_plot),
           filename = file.path('figures', paste('n_obs', pft_scheme, 'png', sep = '.')))

na_plots %$%
    pwalk(list(filename = filename, plot = plot), ggsave, width = 6, height = 4.5)
