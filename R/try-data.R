#' Load TRY data from `extdata`
#'
#' @param mass_area (Optional) Retrieve only `"mass"` or `"area"` normalized
#'   traits
#' @param f Path to data file. Default is `here(extdata/data_anonymized.csv)`
#' @return `tibble` containing a `pft` column and the trait data (one trait per
#'   column)
#' @author Alexey Shiklomanov
#' @importFrom magrittr %>%
#' @export
try_data <- function(mass_area = NULL,
                     f = here::here("extdata", "data_anonymized.csv")) {
  if (!file.exists(f)) {
    stop("Input file ", shQuote(f), " does not exist.")
  }
  dat <- readr::read_csv(f, col_types = c("PFT" = "c",
                                          .default = "n")) %>%
    dplyr::mutate(PFT = factor(PFT, abbr2pft[-1])) %>%
    dplyr::rename(pft = PFT)
  if (is.null(mass_area)) return(dat)
  rxp <- switch(mass_area,
                mass = "leaf_lifespan|SLA|mass",
                area = "leaf_lifespan|SLA|area")
  dat %>%
    dplyr::select(pft, tidyselect::matches(rxp)) %>%
    dplyr::filter_at(dplyr::vars(tidyselect::matches(rxp)),
                     dplyr::any_vars(!is.na(.)))

}
