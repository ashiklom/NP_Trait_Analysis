#' Number of pairwise missing observations
#'
#' @param xvar,yvar (character) Columns to check for missingness
#' @param trait_data `data.frame` of data
#' @return `data.frame` of missing and present observations
#' @author Alexey Shiklomanov
#' @export
get_pairwise_missing <- function(xvar, yvar, trait_data) {
  qxvar <- rlang::sym(xvar)
  qyvar <- rlang::sym(yvar)
  n_all <- nrow(trait_data)
  pairs_present <- trait_data %>%
    dplyr::filter(!is.na(!!qxvar), !is.na(!!qyvar))
  n_present <- nrow(pairs_present)
  n_missing <- n_all - n_present
  tibble::tibble(
    present = n_present,
    missing = n_missing
  )
}

#' Get all pairwise present and missing counts
#'
#' @inheritParams get_pairwise_missing
#' @return `data.frame` of number of pairwise and missing observations
#' @author Alexey Shiklomanov
#' @export
get_all_pairwise_missing <- function(trait_data) {
  trait_grid <- colnames(trait_data) %>%
    combn(2) %>%
    t() %>%
    `colnames<-`(c("yvar", "xvar")) %>%
    tibble::as_tibble()
  trait_grid %>%
    dplyr::mutate(
      missing_info = purrr::map2(
        xvar,
        yvar,
        get_pairwise_missing,
        trait_data = trait_data
      )
    ) %>%
    tidyr::unnest(missing_info)
}
