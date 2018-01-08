#' Paul Tol color schemes
#'
#' Effective color schemes for categorical data.
#' Available for 14, 15, 18, and 21 levels.
#'
#' @export
tol14rainbow <- c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")

#' @rdname tol14rainbow
#' @export
tol15rainbow <- c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")

#' @rdname tol14rainbow
#' @export
tol18rainbow <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")

#' @rdname tol14rainbow
#' @export
tol21rainbow <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")

#' Indices defining PFT colors
#'
#' Indices correspond to `tol21rainbow` color scheme.
pft_inds <- c(
  BlETr = 1,
  BlETe = 3,
  BlDTr = 4,
  BlDTe = 5,
  BlDBo = 6,
  NlETe = 7,
  NlEBo = 8,
  NlD = 9,
  ShE = 13,
  ShDTe = 14,
  ShDBo = 15,
  C3GAr = 16,
  C3GTe = 17,
  C4G = 18
)

#' PFT color scheme
#'
#' @export
pft_colors <- tol21rainbow[pft_inds]

#' Quick function for examining color schemes
#' 
#' Draws a bar plot [graphics::barplot()], with each bar shaded to the specified color.
#' Useful for quickly testing color schemes.
#'
#' @param cols Character vector of colors
#' @export
colortest <- function(cols) {
  y <- rep(1, length(cols))
  names(y) <- as.character(seq_along(cols))
  barplot(y, col = cols)
}
