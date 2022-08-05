#' neami palette with ramped colours
#'
#' @param palette Choose from 'neami_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @examples
#' library(scales)
#' show_col(ochre_pal()(10))
#'
#' filled.contour(volcano,color.palette = neami_pal(), asp=1)
#'
#' @export
neami_pal <- function(palette="neami_qual", alpha = 1, reverse = FALSE) {
  pal <- neami_palettes[[palette]]
  if (reverse){
    pal <- rev(pal)
  }
  return(grDevices::colorRampPalette(pal, alpha))
}

#' Setup colour palette for ggplot2
#'
#' @rdname scale_color_neami
#'
#' @param palette Choose from 'neami_palettes' list
#'
#' @param reverse logical, Reverse the order of the colours?
#'
#' @param alpha transparency
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @inheritParams viridis::scale_color_viridis
#'
#' @importFrom ggplot2 scale_colour_manual
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   scale_colour_neami(palette="neami_seq1")
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = hp)) +
#'   scale_colour_neami(palette="neami_seq1", discrete = FALSE)
#' ggplot(data = mpg) +
#'   geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#'   scale_colour_neami(palette="neami_seq2")
#' ggplot(diamonds) + geom_bar(aes(x = cut, fill = clarity)) +
#'   scale_fill_neami()
#' @export
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
scale_color_neami <- function(..., palette="neami_qual",
                              discrete = TRUE, alpha = 1, reverse = FALSE) {
  if (discrete) {
    ggplot2::discrete_scale("colour", "neami", palette=neami_pal(palette, alpha = alpha, reverse = reverse))
  } else {
    ggplot2::scale_color_gradientn(colours = neami_pal(palette, alpha = alpha, reverse = reverse, ...)(256))
  }
  #scale_colour_manual(values=neami_palettes[[palette]])
}

#' @rdname scale_color_neami
#' @export
scale_colour_neami <- scale_color_neami

#' #' Setup fill palette for ggplot2
#'
#' @param palette Choose from 'neami_palettes' list
#'
#' @inheritParams viridis::scale_fill_viridis
#' @inheritParams ochre_pal
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_fill_manual discrete_scale scale_fill_gradientn
#'
#' @export
scale_fill_neami <- function(..., palette="neami_qual",
                             discrete = TRUE, alpha=1, reverse = TRUE) {
  if (discrete) {
    ggplot2::discrete_scale("fill", "neami", palette=neami_pal(palette, alpha = alpha, reverse = reverse))
  }
  else {
    ggplot2::scale_fill_gradientn(colours = neami_pal(palette, alpha = alpha, reverse = reverse, ...)(256))
  }
}
