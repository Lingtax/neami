#' Neami National Colour Palettes
#'
#' A collection of Neami colour palettes. The list of available palettes is:
#' core_qual_light
#' core_qual
#' blues
#' greens_l
#' oranges
#'
#'@examples
#'
#' # Make an x-y plot using the neami_diverging palette
#' library(tidyverse)
#' df <- data.frame(x = rnorm(100, 0, 20),
#'           y = rnorm(100, 0, 20),
#'           cl = sample(letters[1:8], 100, replace=TRUE))
#' ggplot(df, aes(x, y, colour=cl)) +
#'   geom_point(size=4) + scale_colour_neami_d() +
#'   theme_bw() + theme(aspect.ratio=1)
#' @export
neami_colours = list(

  core_qual_light = c("#F16548", "#00B37D", "#F6A4AF", "#00B8C4", "#FFFFFF"),
  core_qual = c("#000000", "#004184", "#e62900", "#D623A3", "#519445"),

  ext_qual = c("#047F7F",
               "#654ABC",
               "#005AE1",
               "#D623A3",
               "#000000"),
  ext_qual_light = c("#E65280","#E62900", "#00A375", "#DA7900", "#0091E5", "#8B8478", "#CA6AE9", "#FFFFFF"),

  blues = c("#d6e0ef", "#acc1df", "#83a3d0", "#5984c0", "#3065b0", "#26518d", "#1d3d6a"),
  greens_l = c("#4dcaa4", "#33c297", "#1abb8a", "#00b37d", "#00a171", "#008f64"),
  oranges = c("#f5937f", "#f4846d", "#f2745a", "#f16548", "#d95b41", "#c1513a")


)

#' Retrieve a Neami palette
#'
#' @param name name of palette
#' @param n number of levels
#' @param all_palettes list of named palettes
#' @param type a character entry indicating whether the palette is discrete or continuous
#'
#' @return
#' @export
#'
#' @examples
neami_palettes = function(name, n, all_palettes = neami_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

#' Colour a geom with a discrete Neami palette
#'
#' @param name a character name of a palette
#'
#' @return
#' @export
#'
#' @examples
scale_colour_neami_d = function(name) {
  ggplot2::scale_colour_manual(values = neami_palettes(name,
                                                       type = "discrete"))
}

#' Fill a geom with a discrete Neami palette
#'
#' @param name a character name of a palette
#'
#' @return
#' @export
#'
#' @examples
scale_fill_neami_d = function(name) {
  ggplot2::scale_fill_manual(values = neami_palettes(name,
                                                     type = "discrete"))
}

#' Colour a geom with a continuous Neami palette
#'
#' @param name a character name of a palette
#'
#' @return
#' @export
#'
#' @examples
scale_colour_neami_c = function(name) {
  ggplot2::scale_colour_gradientn(colours = neami_palettes(name = name,
                                                           type = "continuous"))
}

#' Fill a geom with a continuous Neami palette
#'
#' @param name a character name of a palette
#'
#' @return
#' @export
#'
#' @examples
scale_fill_neami_c = function(name) {
  ggplot2::scale_colour_gradientn(colours = neami_palettes(name = name,
                                                           type = "continuous"))
}

