#' Neami National Colour Palettes
#'
#' A collection of Neami colour palettes. The list of available palettes is:
#' neami_qual
#' neami_seq1
#' neami_seq2
#' neami_diverging
#'
#'@examples
#'
#' # Make an x-y plot using the neami_diverging palette
#' library(tidyverse)
#' df <- data.frame(x = rnorm(100, 0, 20),
#'           y = rnorm(100, 0, 20),
#'           cl = sample(letters[1:8], 100, replace=TRUE))
#' ggplot(df, aes(x, y, colour=cl, shape=cl)) +
#'   geom_point(size=4) + scale_colour_neami() +
#'   theme_bw() + theme(aspect.ratio=1)
#' @export
neami_palettes <-
  list(
    neami_qual = c("#FFAD00",
                   "#537EC0",
                   "#C52C7C",
                   "#404040"),

    neami_seq1 = c("grey90", "#537EC0"),
    neami_seq2 = c("grey90", "#FFAD00"),


    neami_diverging = c("#537EC0",
                        "#FFAD00")
  )
