#' Create a plot to represent a paired t-test
#'
#' @param data a dataframe in long format
#' @param x a bare variable name containing the timepoint indicator
#' @param y a bare variable name containing the values for comparison
#' @param group a bare variable name containing the case ID
#'
#' @return
#' @export
#'
#' @examples
paired_t_plot <-  function(data, x, y, group) {
  ggplot2::ggplot(data, ggplot2::aes_string(x = deparse(substitute(x)),
                          y = deparse(substitute(y)),
                          group = deparse(substitute(group)))) +
    ggplot2::stat_summary(ggplot2::aes(group = NULL),
                          size = 2,
                          fun.data = ggplot2::mean_sdl,
                          fun.args = list(mult = 1)) +
    ggplot2::geom_point(alpha = .25) +
    ggplot2::geom_line(alpha = .25) +
    ggplot2::labs(x = "Timepoint", y = "Measure score") +
    neami::theme_neami()
}



#' Makes an opinionated waffleplot
#'
#' @param data a dataframe
#' @param count_var a bare variable name indicating the column to be counted
#' @param palette a ggplot2 compatable pallette
#'
#' @return a waffleplot
#' @export
#'
#' @examples
#' neami_waffle(mtcars, cyl)
neami_waffle <- function(data, count_var, palette = "Set2") {

  waf_dat <- data %>%
    tidyr::drop_na({{count_var}}) %>%
    dplyr::group_by({{count_var}}) %>%
    dplyr::summarise(count=n()) %>%
    dplyr::pull(count, name={{count_var}})

  waffle::waffle(waf_dat,rows=5,xlab='1 Square = 1 Consumer',
                 colors = RColorBrewer::brewer.pal(8, palette)[seq_along(waf_dat)])

}
