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
