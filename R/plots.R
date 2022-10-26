#' Create a plot to represent a paired t-test
#'
#' @param data a dataframe in long format
#' @param x a bare variable name containing the timepoint indicator
#' @param y a bare variable name containing the values for comparison
#' @param group a bare variable name containing the case ID
#' @param colour an optional bare variable name containing a parameter to colour on
#'
#' @export
#'
#' @examples
#' dl <- data.frame(x= rep(c("Pre", "Post"), 20), y = rnorm(40, 25, 8), group = sort(rep(1:20, 2)), colour = sort(rep(LETTERS[1:2], 20)))
#' dl %>% paired_t_plot(x = x, y = y, group = group, colour = colour)
paired_t_plot <-  function(data, x, y, group, colour = NULL) {
  ggplot2::ggplot(data, ggplot2::aes(x = {{x}},
                          y = {{y}},
                          group = {{group}}, colour = {{colour}})) +
    ggplot2::stat_summary(ggplot2::aes(group = NULL, colour = NULL),
                          size = 2,
                          fun.data = ggplot2::mean_sdl,
                          fun.args = list(mult = 1)) +
    ggplot2::geom_point(alpha = .25) +
    ggplot2::geom_line(alpha = .25) +
    ggplot2::labs(x = "Timepoint", y = "Measure score") #+
    #neami::theme_neami()
}



#' Makes an opinionated waffleplot
#'
#' @param data a dataframe
#' @param count_var a bare variable name indicating the column to be counted
#' @param rows an integer of the number of output rows
#' @param palette a ggplot2 compatable palette
#'
#' @export
#'
#' @examples
#' neami_waffle(mtcars, cyl)
neami_waffle <- function(data, count_var, rows = 5, palette = "Set2") {

  waf_dat <- data %>%
    tidyr::drop_na({{count_var}}) %>%
    dplyr::group_by({{count_var}}) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::pull(count, name = {{count_var}})

  waffle::waffle(waf_dat,rows = rows, xlab = '1 Square = 1 Consumer',
                 colors = RColorBrewer::brewer.pal(8, palette)[seq_along(waf_dat)])

}
