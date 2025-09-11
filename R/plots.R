#' Create a plot to represent a paired t-test
#'
#' @param data a dataframe in long format
#' @param x a bare variable name containing the timepoint indicator
#' @param y a bare variable name containing the values for comparison
#' @param group a bare variable name containing the case ID
#' @param colour an optional bare variable name containing a parameter to colour on
#' @param type an optional character indicating specific plot parameters (k10)
#'
#' @export
#'
#' @examples
#' dl <- data.frame(x= rep(c("Pre", "Post"), 20), y = rnorm(40, 25, 8), group = sort(rep(1:20, 2)), colour = sort(rep(LETTERS[1:2], 20)))
#' dl %>% paired_t_plot(x = x, y = y, group = group, colour = colour)
#' dl %>% paired_t_plot(x = x, y = y, group = group, colour = colour, type = "k10")
paired_t_plot <-  function(data, x, y, group, colour = NULL, type = NULL) {
  
  if (!missing(type)) {
    if(type == "k10"){
      rects <-  data.frame(level = c("Mild", "Moderate", "Severe"), 
                           ymin = seq(20, 30, 5), 
                           ymax = c(25, 30, 50), 
                           xmin = -Inf, 
                           xmax = Inf, 
                           fill = c("yellow", "orange", "red"))
      values <-  rects$fill
      names(values) <- rects$level
      
      labels <- data.frame(labels =c("Mild", "Moderate", "Severe"), 
                           x = .45, 
                           y= c(22.5, 27.5, 32.5))
      
      ggplot2::ggplot(data, ggplot2::aes(x = factor({{x}}),
                                         y = {{y}},
                                         group = {{group}}, colour = {{colour}})) +
        ggplot2::geom_rect(aes(xmin=xmin, ymin=ymin, xmax= xmax, ymax= ymax, fill = level), alpha =.2, data = rects, inherit.aes = FALSE ) +
        # ggplot2::geom_hline(yintercept = 20, colour = "grey50", alpha = .7,  linetype = "dotted") +
        # ggplot2::geom_hline(yintercept = 25, colour = "grey50", alpha = .7,  linetype = "dashed") +
        # ggplot2::geom_hline(yintercept = 30, colour = "grey50", alpha = .7,  linetype = "longdash") +
        ggplot2::stat_summary(ggplot2::aes(group = NULL, colour = NULL),
                              size = 2,
                              fun.data = ggplot2::mean_sdl,
                              fun.args = list(mult = 1)) +
        ggplot2::geom_point(alpha = .3) +
        ggplot2::geom_line(alpha = .3) +
        ggplot2::geom_text(aes(x=x, y=y, label = labels), data = labels, inherit.aes = FALSE, hjust = "left",
                           colour = "grey30") + 
        ggplot2::labs(x = "Timepoint", y = "Measure score") +
        neami::theme_neami() +
        neami::scale_colour_neami_d("core_qual")+
        ggplot2::scale_fill_manual(values = values) + 
        neami::hide_x_grid() + 
        ggplot2::guides(fill = guide_none())
    }    else {
      warning("Type ", type, " is not supported.")
    }
  } else {
    
    ggplot2::ggplot(data, ggplot2::aes(x = factor({{x}}),
                                       y = {{y}},
                                       group = {{group}}, colour = {{colour}})) +
      ggplot2::stat_summary(ggplot2::aes(group = NULL, colour = NULL),
                            size = 2,
                            fun.data = ggplot2::mean_sdl,
                            fun.args = list(mult = 1)) +
      ggplot2::geom_point(alpha = .25) +
      ggplot2::geom_line(alpha = .25) +
      ggplot2::labs(x = "Timepoint", y = "Measure score") +
      neami::theme_neami() +
      neami::scale_colour_neami_d("core_qual")+
      neami::hide_x_grid()
  }
  
  
}



#' Makes an opinionated waffleplot
#'
#' @param data a dataframe
#' @param count_var a bare variable name indicating the column to be counted
#' @param palette a ggplot2 compatable palette
#'
#' @export
#'
#' @examples
#' neami_waffle(mtcars, cyl)
neami_waffle <- function(data, count_var, rows = NULL, palette = "Set2") {
  
  if(!is.null(rows)) {warning("The rows argument is deprecated.")}
  
  smart_round <- function(x, digits = 0) { # somewhere on SO
    up <- 10 ^ digits
    x <- x * up
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y / up
  }
  
  data |> 
    tidyr::drop_na({{count_var}}) |>
    dplyr::group_by({{count_var}}) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::mutate({{count_var}} := as.factor({{count_var}}), 
                  pct = count / sum(count), 
                  pct = as.integer(smart_round(pct, 1) * 100L)) |> 
    dplyr::select(-count) |> 
    tidyr::uncount(pct) |> 
    dplyr::bind_cols(expand_grid(y = 1:10, x = 1:10)) |> 
    ggplot2::ggplot(aes(x, y, fill = {{count_var}})) + 
    ggplot2::geom_tile(colour = "white") + 
    ggplot2::coord_equal() +
    ggplot2::scale_fill_brewer(palette = palette) + 
    ggplot2::theme_void() +
    ggplot2::labs(caption = "1 Square = 1 percent") +
    ggplot2::theme(legend.position = "bottom", plot.caption.position = "panel") 
  
  
}
