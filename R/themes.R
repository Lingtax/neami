#' Neami ggplot2 theme
#'
#' The function theme_neami will apply a coherent "Neami" theme to a ggplot2 object.
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes()) + theme_neami()
theme_neami <- function() {
  ggplot2::theme(
  plot.background = ggplot2::element_rect(fill = "#FFFFFF"),
  panel.background = ggplot2::element_rect(fill = "#FFFFFF"),
  legend.background = ggplot2::element_rect(fill = "#FFFFFF"),
  legend.key = ggplot2::element_rect(fill = "#FFFFFF"),
  line = ggplot2::element_line(colour = "#000000"),
  text = ggplot2::element_text(colour = "#000000", face = "bold", family = "Arial"),
  axis.text = ggplot2::element_text(colour = "#000000"),
  axis.line = ggplot2::element_line(colour = "#000000"),
  panel.grid.major = ggplot2::element_line(linewidth = .1, colour = "#000000"),
  panel.grid.minor = ggplot2::element_line(linewidth = .05, colour = "#000000"),
  legend.position = "bottom",
  strip.background = ggplot2::element_rect(fill = "#cccccc"),
  strip.text = ggplot2::element_text(colour = "#000000", face = "bold"),
  panel.spacing = ggplot2::unit(1, "lines")
  )
  }

#' Neami ggplot2 dark theme
#'
#' The function theme_neami_dark will apply a coherent "Neami" dark theme to a ggplot2 object.
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes()) + theme_neami_dark()
theme_neami_dark <- function() {
  ggplot2::theme(
  plot.background = ggplot2::element_rect(fill = "#01253B"),
  panel.background = ggplot2::element_rect(fill = "#01253B"),
  legend.background = ggplot2::element_rect(fill = "#01253B"),
  legend.key = ggplot2::element_rect(fill = "#01253B"),
  line = ggplot2::element_line(colour = "#FFFFFF"),
  text = ggplot2::element_text(colour = "#FFFFFF", face = "bold", family = "Arial"),
  axis.text = ggplot2::element_text(colour = "#FFFFFF"),
  axis.line = ggplot2::element_line(colour = "#FFFFFF"),
  panel.grid.major = ggplot2::element_line(linewidth = .1),
  panel.grid.minor = ggplot2::element_line(linewidth = .05),
  legend.position = "bottom",
  strip.background = ggplot2::element_rect(fill = "#4d6676"),
  strip.text = ggplot2::element_text(colour = "#FFFFFF", face = "bold"),
  panel.spacing = ggplot2::unit(1, "lines")
)}


#' Neami ggplot2 theme for maps
#'
#' Applies the Neami theme to a ggplot2 map.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'ggplot(nc) +
#'  geom_sf(aes(fill = AREA)) + theme_neami_map()
theme_neami_map <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::ggplot2::element_text(size = 16, family = "Arial", colour = "#4a4a4a"),
                   plot.title = ggplot2::ggplot2::element_text(family = "Arial", colour = "#53565a"),
                   axis.ticks = ggplot2::ggplot2::element_blank(),
                   axis.text = ggplot2::ggplot2::element_blank(),
                   axis.title = ggplot2::ggplot2::element_blank())
}


#' Saves a neami themed plot
#'
#' @param plot_grid The ggplot object for saving
#' @param width the output width in default units
#' @param height the output height in default units
#' @param save_filepath path to save output file to
#'
#' @export
save_plot <- function(plot_grid, width, height, save_filepath) {

  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = save_filepath, device = "png",
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}


#' Left align plot elements for themed plot
#'
#' @param plot_name the plot object for alignment
#' @param pieces a vector of parameter names for alignment e.g. c("subtitle", "title", "caption")
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

create_footer <- function (source_name, logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
                           grid::textGrob(paste0("Source: ", source_name),
                                          x = 0.004, hjust = 0, gp = grid::gpar(fontsize=16)),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 0.944))
  return(footer)

}

#' Arrange alignment and save Neami ggplot chart
#'
#' Running this function will save your plot with the correct guidelines for publication by Neami.
#' It will left align your title, subtitle and source, add the Neami Logo at the bottom right and save it to your specified location.
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param save A logical which indicates if the plot should be saved
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width_pixels Width in pixels that you want to save your chart to - defaults to 640
#' @param height_pixels Height in pixels that you want to save your chart to - defaults to 450
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to Neami Logo image that sits within the logos folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalise_plot
#' @export
finalise_plot <- function(plot_name,
                          source_name,
                          save=TRUE,
                          save_filepath=file.path(getwd(), "tmp-nc.png"),
                          width_pixels=640,
                          height_pixels=450,
                          logo_image_path = system.file("logos/Neami_No Tagline_Deep Blue.png", package = "neami")) {

  footer <- create_footer(source_name, logo_image_path)

  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, footer,
                                 ncol = 1, nrow = 2,
                                 heights = c(1, 0.045/(height_pixels/450)))
  ## print(paste("Saving to", save_filepath))
  if(save == TRUE){
  save_plot(plot_grid, width_pixels, height_pixels, save_filepath)}
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)

  if(save == FALSE) {grid::grid.draw(plot_grid)}

}


