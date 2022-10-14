#' Calculate Bounding Box
#'
#' Calculate a bounding box for a center point given a set of coordinates.
#'
#' @param lat The latitude of the center point.
#' @param lon The longitude of the center point.
#' @param dist The distance from the center point.
#' @param in.km logical.  If \code{TRUE} uses km as the units of
#' \code{dist}.  If \code{FALSE} uses miles.
#' @return Returns a matrix with max/min latitude/longitude values.
#' @references \url{http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates}
#' @keywords bounding_box, coordinates
#' @export
#' @examples
#' bounding_box(-37.74348546926522, 145.03134441138408, 1)
bounding_box <- function(lat, lon, dist, in.km = TRUE) {

  ## Helper functions
  if (in.km) {
    ang_rad <- function(km) km/1000
  } else {
    ang_rad <- function(km) km/3958.756
  }
  `%+/-%` <- function(x, margin){x + c(-1, +1)*margin}
  deg2rad <- function(x) x/(180/pi)
  rad2deg <- function(x) x*(180/pi)
  lat_range <- function(latr, r) rad2deg(latr %+/-% r)
  lon_range <- function(lonr, dlon) rad2deg(lonr %+/-% dlon)

  r <- ang_rad(dist)
  latr <- deg2rad(lat)
  lonr <- deg2rad(lon)
  dlon <- asin(sin(r)/cos(latr))

  m <- matrix(c(lon_range(lonr = lonr, dlon = dlon),
                lat_range(latr=latr, r=r)), nrow=2, byrow = TRUE)

  dimnames(m) <- list(c("lng", "lat"), c("min", "max"))
  m
}


#' Generate a map of consumers origin around a site
#'
#' @param lat Numeric Latitude of the site
#' @param lon Numeric Longitude of the site
#' @param dist Numeric Distance in KM around the site that the bounding box should be drawn
#' @param suburb_df Dataframe containing consumers and a column of suburbs
#' @param suburb The column of suburbs within the dataframe containing suburn names
#' @param shapefile_path Path to the shapefile to join.
#' @param state State of Australia to filter the shapefile to.
#'
#' @export
site_map <- function(lat, lon, dist = 15, suburb_df, suburb, shapefile_path, state) {

  bb <- bounding_box(lat, lon, dist)
  rm <- ggmap::get_map(bb)

  suburb_df <- dplyr::summarise(dplyr::group_by(suburb_df, deparse(substitute(suburb))),
                                count = dplyr::n())

  sf <- sf::read_sf(shapefile_path)
  sf <-  dplyr::left_join(sf[sf$STE_NAME21 == state], suburb_df, by = c("SAL_NAME21" = "suburb"))


  ggmap::ggmap(rm) +
    ggplot2::geom_sf(inherit.aes = FALSE,  data = sf, fill = NA, colour = "grey40") +
    ggplot2::geom_sf(inherit.aes = FALSE,  data = sf[!is.na(sf$count), ],
                     ggplot2::aes(fill = count), colour = NA)
}



