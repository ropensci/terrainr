#' Extract latitude and longitude from a provided object
#'
#' This is an internal utility function to convert bounding boxes into
#' coordinate pairs.
#'
#' @param point_1,point_2 Coordinate pairs (as length-2 numeric vectors with the
#' names "lat" and "lng") to calculate distance between.
#'
#' @keywords internal
#'
#' @family utilities
#'
#' @return A vector of length 2 containing object latitude and longitude
#'
#' @examples
#' calc_haversine_distance(
#'   c(lat = 44.121268, lng = -73.903734),
#'   c(lat = 43.121268, lng = -74.903734)
#' )
#' @export
calc_haversine_distance <- function(point_1, point_2) {
  if (!methods::is(point_1, "terrainr_coordinate_pair")) {
    point_1 <- terrainr_coordinate_pair(point_1)
  }
  if (!methods::is(point_2, "terrainr_coordinate_pair")) {
    point_2 <- terrainr_coordinate_pair(point_2)
  }

  R <- 6371e3 # Radius of the earth in m

  lat1_rad <- deg_to_rad(point_1@lat)
  lat2_rad <- deg_to_rad(point_2@lat)
  delta_lat <- deg_to_rad(point_2@lat - point_1@lat)
  delta_lng <- deg_to_rad(point_2@lng - point_1@lng)

  a <- (sin(delta_lat / 2) * sin(delta_lat / 2)) +
    (cos(lat1_rad) * cos(lat2_rad) * sin(delta_lng / 2) * sin(delta_lng / 2))
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  # c inherits the name "lat" from a
  # which means this returns a length(1) vector of distance named... "lat"
  # so we'll get rid of that
  names(c) <- NULL

  return(R * c)
}
