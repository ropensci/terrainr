#' Find latitude and longitude for a certain distance and azimuth from a point.
#'
#' @param coord_pair A numeric vector of length 2 with names "lat" and "lng"
#' @param distance A distance (in meters) representing the distance away from
#' the original point to apply
#' @param azimuth A azimuth (in units specified in \code{azimuth_unit})
#' representing the direction to apply the distance from the original point in
#' @param distance_unit A string passed to [terrainr::convert_distance]
#' indicating the units of the provided distance.
#' @param azimuth_unit A string (either \code{degrees} or \code{radians})
#' indicating the units of the \code{azimuth} argument
#'
#' @family utilities
#'
#' @return A \code{\link{terrainr_coordinate_pair}} object.
#'
#' @examples
#' # Calculate a point 100m straight north of the coordinate pair
#' point_from_distance(c(lat = 44.121268, lng = -73.903734), 100, 0)
#' @export
point_from_distance <- function(coord_pair,
                                distance,
                                azimuth,
                                distance_unit = "meters",
                                azimuth_unit = c("degrees", "radians")) {
  distance_unit <- distance_unit[[1]]
  azimuth_unit <- azimuth_unit[[1]]

  distance <- terrainr::convert_distance(distance, distance_unit)

  r <- 6371e3 # Radius of the earth in m

  if (!methods::is(coord_pair, "terrainr_coordinate_pair")) {
    coord_pair <- terrainr::terrainr_coordinate_pair(coord_pair)
  }

  lat <- coord_pair@lat
  lng <- coord_pair@lng

  azimuth_unit <- azimuth_unit[[1]]
  if (azimuth_unit == "degrees") {
    azimuth <- deg_to_rad(azimuth)
    lat <- deg_to_rad(lat)
    lng <- deg_to_rad(lng)
  }

  angular_distance <- distance / r

  new_lat <- asin(
    sin(lat) * cos(angular_distance) +
      cos(lat) * sin(angular_distance) * cos(azimuth)
  )

  new_lng <- lng + atan2(
    sin(azimuth) * sin(angular_distance) * cos(lat),
    cos(angular_distance) - sin(lat) * sin(new_lat)
  )

  return(terrainr::terrainr_coordinate_pair(c(
    rad_to_deg(new_lat),
    rad_to_deg(new_lng)
  )))
}
