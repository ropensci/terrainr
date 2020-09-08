#' Find latitude and longitude for a certain distance and azimuth from a point.
#'
#' @param coord_pair A numeric vector of length 2 with names "lat" and "lng"
#' @param distance A distance (in meters) representing the distance away from
#' the original point to apply
#' @param azimuth A azimuth (in units specified in \code{azimuth_unit})
#' representing the direction to apply the distance from the original point in
#' @param azimuth_unit A string (either \code{degrees} or \code{radians})
#' indicating the units of the \code{azimuth} argument
#'
#' @examples
#' # Calculate a point 100m straight north of the coordinate pair
#' point_from_distance(c(lat = 44.121268, lng = -73.903734), 100, 0)
#'
#' @export
point_from_distance <- function(coord_pair,
                                distance,
                                azimuth,
                                distance_unit = c("meters",
                                                  "kilometers",
                                                  "miles",
                                                  "feet"),
                                azimuth_unit = c("degrees", "radians")) {

  distance_unit <- distance_unit[[1]]
  azimuth_unit <- azimuth_unit[[1]]

  if (distance_unit == "miles") {
    distance <- distance * 1609.344
  } else if (distance_unit == "feet") {
    distance <- distance * 3048
  } else if (distance_unit == "kilometers") {
    distance <- distance / 1000
  }

  R <- 6371e3 # Radius of the earth in m

  if (azimuth_unit == "degrees") {
    azimuth <- deg_to_rad(azimuth)
    lat <- deg_to_rad(coord_pair[["lat"]])
    lng <- deg_to_rad(coord_pair[["lng"]])
  }

  angular_distance <- distance / R

  new_lat <- asin(
    sin(lat) * cos(angular_distance) +
      cos(lat) * sin(angular_distance) * cos(azimuth)
  )

  new_lng <- lng + atan2(
    sin(azimuth) * sin(angular_distance) * cos(lat),
    cos(angular_distance) - sin(lat) * sin(new_lat)
  )

  if (azimuth_unit == "degrees") {
    return(c(lat = rad_to_deg(new_lat),
             lng = rad_to_deg(new_lng)))
  } else {
    return(c(lat = new_lat,
             lng = new_lng))
  }
}
