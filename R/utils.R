#' Convert decimal degrees to radians
#'
#' @param deg A vector of values, in decimal degrees, to convert to radians
#'
#' @keywords internal
#'
#' @family utilities
#'
#' @return A vector of the same length in radians
deg_to_rad <- function(deg) {
  stopifnot(is.numeric(deg))
  deg * base::pi / 180
}

#' Convert radians to degrees
#'
#' @param rad A vector of values, in radians, to convert to decimal degrees
#'
#' @keywords internal
#'
#' @family utilities
#'
#' @return A vector of the same length in decimal degrees
rad_to_deg <- function(rad) {
  stopifnot(is.numeric(rad))
  rad * 180 / base::pi
}

#' Get the great-circle centroid for latitude/longitude data
#'
#' @param lat A vector of latitudes in degrees.
#' @param lng A vector of longitudes in degrees.
#'
#' @keywords internal
#'
#' @family utilities
#'
#' @return A latitude/longitude
get_centroid <- function(lat, lng) {

  lat <- deg_to_rad(lat)
  lng <- deg_to_rad(lng)

  x <- sum(cos(lat) * cos(lng)) / length(lat)
  y <- sum(cos(lat) * sin(lng)) / length(lat)
  z <- sum(sin(lat)) / length(lat)
  lng <- atan2(y, x)
  lat <- atan2(z, sqrt(x * x + y * y))

  lat <- rad_to_deg(lat)
  lng <- rad_to_deg(lng)

  return(c(lat = lat, lng = lng))
}

#' Find latitude and longitude for a certain distance and azimuth from a point.
#'
#' @param coord_pair A numeric vector of length 2 with names "lat" and "lng"
#' @param distance A distance (in meters) representing the distance away from
#' the original point to apply
#' @param azimuth A azimuth (in units specified in \code{azimuth_unit})
#' representing the direction to apply the distance from the original point in
#' @param distance_unit A string passed to [convert_distance]
#' indicating the units of the provided distance.
#' @param azimuth_unit A string (either \code{degrees} or \code{radians})
#' indicating the units of the \code{azimuth} argument
#'
#' @keywords internal
#'
#' @return An object of class [terrainr_coordinate_pair].
point_from_distance <- function(coord_pair,
                                distance,
                                azimuth,
                                distance_unit = "meters",
                                azimuth_unit = c("degrees", "radians")) {
  distance_unit <- distance_unit[[1]]
  units(distance) <- units::as_units(distance_unit)
  azimuth_unit <- azimuth_unit[[1]]

  # Force units to meters
  distance <- units::as_units("meter") + distance - units::as_units("meter")
  # Remove units to prevent errors in trig
  units(distance) <- units::as_units(NULL)

  r <- 6371e3 # Radius of the earth in m

  if (!methods::is(coord_pair, "terrainr_coordinate_pair")) {
    coord_pair <- terrainr_coordinate_pair(coord_pair)
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

  return(terrainr_coordinate_pair(c(
    rad_to_deg(new_lat),
    rad_to_deg(new_lng)
  )))
}
