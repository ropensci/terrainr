#' Convert decimal degrees to radians
#'
#' @param deg A vector of values, in decimal degrees, to convert to radians
#'
#' @family utilities
#'
#' @return A vector of the same length in radians
#'
#' @examples
#' deg_to_rad(360)
#' rad_to_deg(deg_to_rad(360))
#' @export
deg_to_rad <- function(deg) {
  stopifnot(is.numeric(deg))
  deg * base::pi / 180
}

#' Convert radians to degrees
#'
#' @param rad A vector of values, in radians, to convert to decimal degrees
#'
#' @family utilities
#'
#' @return A vector of the same length in decimal degrees
#'
#' @examples
#' rad_to_deg(2 * base::pi)
#' rad_to_deg(deg_to_rad(360))
#' @export
rad_to_deg <- function(rad) {
  stopifnot(is.numeric(rad))
  rad * 180 / base::pi
}

#' Convert distance into meters
#'
#' @param distance The numeric distance to be converted to meters
#' @param distance_unit A string indicating the units to convert distance from
#'
#' @family utilities
#'
#' @return A numeric vector of distances, converted into meters.
#'
#' @examples
#' convert_distance(100, "miles")
#' @export
convert_distance <- function(distance, distance_unit = c(
                               "meters",
                               "m",
                               "kilometers",
                               "km",
                               "miles",
                               "mi",
                               "feet",
                               "ft",
                               "inches",
                               "in"
                             )) {
  distance_unit <- distance_unit[[1]]
  if (distance_unit == "miles" || distance_unit == "mi") {
    distance * 1609.344
  } else if (distance_unit == "feet" || distance_unit == "ft") {
    distance * 0.3048
  } else if (distance_unit == "kilometers" || distance_unit == "km") {
    distance * 1000
  } else if (distance_unit == "inches" || distance_unit == "in") {
    distance * 0.0254
  } else {
    distance
  }
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
