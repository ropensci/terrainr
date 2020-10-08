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
                               "ft"
                             )) {
  distance_unit <- distance_unit[[1]]
  if (distance_unit == "miles" || distance_unit == "mi") {
    distance * 1609.344
  } else if (distance_unit == "feet" || distance_unit == "ft") {
    distance * 0.3048
  } else if (distance_unit == "kilometers" || distance_unit == "km") {
    distance * 1000
  } else {
    distance
  }
}
