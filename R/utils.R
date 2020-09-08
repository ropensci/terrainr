#' Convert decimal degrees to radians
#'
#' @param deg A vector of values, in decimal degrees, to convert to radians
#'
#' @return A vector of the same length in radians
#'
#' @examples
#' deg_to_rad(360)
#' rad_to_deg(deg_to_rad(360))
#'
#' @export
deg_to_rad <- function(deg) {
  stopifnot(is.numeric(deg))
  deg * base::pi / 180
}

#' Convert radians to degrees
#'
#' @param rad A vector of values, in radians, to convert to decimal degrees
#'
#' @return A vector of the same length in decimal degrees
#'
#' @examples
#' rad_to_deg(2 * base::pi)
#' rad_to_deg(deg_to_rad(360))
#'
#' @export
rad_to_deg <- function(rad) {
  stopifnot(is.numeric(rad))
  rad * 180 / base::pi
}

#' Extract latitude and longitude from a provided object
#'
#' This is an internal utility function to convert bounding boxes into
#' coordinate pairs.
#'
#' @param coords An object to try and extract coordinates from
#'
#' @keywords internal
#'
#' @return A vector of length 2 containing object latitude and longitude
#'
#' @examples
#' extract_coords(get_centroid_bounding_box(c(
#'   "lat" = 44.121268,
#'   "lng" = -73.903734
#' ),
#' distance = 10
#' )[[1]])
#'
#' @export
extract_coords <- function(coords) {
  if (all(names(coords %in% c(
    "lat",
    "lng",
    "long",
    "latitude",
    "longitude"
  )))) {
    if (sum(names(coords) %in% c("lat", "latitude")) != 1) {
      stop("Couldn't infer latitude variable -- try passing a value to lat.")
    } else if (sum(names(coords) %in% c("lng", "long", "longitude")) != 1) {
      stop("Couldn't infer longitude variable -- try passing a value to lng.")
    } else {
      output <- c(
        coords[names(coords) %in% c("lat", "latitude")],
        coords[names(coords) %in% c("lng", "long", "longitude")]
      )
      names(output) <- c("lat", "lng")
      return(output)
    }
  } else {
    stop("Couldn't determine lat/lng values.")
  }
}
