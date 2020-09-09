#' Coordinate points in the format expected by [terrainr] functions
#'
#' @slot lat Numeric latitude, in decimal degrees
#' @slot lng Numeric longitude, in decimal degrees
#'
#' @exportClass terrainr_coordinate_point
methods::setClass("terrainr_coordinate_point",
                  slots = c(
                    lat = "numeric",
                    lng = "numeric"
                  ),
                  prototype = list(
                    lat = NA_real_,
                    lng = NA_real_
                  )
)

#' Construct a terrainr_coordinate_point object
#'
#' @param coords A vector of length 2 containing a latitude and longitude. If
#' unnamed, coordinates are assumed to be in (latitude, longitude) format; if
#' named, the function will attempt to figure out which value represents which
#' coordinate.
#' @param coord_units String indicating whether coordinates are in degrees or
#' radians. Degrees stored in radians will be converted to degrees.
#'
#' @return A [terrainr::terrainr_coordinate_point] object
#'
#' @examples
#' terrainr_coordinate_point(c(44.05003, -74.01164))
#'
#' @export
terrainr_coordinate_point <- function(coords, coord_units = c("degrees",
                                                              "radians")) {
  stopifnot(length(coords) == 2)
  longitude_names <- c("lng",
                       "long",
                       "longitude",
                       "x")
  latitude_names <- c("lat",
                      "latitude",
                      "y")
  if (is.null(names(coords))) {
    message("Note: assuming unnamed coordinate vector is in (lat, lng) format")
    lat <- coords[[1]]
    lng <- coords[[2]]
  } else if (all(names(coords) %in% c(latitude_names, longitude_names))) {
    stopifnot(sum(names(coords) %in% latitude_names) == 1)
    stopifnot(sum(names(coords) %in% longitude_names) == 1)
    lat <- coords[[names(coords) %in% latitude_names]]
    lng <- coords[[names(coords) %in% longitude_names]]
  } else stop("Couldn't understand coordinate vector names.")

  coord_units <- coord_units[[1]]
  if (coord_units == "radians") {
    lat <- rad_to_deg(lat)
    lng <- rad_to_deg(lng)
  }

  return(methods::new("terrainr_coordinate_point", lat = lat, lng = lng))

}

#' Bounding boxes in the format expected by [terrainr] functions
#'
#' @slot bl A [terrainr::terrainr_coordinate_pair] representing the bottom left
#' corner of the bounding box
#' @slot tr A [terrainr::terrainr_coordinate_pair] representing the top right
#' corner of the bounding box
#'
#' @exportClass terrainr_bounding_box
methods::setClass("terrainr_bounding_box",
                  slots = c(bl = "terrainr_coordinate_point",
                            tr = "terrainr_coordinate_point"),
                  prototype = list(
                    bl = methods::new("terrainr_coordinate_point"),
                    tr = methods::new("terrainr_coordinate_point")
                  ))

#' Construct a terrainr_bounding_box object
#'
#' @param bl,tr The bottom left (\code{bl}) and top right (\code{tr}) corners of
#' the bounding box, either as a [terrainr::terrainr_coordinate_point] object or
#' a coordinate pair. If the coordinate pair is not named, it is assumed to be
#' in (lat, lng) format; if it is named, the function will attempt to properly
#' identify coordinates.
#' @param coord_units Arguments passed to [terrainr::terrainr_coordinate_point].
#' If \code{bl} and \code{tr} are already [terrainr::terrainr_coordinate_point]
#' objects, these arguments are not used.
#'
#' @return A terrainr_bounding_box object
#'
#' @examples
#' # Create bounding box from coordinates:
#' terrainr_bounding_box(bl = c(44.05003, -74.01164),
#'                       tr = c(44.17538, -73.83500))
#' # This is identical to:
#' bl_coords <- terrainr_coordinate_point(c(44.05003, -74.01164))
#' tr_coords <- terrainr_coordinate_point(c(44.17538, -73.83500))
#' terrainr_bounding_box(bl = bl_coords,
#'                       tr = tr_coords)
#'
#' @export
terrainr_bounding_box <- function(bl, tr, coord_units = "degrees") {
  if (!methods::is(bl, "terrainr_coordinate_point")) {
    bl <- terrainr_coordinate_point(bl, coord_units)
  }
  if (!methods::is(tr, "terrainr_coordinate_point")) {
    tr <- terrainr_coordinate_point(tr, coord_units)
  }
  if ((bl@lat < tr@lat) && (bl@lng < tr@lng)) {

  } else if ((bl@lat > tr@lat) && (bl@lng > tr@lng)) {
    swap <- bl
    bl <- tr
    tr <- swap
  } else {
    stop("The given coordinates don't seem to be the bottom left and upper right corners.")
  }
  methods::new("terrainr_bounding_box",
               bl = bl,
               tr = tr)
}
