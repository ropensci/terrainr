#' S4 class for coordinate points in the format expected by
#' \code{\link{terrainr}} functions.
#'
#' @slot lat Numeric latitude, in decimal degrees
#' @slot lng Numeric longitude, in decimal degrees
#'
#' @family classes and related functions
#'
#' @exportClass terrainr_coordinate_pair
methods::setClass("terrainr_coordinate_pair",
  slots = c(
    lat = "numeric",
    lng = "numeric"
  ),
  prototype = list(
    lat = NA_real_,
    lng = NA_real_
  )
)

#' Construct a terrainr_coordinate_pair object.
#'
#' In order to simplify code, most \code{terrainr} functions expect a set S4
#' class representation of coordinate pairs and bounding boxes. If the provided
#' data isn't in the expected S4 format, these functions are used to cast the
#' data into the target class.
#'
#' @param coords A vector of length 2 containing a latitude and longitude. If
#' unnamed, coordinates are assumed to be in (latitude, longitude) format; if
#' named, the function will attempt to figure out which value represents which
#' coordinate. Currently this function understands "lat", "latitude", and "y" as
#' names for latitude and "lng", "long", "longitude", and "x" for longitude.
#' @param coord_units String indicating whether coordinates are in degrees or
#' radians. Degrees stored in radians will be converted to degrees.
#'
#' @return \code{terrainr_coordinate_pair} object
#'
#' @family classes and related functions
#'
#' @examples
#' terrainr_coordinate_pair(c(lat = 44.05003, lng = -74.01164))
#' @export
terrainr_coordinate_pair <- function(coords, coord_units = c(
                                       "degrees",
                                       "radians"
                                     )) {
  stopifnot(length(coords) == 2)
  longitude_names <- c(
    "lng",
    "long",
    "longitude",
    "x"
  )
  latitude_names <- c(
    "lat",
    "latitude",
    "y"
  )
  # the components of coord pairs are named
  # so if someone passes them to try and construct a new bbox and names the
  # vectors (to avoid the warning), they get an error
  #
  # build in a control for that
  longitude_names <- c(longitude_names,
                       paste0(longitude_names, ".lng"))
  latitude_names <- c(latitude_names,
                       paste0(latitude_names, ".lat"))


  if (is.null(names(coords))) {
    # If we have no names to go by,
    # assume that provided data is ISO 6709 compliant.
    warning("Assuming unnamed coordinate vector is in (lat, lng) format")
    lat <- coords[[1]]
    lng <- coords[[2]]
  } else if (all(names(coords) %in% c(latitude_names, longitude_names))) {
    stopifnot(sum(names(coords) %in% latitude_names) == 1)
    stopifnot(sum(names(coords) %in% longitude_names) == 1)
    lat <- coords[names(coords) %in% latitude_names]
    lng <- coords[names(coords) %in% longitude_names]
  } else {
    stop("Couldn't understand coordinate vector names.")
  }

  coord_units <- coord_units[[1]]
  if (coord_units == "radians") {
    lat <- rad_to_deg(lat)
    lng <- rad_to_deg(lng)
  }

  return(methods::new("terrainr_coordinate_pair", lat = lat, lng = lng))
}

#' Convert a terrainr_coordinate_pair object to a base vector.
#'
#' @param coord_pair A \code{\link{terrainr_coordinate_pair}} object
#'
#' @return A vector with a coordinate pair in (latitude, longitude) format
#'
#' @family classes and related functions
#'
#' @examples
#' coord_pair <- terrainr_coordinate_pair(c(lat = 44.05003, lng = -74.01164))
#' export_coord_pair(coord_pair)
#' @export
export_coord_pair <- function(coord_pair) {
  if (!methods::is(coord_pair, "terrainr_coordinate_pair")) {
    warning("coord_pair is not terrainr_coordinate_pair object, doing nothing.")
    return(coord_pair)
  } else {
    return(c(coord_pair@lat, coord_pair@lng))
  }
}

#' S4 class for bounding boxes in the format expected by \code{terrainr}
#' functions.
#'
#' @slot bl A \code{\link{terrainr_coordinate_pair}} representing the bottom
#' left corner of the bounding box
#' @slot tr A \code{\link{terrainr_coordinate_pair}} representing the top right
#' corner of the bounding box
#'
#' classes and related functions
#'
#' @exportClass terrainr_bounding_box
methods::setClass("terrainr_bounding_box",
  slots = c(
    bl = "terrainr_coordinate_pair",
    tr = "terrainr_coordinate_pair"
  ),
  prototype = list(
    bl = methods::new("terrainr_coordinate_pair"),
    tr = methods::new("terrainr_coordinate_pair")
  )
)

#' Construct a terrainr_bounding_box object
#'
#' In order to simplify code, most \code{terrainr} functions expect a set S4
#' class representation of coordinate pairs and bounding boxes. If the provided
#' data is not in the expected S4 format, these functions are used to cast the
#' data into the target class.
#'
#' @param bl,tr The bottom left (\code{bl}) and top right (\code{tr}) corners of
#' the bounding box, either as a \code{\link{terrainr_coordinate_pair}} object
#' or a coordinate pair. If the coordinate pair is not named, it is assumed to
#' be in (lat, lng) format; if it is named, the function will attempt to
#' properly identify coordinates.
#' @param coord_units Arguments passed to \code{\link{terrainr_coordinate_pair}}.
#' If \code{bl} and \code{tr} are already \code{\link{terrainr_coordinate_pair}}
#' objects, these arguments are not used.
#'
#' @return A terrainr_bounding_box object
#'
#' classes and related functions
#'
#' @examples
#' # Create bounding box from coordinates:
#' terrainr_bounding_box(
#'   bl = c(lat = 44.05003, lng = -74.01164),
#'   tr = c(lat = 44.17538, lng = -73.83500)
#' )
#' # This is identical to:
#' bl_coords <- terrainr_coordinate_pair(c(lat = 44.05003, lng = -74.01164))
#' tr_coords <- terrainr_coordinate_pair(c(lat = 44.17538, lng = -73.83500))
#' terrainr_bounding_box(
#'   bl = bl_coords,
#'   tr = tr_coords
#' )
#' @export
terrainr_bounding_box <- function(bl, tr, coord_units = "degrees") {
  if (!methods::is(bl, "terrainr_coordinate_pair")) {
    bl <- terrainr_coordinate_pair(bl, coord_units)
  }
  if (!methods::is(tr, "terrainr_coordinate_pair")) {
    tr <- terrainr_coordinate_pair(tr, coord_units)
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
    tr = tr
  )
}

#' Convert a terrainr_bounding_box object to a base list
#'
#' @param bbox A \code{\link{terrainr_bounding_box}} object
#'
#' @return A list with coordinate pairs representing the lower left and upper
#' right corners of a bounding box
#'
#' classes and related functions
#'
#' @examples
#' # Create bounding box from coordinates:
#' bbox <- terrainr_bounding_box(
#'   bl = c(lat = 44.05003, lng = -74.01164),
#'   tr = c(lat = 44.17538, lng = -73.83500)
#' )
#' export_bounding_box(bbox)
#' @export
export_bounding_box <- function(bbox) {
  if (!methods::is(bbox, "terrainr_bounding_box")) {
    warning("bbox is not terrainr_bounding_box object, doing nothing.")
    return(bbox)
  } else {
    return(list(
      bl = c(
        bbox@bl@lat,
        bbox@bl@lng
      ),
      tr = c(
        bbox@tr@lat,
        bbox@tr@lng
      )
    ))
  }
}
