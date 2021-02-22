#' S4 class for coordinate points in the format expected by
#' \code{terrainr} functions.
#'
#' @slot lat Numeric latitude, in decimal degrees
#' @slot lng Numeric longitude, in decimal degrees
#'
#' @family classes and related functions
#' @keywords internal
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
#' @keywords internal
terrainr_coordinate_pair <- function(coords, coord_units = c(
                                       "degrees",
                                       "radians"
                                     )) {
  stopifnot(length(coords) == 2)
  longitude_names <- c(
    "lng",
    "lon",
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
  longitude_names <- c(
    longitude_names,
    paste0(longitude_names, ".lng")
  )
  latitude_names <- c(
    latitude_names,
    paste0(latitude_names, ".lat")
  )

  if (is.null(names(coords))) {
    # If we have no names to go by,
    # assume that provided data is ISO 6709 compliant.
    warning("Assuming unnamed coordinate vector is in (lat, lng) format")
    lat <- coords[[1]]
    lng <- coords[[2]]
  } else {
    names(coords) <- tolower(names(coords))

    if (all(names(coords) %in% c(latitude_names, longitude_names))) {
      stopifnot(sum(names(coords) %in% latitude_names) == 1)
      stopifnot(sum(names(coords) %in% longitude_names) == 1)
      lat <- coords[names(coords) %in% latitude_names]
      names(lat) <- "lat"
      lng <- coords[names(coords) %in% longitude_names]
      names(lng) <- "lng"
    } else {
      stop("Couldn't understand coordinate vector names.")
    }
  }


  coord_units <- coord_units[[1]]
  if (coord_units == "radians") {
    lat <- rad_to_deg(lat)
    lng <- rad_to_deg(lng)
  }

  return(methods::new("terrainr_coordinate_pair", lat = lat, lng = lng))
}

#' S4 class for bounding boxes in the format expected by \code{terrainr}
#' functions.
#'
#' @slot bl A \code{\link{terrainr_coordinate_pair}} representing the bottom
#' left corner of the bounding box
#' @slot tr A \code{\link{terrainr_coordinate_pair}} representing the top right
#' corner of the bounding box
#'
#' @keywords internal
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
#' the bounding box, either as a [terrainr_coordinate_pair] object
#' or a coordinate pair. If the coordinate pair is not named, it is assumed to
#' be in (lat, lng) format; if it is named, the function will attempt to
#' properly identify coordinates.
#' @param coord_units Arguments passed to [terrainr_coordinate_pair].
#' If \code{bl} and \code{tr} are already [terrainr_coordinate_pair]
#' objects, these arguments are not used.
#'
#' @return An object of class [terrainr_bounding_box].
#'
#' @keywords internal
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
    stop(
      "The given coordinates don't seem to be ",
      "the bottom left and upper right corners."
    )
  }
  methods::new("terrainr_bounding_box",
    bl = bl,
    tr = tr
  )
}
