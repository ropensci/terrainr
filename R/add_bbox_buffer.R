#' @name addbuff
#' @title Add a uniform buffer around a bounding box
#'
#' @description
#' [add_bbox_buffer] calculates the great circle distance both corners of
#' your bounding box are from the centroid and extends those by a set distance.
#' Due to using Haversine "great circle" distance, calculations will not be
#' exact.
#'
#' [set_bbox_side_length] is a thin wrapper around [add_bbox_buffer] which sets
#' all sides of the bounding box to (approximately) a specified length.
#'
#' @param bbox The original bounding box to add a buffer around. If not already
#' a \code{\link{terrainr_bounding_box}} object, will be converted.
#' @param distance The distance to add to the buffer.
#' @param distance_unit The units of the distance to add to the buffer, passed
#' to \code{\link{convert_distance}}.
#' @param divisible Numeric: Extend the top right and bottom left corners so
#' that each side is divisible by \code{divisible}. Leave set to \code{NULL} to
#' not extend. This argument is in the same units as \code{distance_unit}.
#'
#' @return A [terrainr_bounding_box] object.
#'
#' @family utilities
NULL

#' @rdname addbuff
#' @examples
#' add_bbox_buffer(
#'   list(
#'     c(lat = 44.04905, lng = -74.01188),
#'     c(lat = 44.17609, lng = -73.83493)
#'   ),
#'   10
#' )
#' @export
#' @md
add_bbox_buffer <- function(bbox,
                            distance,
                            distance_unit = "meters",
                            divisible = NULL) {
  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr::terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  centroid <- terrainr::get_bbox_centroid(bbox)
  corner_distance <- terrainr::calc_haversine_distance(
    centroid,
    bbox@bl
  )
  distance <- terrainr::convert_distance(distance, distance_unit)
  add_distance <- corner_distance + distance
  bl <- terrainr::point_from_distance(centroid, add_distance, 225)
  tr <- terrainr::point_from_distance(centroid, add_distance, 45)

  if (!is.null(divisible)) {
    tl <- c(tr@lat, bl@lng)
    divisible <- terrainr::convert_distance(divisible, distance_unit)

    x <- ceiling(terrainr::calc_haversine_distance(tl, tr) / divisible)
    tr <- terrainr::point_from_distance(tl, divisible * x, 90)

    y <- ceiling(terrainr::calc_haversine_distance(tl, tr) / divisible)
    bl <- terrainr::point_from_distance(tl, divisible * y, 180)
  }

  return(terrainr::terrainr_bounding_box(bl, tr))
}

#' @rdname addbuff
#' @examples
#' set_bbox_side_length(
#'   list(
#'     c(lat = 44.04905, lng = -74.01188),
#'     c(lat = 44.17609, lng = -73.83493)
#'   ),
#'   4000
#' )
#' @export
set_bbox_side_length <- function(bbox,
                                 distance,
                                 distance_unit = "meters") {
  center <- terrainr::export_coord_pair(terrainr::get_bbox_centroid(bbox))
  terrainr::add_bbox_buffer(
    list(tr = c(lat = center[["lat"]], lng = center[["lng"]]),
         bl = c(lat = center[["lat"]] - 0.000001,
                lng = center[["lng"]] - 0.000001)),
    distance = sqrt((distance^2) * 2) / 2,
    distance_unit = distance_unit
  )
}
