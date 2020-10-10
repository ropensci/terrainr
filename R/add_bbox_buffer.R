#' Add a uniform buffer around a bounding box
#'
#' This function calculates the great circle distance both corners of your
#' bounding box are from the centroid and extends those by a set distance. Due
#' to using Haversine "great circle" distance, calculations will not be exact.
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
#' @return A \code{\link{terrainr_bounding_box}} object.
#'
#' @family utilities
#'
#' @examples
#' add_bbox_buffer(
#'   list(
#'     c(lat = 44.04905, lng = -74.01188),
#'     c(lat = 44.17609, lng = -73.83493)
#'   ),
#'   10
#' )
#'
#' @export
add_bbox_buffer <- function(bbox,
                            distance,
                            distance_unit = "meters",
                            divisible = NULL) {

  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  centroid <- get_bbox_centroid(bbox)
  corner_distance <- calc_haversine_distance(
    centroid,
    bbox@bl
  )
  distance <- convert_distance(distance, distance_unit)
  add_distance <- corner_distance + distance
  bl <- point_from_distance(centroid, add_distance, 225)
  tr <- point_from_distance(centroid, add_distance, 45)

  if (!is.null(divisible)) {
    tl <- c(tr@lat, bl@lng)
    divisible <- convert_distance(divisible, distance_unit)

    x <- ceiling(calc_haversine_distance(tl, tr) / divisible)
    tr <- point_from_distance(tl, divisible * x, 90)

    y <- ceiling(calc_haversine_distance(tl, tr) / divisible)
    bl <- point_from_distance(tl, divisible * y, 180)

  }

  return(terrainr_bounding_box(bl, tr))
}
