#' Add a uniform buffer around a bounding box
#'
#' This function calculates the great circle distance both corners of your
#' bounding box are from the centroid and extends those by a set distance. Due
#' to using great circle distance, calculations will not be exact.
#'
#' @param bbox The original bounding box to add a buffer around. If not already
#' a [terrainr::terrainr_bounding_box] object, will be converted.
#' @param distance The distance to add to the buffer.
#' @param distance_unit The units of the distance to add to the buffer, passed
#' to [terrainr::convert_distance].
#'
#' @export
add_bbox_buffer <- function(bbox, distance, distance_unit = "meters") {

  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  centroid <- get_bbox_centroid(bbox)
  corner_distance <- calc_haversine_distance(centroid,
                                             bbox@bl)
  distance <- convert_distance(distance, distance_unit)
  add_distance <- corner_distance + distance
  bl <- point_from_distance(centroid, add_distance, 225)
  tr <- point_from_distance(centroid, add_distance, 45)

  return(terrainr_bounding_box(bl, tr))

}
