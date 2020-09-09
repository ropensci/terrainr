#' Add a uniform buffer around a bounding box
#'
#' This function calculates the great circle distance both corners of your
#' bounding box are from the centroid and extends those by a set distance. Due
#' to using great circle distance, calculations will not be exact.
#'
#' @param bbox The original bounding box to add a buffer around.
#'
#' @export
add_bbox_buffer <- function(bbox, distance) {

  centroid <- get_bbox_centroid(bbox)
  corner_distance <- calc_haversine_distance(centroid,
                                             bbox[[1]])
  add_distance <- corner_distance + distance
  bl <- point_from_distance(centroid, add_distance, 225)
  tr <- point_from_distance(centroid, add_distance, 45)

  return(list(
    bl = bl,
    tr = tr
  ))

}
