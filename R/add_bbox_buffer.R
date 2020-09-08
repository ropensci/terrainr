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
