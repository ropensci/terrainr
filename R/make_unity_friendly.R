# TODO: document
#' @export
make_unity_friendly <- function(bbox) {

  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  tr <- bbox@tr
  bl <- bbox@bl

  tl <- terrainr_coordinate_pair(c(tr@lat,
                                   bl@lng))

  x_tiles <- ceiling((calc_haversine_distance(tl, tr) / 4097))
  y_tiles <- ceiling(calc_haversine_distance(tl, bl) / 4097)

  return(terrainr_bounding_box(point_from_distance(tl, x_tiles * 4097, 90),
                               point_from_distance(tl, y_tiles * 4097, 180)))

}
