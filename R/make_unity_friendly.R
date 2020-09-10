#' Resize bounding box to reduce number of blank tiles created
#'
#' @param bbox A bounding box representing the area to be resized. If not
#' a [terrainr::terrainr_bounding_box] object the function will attempt to
#' convert it.
#' @param size The side length, in pixels, of your eventual .raw tiles.
#'
#' @return A resized [terrainr::terrainr_bounding_box] object.
#'
#' @examples
#' simulated_data <-  data.frame(id = seq(1, 100, 1),
#'                               lat = runif(100, 44.04905, 44.17609),
#'                               lng = runif(100, -74.01188, -73.83493))
#' bbox <- get_coord_bbox(lat = simulated_data$lat, lng = simulated_data$lng)
#' make_unity_friendly(bbox)
#'
#' @export
make_unity_friendly <- function(bbox, size = 4097) {

  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  tr <- bbox@tr
  bl <- bbox@bl

  tl <- terrainr_coordinate_pair(c(tr@lat,
                                   bl@lng))

  x_tiles <- ceiling((calc_haversine_distance(tl, tr) / size))
  y_tiles <- ceiling(calc_haversine_distance(tl, bl) / size)

  return(terrainr_bounding_box(point_from_distance(tl, x_tiles * size, 90),
                               point_from_distance(tl, y_tiles * size, 180)))

}
