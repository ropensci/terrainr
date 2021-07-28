#' Extract latitude and longitude from a provided object
#'
#' This is an internal utility function to convert bounding boxes into
#' coordinate pairs.
#'
#' @param point_1,point_2 Coordinate pairs (as length-2 numeric vectors with the
#' names "lat" and "lng") to calculate distance between.
#' @keywords internal
#'
#' @family utilities
#'
#' @return A vector of length 1 containing distance between points
calc_haversine_distance <- function(point_1, point_2) {
  points <- lapply(
    list(point_1, point_2), # list, not c, since these are both numeric vectors
    function(x) {
      if (!methods::is(x, "terrainr_coordinate_pair")) {
        x <- terrainr_coordinate_pair(x)
      }
      x
    }
  )

  r <- 6371e3 # Radius of the earth in m

  used_pts <- vapply(
    list(
      "lat1" = points[[1]]@lat,
      "lat2" = points[[2]]@lat,
      "delta_lat" = points[[2]]@lat - points[[1]]@lat,
      "delta_lng" = points[[2]]@lng - points[[1]]@lng
    ),
    deg_to_rad,
    numeric(1)
  )

  a <- (sin(used_pts[["delta_lat"]] / 2) * sin(used_pts[["delta_lat"]] / 2)) +
    (cos(used_pts[["lat1"]]) * cos(used_pts[["lat2"]]) *
      sin(used_pts[["delta_lng"]] / 2) * sin(used_pts[["delta_lng"]] / 2))
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  # c inherits the name "lat" from a
  # which means this returns a length(1) vector of distance named... "lat"
  # so we'll get rid of that
  names(c) <- NULL

  return(r * c)
}
