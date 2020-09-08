#' Get bounding box for set of coordinate points
#'
#' @param lat A vector of latitudes
#' @param lng A vector of longitudes
#'
#' @return A list of length 2, containing the bottom-left (named "bl") and
#' top-right (named "tr") coordinates of the bounding box.
#'
#' @examples
#' df <- data.frame(
#'   lat = c(44.05771, 44.18475),
#'   lng = c(-73.99212, -73.81515)
#' )
#' get_coord_bounding_box(df$lat, df$lng)
#' @export
get_coord_bbox <- function(lat, lng) {
  stopifnot(length(lat) == length(lng))

  if (any(is.na(lat) | is.na(lng))) {
    warning("NAs present in coordinate data and will be ignored.")
  }

  minlat <- min(lat, na.rm = TRUE)
  minlng <- min(lng, na.rm = TRUE)
  maxlat <- max(lat, na.rm = TRUE)
  maxlng <- max(lng, na.rm = TRUE)

  return(list(
    "bl" = c("lat" = minlat, "lng" = minlng),
    "tr" = c("lat" = maxlat, "lng" = maxlng)
  ))
}

