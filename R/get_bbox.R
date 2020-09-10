#' Get bounding box for set of coordinate points
#'
#' @param data Optionally, a dataframe containing vectors of latitude and
#' longitude.
#' @param lat If \code{data} is not \code{NULL}, the name of the column
#' containing latitude values. If \code{data} is \code{NULL}, a vector of
#' latitude values.
#' @param lng If \code{data} is not \code{NULL}, the name of the column
#' containing longitude values. If \code{data} is \code{NULL}, a vector of
#' longitude values.
#'
#' @return A list of length 2, containing the bottom-left (named "bl") and
#' top-right (named "tr") coordinates of the bounding box.
#'
#' @examples
#' df <- data.frame(
#'   lat = c(44.05771, 44.18475),
#'   lng = c(-73.99212, -73.81515)
#' )
#' get_coord_bbox(df, lat, lng)
#' get_coord_bbox(lat = df$lat, lng = df$lng)
#'
#' @export
get_coord_bbox <- function(data = NULL, lat, lng) {

  if (!is.null(data)) {
    lat <- quote(lat)
    lng <- quote(lng)
    lat_vals <- data[[lat]]
    lng_vals <- data[[lng]]
  } else {
    lat_vals <- lat
    lng_vals <- lng
  }

  if (any(is.na(lat_vals) | is.na(lng_vals))) {
    warning("NAs present in coordinate data will be ignored.")
  }

  minlat <- min(lat_vals, na.rm = TRUE)
  minlng <- min(lng_vals, na.rm = TRUE)
  maxlat <- max(lat_vals, na.rm = TRUE)
  maxlng <- max(lng_vals, na.rm = TRUE)

  return(
    terrainr_bounding_box(c("lat" = minlat, "lng" = minlng),
                          c("lat" = maxlat, "lng" = maxlng))
    )
}

