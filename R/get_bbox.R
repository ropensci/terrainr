#' Get bounding box for set of coordinate points.
#'
#' This function returns a \code{\link{terrainr_bounding_box}} object
#' representing the bottom left and upper right corners of the smallest
#' rectangle containing your data. If you only have one data point for either
#' latitude or longitude, this function will buffer it in both directions by
#' 1e-10 in order to return a rectangle with a real "bottom left" and
#' "upper right".
#'
#' @param data Optionally, a dataframe containing vectors of latitude and
#' longitude.
#' @param lat If \code{data} is not \code{NULL}, the name of the column
#' containing latitude values. If \code{data} is \code{NULL}, a vector of
#' latitude values.
#' @param lng If \code{data} is not \code{NULL}, the name of the column
#' containing longitude values. If \code{data} is \code{NULL}, a vector of
#' longitude values.
#' @param na.rm Logical: Silently remove NA values? If \code{NULL}, the default,
#' will warn if there are NAs. If \code{FALSE}, will raise an error on NA.
#'
#' @family utilities
#'
#' @return A \code{\link{terrainr_bounding_box}} object.
#'
#' @examples
#' df <- data.frame(
#'   lat = c(44.05771, 44.18475),
#'   lng = c(-73.99212, -73.81515)
#' )
#' get_coord_bbox(df, "lat", "lng")
#' get_coord_bbox(lat = df$lat, lng = df$lng)
#' @export
get_coord_bbox <- function(data = NULL, lat, lng, na.rm = NULL) {
  if (!is.null(data)) {
    lat_vals <- data[[lat]]
    lng_vals <- data[[lng]]
  } else {
    lat_vals <- lat
    lng_vals <- lng
  }

  if (any(is.na(lat_vals) | is.na(lng_vals))) {
    if (is.null(na.rm)) {
      warning("NAs present in coordinate data will be ignored.")
    } else if (!na.rm) {
      stop("NAs present in coordinate data.")
    }
  }


  # let people get bounding boxes for a single point
  if (length(lat_vals) == 1) {
    lat_vals <- c(lat_vals - 1e-10, lat_vals + 1e-10)
  }

  if (length(lng_vals) == 1) {
    lng_vals <- c(lng_vals - 1e-10, lng_vals + 1e-10)
  }

  minlat <- min(lat_vals, na.rm = TRUE)
  minlng <- min(lng_vals, na.rm = TRUE)
  maxlat <- max(lat_vals, na.rm = TRUE)
  maxlng <- max(lng_vals, na.rm = TRUE)

  return(
    terrainr::terrainr_bounding_box(
      c("lat" = minlat, "lng" = minlng),
      c("lat" = maxlat, "lng" = maxlng)
    )
  )
}
