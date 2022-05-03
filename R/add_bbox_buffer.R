#' @name addbuff
#' @title Add a uniform buffer around a bounding box for geographic coordinates
#'
#' @description
#' [add_bbox_buffer] calculates the great circle distance both corners of
#' your bounding box are from the centroid and extends those by a set distance.
#' Due to using Haversine/great circle distance, latitude/longitude calculations
#' will not be exact.
#'
#' [set_bbox_side_length] is a thin wrapper around [add_bbox_buffer] which sets
#' all sides of the bounding box to (approximately) a specified length.
#'
#' Both of these functions are intended to be used with geographic coordinate
#' systems (data using longitude and latitude for position). For projected
#' coordinate systems, a more sane approach is to use [sf::st_buffer] to add a
#' buffer, or combine [sf::st_centroid] with the buffer to set a specific side
#' length.
#'
#' @param data The original data to add a buffer around. Must be either an `sf`
#' or `Raster` object.
#' @param distance The distance to add or to set side lengths equal to.
#' @param distance_unit The units of the distance to add to the buffer, passed
#' to [units::as_units].
#' @param error_crs Logical: Should this function error if `data` has no CRS?
#' If `TRUE`, function errors; if `FALSE`, function quietly assumes EPSG:4326.
#' If `NULL`, the default, function assumes EPSG:4326 with a warning.
#'
#' @return An `sfc` object (from [sf::st_as_sfc]).
#'
#' @family utilities
NULL

#' @rdname addbuff
#' @examples
#'
#' df <- data.frame(
#'   lat = c(44.04905, 44.17609),
#'   lng = c(-74.01188, -73.83493)
#' )
#'
#' df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))
#' df_sf <- sf::st_set_crs(df_sf, 4326)
#'
#' add_bbox_buffer(df_sf, 10)
#' @export
#' @md
add_bbox_buffer <- function(data,
                            distance,
                            distance_unit = "meters",
                            error_crs = NULL) {
  projected <- sf::st_is_longlat(data)
  if (!is.na(projected) && !projected) {
    warning(
      "add_bbox_buffer and set_bbox_side_length only make sense for geographic coordinate systems.", # nolint
      "Consider using sf::st_buffer instead."
    )
  }

  UseMethod("add_bbox_buffer")
}

#' @rdname addbuff
#' @export
add_bbox_buffer.sf <- function(data,
                               distance,
                               distance_unit = "meters",
                               error_crs = NULL) {
  input_crs <- sf::st_crs(data)$wkt

  if (is.na(input_crs)) {
    if (is.null(error_crs)) {
      warning("No CRS associated with input data. Assuming EPSG:4326.\n")
    } else if (error_crs) {
      stop("No CRS associated with input data.")
    }
    input_crs <- 4326
    data <- sf::st_set_crs(data, input_crs)
  }


  units(distance) <- units::as_units(distance_unit)

  bbox <- sf::st_bbox(data)
  bbox_sfc <- sf::st_as_sfc(bbox)
  units(distance) <- distance_unit
  bbox <- tryCatch(
    {
      # force an error before the warning if it'll be a problem
      ignored <- units::as_units("degree")
      ignored + distance
      # If distance will error, we're already in the second method now.
      # If it'll only warn, return the sf version
      sf::st_buffer(bbox_sfc, distance)
    },
    error = function(e) {
      centroid <- get_centroid(
        lat = c(bbox[["ymin"]], bbox[["ymax"]]),
        lng = c(bbox[["xmin"]], bbox[["xmax"]])
      )
      corner_distance <- calc_haversine_distance(
        centroid,
        c(lng = bbox[["xmin"]], lat = bbox[["ymin"]])
      )
      units(corner_distance) <- units::as_units("meter")
      # This forces add_distance into meters since corner_distance is first
      add_distance <- corner_distance + distance
      # Now drop units for trig to not give warnings
      units(add_distance) <- units::as_units(NULL)
      bl <- point_from_distance(centroid, add_distance, 225)
      tr <- point_from_distance(centroid, add_distance, 45)
      output <- stats::setNames(
        c(bl@lng, bl@lat, tr@lng, tr@lat),
        c("xmin", "ymin", "xmax", "ymax")
      )
      class(output) <- "bbox"
      sf::st_as_sfc(output)
    }
  )

  return(sf::st_set_crs(bbox, input_crs))
}

#' @rdname addbuff
#' @export
add_bbox_buffer.Raster <- function(data,
                                   distance,
                                   distance_unit = "meters",
                                   error_crs = NULL) {
  tmp <- tempfile(fileext = ".tiff")
  raster::writeRaster(data, tmp)
  data <- terra::rast(tmp)
  add_bbox_buffer(
    data = data,
    distance = distance,
    distance_unit = distance_unit,
    error_crs = error_crs
  )
}

#' @rdname addbuff
#' @export
add_bbox_buffer.SpatRaster <- function(data,
                                       distance,
                                       distance_unit = "meters",
                                       error_crs = NULL) {
  bbox <- as.vector(terra::ext(data))
  data_sf <- data.frame(
    lat = c(bbox[[3]], bbox[[4]]),
    lng = c(bbox[[1]], bbox[[2]])
  )
  data_sf <- sf::st_as_sf(data_sf, coords = c("lng", "lat"))
  data_sf <- sf::st_set_crs(data_sf, sf::st_crs(data))
  add_bbox_buffer(data_sf,
                  distance = distance,
                  distance_unit = distance_unit,
                  error_crs = error_crs
  )
}



#' @rdname addbuff
#' @examples
#'
#' df <- data.frame(
#'   lat = c(44.04905, 44.17609),
#'   lng = c(-74.01188, -73.83493)
#' )
#'
#' df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))
#' df_sf <- sf::st_set_crs(df_sf, 4326)
#'
#' set_bbox_side_length(df_sf, 4000)
#' @export
set_bbox_side_length <- function(data,
                                 distance,
                                 distance_unit = "meters",
                                 error_crs = NULL) {
  UseMethod("set_bbox_side_length")
}

#' @rdname addbuff
#' @export
set_bbox_side_length.sf <- function(data,
                                    distance,
                                    distance_unit = "meters",
                                    error_crs = NULL) {
  bbox <- sf::st_bbox(data)
  center <- get_centroid(
    lat = c(bbox[["ymin"]], bbox[["ymax"]]),
    lng = c(bbox[["xmin"]], bbox[["xmax"]])
  )
  data_sf <- data.frame(
    lat = c(center[["lat"]], center[["lat"]] - 0.000001),
    lng = c(center[["lng"]], center[["lng"]] - 0.000001)
  )

  data_sf <- sf::st_as_sf(data_sf, coords = c("lng", "lat"))
  data_sf <- sf::st_set_crs(data_sf, sf::st_crs(data))

  add_bbox_buffer(
    data_sf,
    distance = sqrt((distance^2) * 2) / 2,
    distance_unit = distance_unit,
    error_crs = error_crs
  )
}

#' @rdname addbuff
#' @export
set_bbox_side_length.Raster <- function(data,
                                        distance,
                                        distance_unit = "meters",
                                        error_crs = NULL) {
  data <- terra::rast(data@file@name)
  set_bbox_side_length(
    data = data,
    distance = distance,
    distance_unit = distance_unit,
    error_crs = error_crs
  )
}


#' @rdname addbuff
#' @export
set_bbox_side_length.SpatRaster <- function(data,
                                            distance,
                                            distance_unit = "meters",
                                            error_crs = NULL) {
  bbox <- terra::ext(data)@ptr$vector
  data_sf <- data.frame(
    lat = c(bbox[[3]], bbox[[4]]),
    lng = c(bbox[[1]], bbox[[2]])
  )
  data_sf <- sf::st_as_sf(data_sf, coords = c("lng", "lat"))
  data_sf <- sf::st_set_crs(data_sf, sf::st_crs(data))
  set_bbox_side_length(
    data_sf,
    distance = distance,
    distance_unit = distance_unit,
    error_crs = error_crs
  )
}
