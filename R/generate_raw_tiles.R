#' Generate .RAW image tiles of a heightmap
#'
#' This function combines the main functions in the [terrainr] package to create
#' .RAW image tiles of a USGS digital elevation model (DEM) covering an area of
#' interest.
#'
#' @param data Optionally, a dataframe containing vectors of latitude and
#' longitude.
#' @param lat If \code{data} is not \code{NULL}, the name of the column
#' containing latitude values. If \code{data} is \code{NULL}, a vector of
#' latitude values.
#' @param lng If \code{data} is not \code{NULL}, the name of the column
#' containing longitude values. If \code{data} is \code{NULL}, a vector of
#' longitude values.
#' @param raw_output_prefix A string indicating the prefix to label output files
#' with. Files are named in the pattern \code{output_prefix_x_y.raw} where
#' \code{x} and \code{y} are the x and y position in pixels of the upper left
#' corner.
#' @param bbox Optionally, a bounding box representing the area to pull a DEM
#' for. If provided, arguments to \code{data}, \code{lat}, and \code{lng} are
#' ignored.
#' @param buffer_distance Optionally, a numeric value indicating how much to pad
#' the bounding box by.
#' @param distance_unit A string passed to [terrainr::convert_distance]
#' specifying the units of the \code{buffer_distance} argument.
#' @param unity_friendly Logical: should the bounding box be padded to ensure
#' the output .raw tiles can be easily imported to Unity?
#' @param tif_filename The path to save the DEM raster file to. Must end with
#' \code{.tif} or \code{.tiff}. Ignored if \code{save_tif} is \code{FALSE}.
#' @param save_tif Logical: should the DEM raster be saved as a TIF in addition
#' to .raw tiles?
#'
#' @return NULL
#'
#' @export
generate_raw_tiles <- function(data = NULL, lat = NULL, lng = NULL,
                               raw_output_prefix, bbox = NULL,
                               buffer_distance = NULL, distance_unit = "meters",
                               unity_friendly = TRUE, tif_filename = NULL,
                               save_tif = TRUE) {
  eval(raw_output_prefix)
  if (is.null(bbox)) {
    bbox <- get_coord_bbox(data, lat, lng)
  }
  if (!is.null(buffer_distance)) {
    bbox <- add_bbox_buffer(bbox, buffer_distance, distance_unit)
  }
  if (!is.null(unity_friendly) && unity_friendly) {
    bbox <- make_unity_friendly(bbox)
  }
  if (!save_tif) tif_filename <- tempfile(fileext = ".tif")
  if (is.null(tif_filename)) stop("Please provide a tif_filename to save TIFF")
  get_heightmap(bbox, tif_filename)
  raster_to_raw_tiles(tif_filename, raw_output_prefix)
  return(NULL)
}
