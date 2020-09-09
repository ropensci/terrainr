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
#' @param bbox Optionally, a bounding box representing the area to pull a DEM
#' for. If provided, arguments to \code{data}, \code{lat}, and \code{lng} are
#' ignored.
#'
#' @return NULL
#'
#' @export
generate_raw_tiles <- function(data = NULL, lat = NULL, lng = NULL, bbox = NULL,
                               buffer_distance = NULL, unity_friendly = NULL,
                               tif_filename = NULL, raw_output_prefix,
                               save_tif = TRUE) {
  eval(raw_output_prefix)
  if (is.null(bbox)) {
    bbox <- get_coord_bbox(data, lat, lng)
  }
  if (!is.null(buffer_distance)) bbox <- add_bbox_buffer(bbox, buffer_distance)
  if (!is.null(unity_friendly) && unity_friendly) bbox <- make_unity_friendly(bbox)
  if (!save_tif) tif_filename <- tempfile(fileext = ".tif")
  if (is.null(tif_filename)) stop("Please provide a tif_filename to save TIFF")
  get_heightmap_tiles(bbox, tif_filename)
  raster_to_raw_tiles(tif_filename, raw_output_prefix)
  return(NULL)
}
