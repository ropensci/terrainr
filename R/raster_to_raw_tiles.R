#' Convert raster file into .raw image tiles
#'
#' This function converts raster objects (stored as GeoTIFF files) into .raw
#' image square tiles.
#'
#' @param input_file File path to the input GeoTIFF file to convert.
#' @param output_prefix The file path to prefix output tiles with.
#' @param side_length The side length, in pixels, for the .raw tiles.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' simulated_data <-  data.frame(id = seq(1, 100, 1),
#'                               lat = runif(100, 44.04905, 44.17609),
#'                               lng = runif(100, -74.01188, -73.83493))
#'
#' bbox <- get_coord_bbox(lat = simulated_data$lat, lng = simulated_data$lng)
#' bbox <- add_bbox_buffer(bbox, 100)
#' bbox <- make_unity_friendly(bbox)
#' temptiff <- tempfile(fileext = ".tif")
#' get_heightmap_tiles(temptiff)
#' raster_to_raw_tiles(temptiff, tempfile())
#' }
#'
#' @export
raster_to_raw_tiles <- function(input_file, output_prefix, side_length = 4097) {
  input_raster <- raster::raster(input_file)
  max_raster <- raster::cellStats(input_raster, "max")

  x_tiles <- ceiling(input_raster@nrows / side_length)
  y_tiles <- ceiling(input_raster@ncols / side_length)

  temptiffs <- NULL
  while (length(temptiffs) != x_tiles * y_tiles) {
    temptiffs <- unique(vapply(1:(x_tiles * y_tiles),
                               function(x) tempfile(fileext = ".tiff"),
                               character(1)))
  }

  counter <- 1

  for (i in seq(0, input_raster@nrows, 4097)) {
    for (j in seq(0, input_raster@ncols, 4097)) {
      gdalUtils::gdal_translate(input_file, temptiffs[[counter]],
                                srcwin = paste0(i, ", ", j, ", 4097, 4097")
      )
      names(temptiffs)[[counter]] <-  paste0(output_prefix, "_", i, "_", j, ".raw")
      counter <- counter + 1
    }
  }

  temppngs <- NULL
  while (length(temppngs) != length(temptiffs)) {
    temppngs <- unique(vapply(seq_along(temptiffs),
                              function(x) tempfile(fileext = ".png"),
                              character(1)))
  }
  names(temppngs) <- names(temptiffs)

  for (i in seq_along(temptiffs)) {
    gdalUtilities::gdal_translate(src_dataset = temptiffs[[i]],
                                  dst_dataset = temppngs[[i]],
                                  ot = "UInt16",
                                  strict = FALSE,
                                  scale = c(0, max_raster, 0, (2^16) - 1),
                                  of = "png")
  }

  for (i in seq_along(temppngs)) {
    processing_image <- magick::image_read(temppngs[[i]])
    processing_image <- magick::image_convert(processing_image,
                                              format = "RGB",
                                              depth = 16,
                                              interlace = "Plane")
    magick::image_write(processing_image, names(temppngs)[[i]])
  }

}
