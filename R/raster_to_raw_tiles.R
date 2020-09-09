# TODO: document
#' @export
raster_to_raw_tiles <- function(input_file, output_prefix) {
  input_raster <- raster::raster(input_file)
  max_raster <- raster::cellStats(input_raster, "max")

  x_tiles <- ceiling(input_raster@nrows / 4097)
  y_tiles <- ceiling(input_raster@ncols / 4097)

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
