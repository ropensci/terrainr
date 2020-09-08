# TODO: document
#' @export
raster_to_raw <- function(filenames) {
  tempfiles <- NULL
  while (length(tempfiles) != length(filenames)) {
    tempfiles <- unique(vapply(filenames,
                               function(x) tempfile(fileext = ".png"),
                               character(1)))
  }

  names(tempfiles) <- filenames
  max_raster <- 0
  for (i in seq_along(tempfiles)) {
    temp_raster <- raster::raster(names(tempfiles)[[i]])
    max_raster <- max(raster::cellStats(temp_raster, "max"), max_raster)
  }
  for (i in seq_along(tempfiles)) {
    gdalUtilities::gdal_translate(src_dataset = names(tempfiles)[[i]],
                                  dst_dataset = tempfiles[[i]],
                                  ot = "UInt16",
                                  strict = FALSE,
                                  scale = c(0, max_raster, 0, (2^16) - 1),
                                  of = "png")
  }
  # extract file names sans extensions
  names(tempfiles) <- paste0(regmatches(filenames,
                                        regexpr(".*(?=\\.[[:alnum:]]*$)",
                                                filenames,
                                                perl = TRUE)),
                             ".raw")

  for (i in seq_along(tempfiles)) {
    processing_image <- magick::image_read(tempfiles[[i]])
    processing_image <- magick::image_convert(processing_image,
                                              format = "RGB",
                                              depth = 16,
                                              interlace = "Plane")
    magick::image_write(processing_image, names(tempfiles)[[i]])
  }

}
