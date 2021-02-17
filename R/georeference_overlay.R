#' Georeference image overlays based on a reference raster
#'
#' This function georeferences an image overlay based on a reference raster,
#' setting the extent and CRS of the image to those of the raster file. To
#' georeference multiple images and merge them into a single file, see
#' [merge_rasters].
#'
#' @param overlay_file The image overlay to georeference. File format will be
#' detected automatically from file extension; options include `jpeg/jpg`,
#' `png`, and `tif/tiff`.
#' @param reference_raster The raster file to base georeferencing on. The output
#' image will have the same extent and CRS as the reference raster. Accepts both
#' Raster* objects from the `raster` package or a file readable by
#' [raster::raster].
#' @param output_file The path to write the georeferenced image file to. Must
#' be a TIFF.
#'
#' @return The file path written to, invisibly.
#'
#' @family data manipulation functions
#' @family overlay creation functions
#'
#' @examples
#' \dontrun{
#' simulated_data <- data.frame(
#'   id = seq(1, 100, 1),
#'   lat = runif(100, 44.1114, 44.1123),
#'   lng = runif(100, -73.92273, -73.92147)
#' )
#'
#' simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
#'
#' downloaded_tiles <- get_tiles(simulated_data,
#'                               services = c("elevation", "ortho"),
#'                               georeference = FALSE)
#'
#' georeference_overlay(
#'   overlay_file = downloaded_tiles[[2]],
#'   reference_raster = downloaded_tiles[[1]],
#'   output_file = tempfile(fileext = ".tif")
#' )
#' }
#'
#' @export
georeference_overlay <- function(overlay_file,
                                 reference_raster,
                                 output_file = tempfile(fileext = ".tif")) {
  stopifnot(is.character(overlay_file) && length(overlay_file) == 1)
  stopifnot(grepl("tiff?$", output_file))
  file_type <- regmatches(overlay_file, regexpr("\\w*$", overlay_file))

  official_names <- c(
    "jpeg" = "jpg",
    "tiff" = "tif"
  )
  if (file_type %in% official_names) {
    file_type <- names(official_names)[which((official_names) %in% file_type)]
  }

  if (!requireNamespace(file_type, quietly = TRUE)) { # nocov start
    stop(
      "Package ", file_type, " is required to load ", file_type,
      " images.\n", "Please install ", file_type,
      " via install.packages('", file_type, "') to continue."
    )
  } # nocov end

  image_read <- switch(file_type,
    "png" = png::readPNG,
    "tiff" = tiff::readTIFF,
    "jpeg" = jpeg::readJPEG
  )

  if (is.character(reference_raster) && length(reference_raster) == 1) {
    reference_raster <- raster::raster(reference_raster)
  } else {
    stopifnot(any(grepl("^Raster", class(reference_raster))))
  }

  # Need image_read in order for brick to correctly detect scale
  # otherwise assumes 8bit
  overlay_file <- raster::brick(image_read(overlay_file))
  raster::crs(overlay_file) <- reference_raster@crs
  raster::extent(overlay_file) <- reference_raster@extent
  raster::writeRaster(overlay_file, output_file)

  return(invisible(output_file))
}
