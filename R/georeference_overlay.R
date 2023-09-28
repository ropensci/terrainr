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
#' image will have the same extent and CRS as the reference raster. Accepts
#' anything that can be read by [terra::rast]
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
#'   services = c("elevation", "ortho"),
#'   georeference = FALSE
#' )
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

  reference_raster <- terra::rast(reference_raster)

  official_names <- c(
    "jpeg" = "jpg",
    "tiff" = "tif"
  )
  if (file_type %in% official_names) {
    file_type <- names(official_names)[which((official_names) %in% file_type)]
  }

  rlang::check_installed(file_type)

  image_read <- switch(file_type,
    "png" = png::readPNG,
    "tiff" = \(x) suppressWarnings(tiff::readTIFF(x)),
    "jpeg" = jpeg::readJPEG
  )

    # Need image_read in order for brick to correctly detect scale
  # otherwise assumes 8bit
  overlay_file <- terra::rast(image_read(overlay_file))
  terra::crs(overlay_file) <- terra::crs(reference_raster)
  terra::ext(overlay_file) <- terra::ext(reference_raster)
  terra::writeRaster(overlay_file, output_file)

  return(invisible(output_file))
}
