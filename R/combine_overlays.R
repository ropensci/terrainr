#' Combine multiple image overlays into a single file
#'
#' This function combines any number of images into a single file, which may
#' then be further processed as an image or transformed into an image overlay.
#'
#' @param ... File paths for images to be combined. Note that combining TIFF
#' images requires the `tiff` package be installed.
#' @param output_file The path to save the resulting image to. Can
#' be any format accepted by [magick::image_read]. Optionally, can be set to
#' `NULL`, in which case this function will return the image as a `magick`
#' object instead of writing to disk.
#' @param transparency A value indicating how much transparency should be added
#' to each image. If less than 1, interpreted as a proportion (so a value of
#' 0.1 results in each image becoming 10% more transparent); if between 1 and
#' 100, interpreted as a percentage (so a value of 10 results in each image
#' becoming 10% more transparent.) A value of 0 is equivalent to no
#' additional transparency.
#'
#' @examples
#' \dontrun{
#' # Generate points and download orthoimagery
#' mt_elbert_points <- data.frame(
#'   lat = runif(100, min = 39.11144, max = 39.12416),
#'   lng = runif(100, min = -106.4534, max = -106.437)
#' )
#'
#' mt_elbert_sf <- sf::st_as_sf(mt_elbert_points, coords = c("lng", "lat"))
#' sf::st_crs(mt_elbert_sf) <- sf::st_crs(4326)
#'
#' output_files <- get_tiles(
#'   mt_elbert_sf,
#'   output_prefix = tempfile(),
#'   services = c("ortho")
#' )
#'
#' # Merge orthoimagery into a single file
#' ortho_merged <- merge_rasters(
#'   input_rasters = output_files[1],
#'   output_raster = tempfile(fileext = ".tif")
#' )
#'
#' # Convert our points into an overlay
#' mt_elbert_overlay <- vector_to_overlay(mt_elbert_sf,
#'   ortho_merged[[1]],
#'   size = 15,
#'   color = "red",
#'   na.rm = TRUE
#' )
#'
#' # Combine the overlay with our orthoimage
#' ortho_with_points <- combine_overlays(
#'   ortho_merged[[1]],
#'   mt_elbert_overlay
#' )
#' }
#'
#' @family data manipulation functions
#' @family overlay creation functions
#' @family visualization functions
#'
#' @return If `output_file` is not null, `output_file`, invisibly. If
#' `output_file` is null, a `magick` image object.
#'
#' @export
#' @md
combine_overlays <- function(...,
                             output_file = tempfile(fileext = ".png"),
                             transparency = 0) {
  dots <- list(...)

  stopifnot(transparency >= 0)

  if (transparency > 1) transparency <- transparency / 100
  if (transparency > 0) transparency <- 1 - transparency

  for (i in seq_len(length(dots))) {
    file_type <- regmatches(dots[[i]], regexpr("\\w*$", dots[[i]]))

    if (file_type %in% c("tif", "tiff")) {
      rlang::check_installed("tiff") # nocov
      current_image <- magick::image_read(
        # geoTIFF contain headers that readTIFF ignores with a warning
        #
        # since that means ~100% of uses of this function will warn,
        # even though we're expecting the behavior be ignored,
        # suppress warnings here.
        suppressWarnings(tiff::readTIFF(dots[[i]]))
      )
    } else {
      current_image <- magick::image_read(dots[[i]])
    }

    if (transparency > 0) {
      pixels <- magick::image_data(current_image, "rgba")
      pixels[4, , ] <- as.raw(round(as.integer(pixels[4, , ])) * transparency)
      current_image <- magick::image_read(pixels)
    }

    if (exists("image_storage")) {
      image_storage <- c(image_storage, current_image)
    } else {
      image_storage <- current_image
    }
  }

  img_out <- magick::image_mosaic(image_storage)
  if (is.null(output_file)) {
    return(img_out)
  }
  magick::image_write(img_out, output_file)
  return(invisible(output_file))
}
