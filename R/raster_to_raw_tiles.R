#' Crop a raster and convert the output tiles into new formats.
#'
#' This function crops input raster files into smaller square tiles and then
#' converts them into either .png or .raw files which are ready to be imported
#' into the Unity game engine.
#'
#' @param input_file File path to the input TIFF file to convert.
#' @param output_prefix The file path to prefix output tiles with.
#' @param side_length The side length, in pixels, for the .raw tiles.
#' @param raw Logical: Convert the cropped tiles to .raw? When \code{FALSE}
#' returns a .png.
#'
#' @family data manipulation functions
#'
#' @return Invisibly, a character vector containing the file paths that were
#' written to.
#'
#' @examples
#' \dontrun{
#' simulated_data <- data.frame(
#'   id = seq(1, 100, 1),
#'   lat = runif(100, 44.04905, 44.17609),
#'   lng = runif(100, -74.01188, -73.83493)
#' )
#' bbox <- get_coord_bbox(lat = simulated_data$lat, lng = simulated_data$lng)
#' bbox <- add_bbox_buffer(bbox, 100)
#' output_files <- get_tiles(bbox)
#' temptiff <- tempfile(fileext = ".tif")
#' merge_rasters(output_files["3DEPElevation"][[1]], temptiff)
#' raster_to_raw_tiles(temptiff, tempfile())
#' }
#'
#' @export
raster_to_raw_tiles <- function(input_file,
                                output_prefix,
                                side_length = 4097,
                                raw = TRUE) {
  input_raster <- raster::raster(input_file)
  max_raster <- raster::cellStats(input_raster, "max")

  x_tiles <- ceiling(input_raster@nrows / side_length)
  y_tiles <- ceiling(input_raster@ncols / side_length)
  if (any(grepl("progressr", utils::installed.packages()))) {
    p <- progressr::progressor(steps = x_tiles * y_tiles * 3)
  }

  temptiffs <- NULL
  while (length(temptiffs) != x_tiles * y_tiles) {
    temptiffs <- unique(vapply(
      1:(x_tiles * y_tiles),
      function(x) tempfile(fileext = ".tiff"),
      character(1)
    ))
  }

  x_tiles <- 0:(x_tiles - 1)
  x_tiles <- (x_tiles * side_length)

  y_tiles <- 0:(y_tiles - 1)
  y_tiles <- (y_tiles * side_length)


  counter <- 1

  for (i in seq_along(x_tiles)) {
    for (j in seq_along(y_tiles)) {
      if (any(grepl("progressr", utils::installed.packages()))) {
        p(message = sprintf(
          "Cropping tile (%d,%d)",
          x_tiles[[i]],
          y_tiles[[j]]
        ))
      }
      gdalUtils::gdal_translate(input_file, temptiffs[[counter]],
        srcwin = paste0(
          x_tiles[[i]],
          ", ",
          y_tiles[[j]],
          ", ",
          side_length,
          ", ",
          side_length
        )
      )
      names(temptiffs)[[counter]] <- paste0(
        output_prefix,
        "_",
        i,
        "_",
        j,
        ifelse(raw, ".raw", ".png")
      )
      counter <- counter + 1
    }
  }

  temppngs <- NULL
  if (raw) {
    while (length(temppngs) != length(temptiffs)) {
      temppngs <- unique(vapply(
        seq_along(temptiffs),
        function(x) tempfile(fileext = ".png"),
        character(1)
      ))
    }
  } else {
    temppngs <- names(temptiffs)
  }

  names(temppngs) <- names(temptiffs)

  mapply(
    function(x, y) {
      if (any(grepl("progressr", utils::installed.packages()))) {
        p(message = sprintf("Converting tile %s to PNG", x))
      }
      gdalUtilities::gdal_translate(
        src_dataset = x,
        dst_dataset = y,
        ot = "UInt16",
        strict = FALSE,
        scale = c(0, max_raster, 0, (2^16) - 1),
        of = "png"
      )
    },
    temptiffs,
    temppngs
  )

  mapply(
    function(x, y) {
      processing_image <- magick::image_read(png::readPNG(x))

      if (any(grepl("progressr", utils::installed.packages()))) {
        if (raw) {
          p(message = sprintf("Converting tile %s to RAW", x))
        } else {
          p(message = sprintf("Flipping tile %s for Unity", x))
        }
      }

      if (raw) {
        processing_image <- magick::image_flop(processing_image)
        processing_image <- magick::image_convert(processing_image,
          format = "RGB",
          depth = 16,
          interlace = "Plane"
        )
      } else {
        processing_image <- magick::image_flip(processing_image)
        processing_image <- magick::image_flop(processing_image)
      }

      magick::image_write(processing_image, y)
    },
    temppngs,
    names(temppngs)
  )

  return(names(temppngs))
}
