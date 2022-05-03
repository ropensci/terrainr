#' Crop a raster and convert the output tiles into new formats.
#'
#' This function has been deprecated as of terrainr 0.5.0 in favor of the new
#' function, [make_manifest]. While it will be continued to be exported until
#' at least 2022, improvements and bug fixes will only be made to the new
#' function. Please open an issue if any features you relied upon is
#' missing from the new function!
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
#' @family visualization functions
#'
#' @return Invisibly, a character vector containing the file paths that were
#' written to.
#'
#' @examples
#' \dontrun{
#' if (!isTRUE(as.logical(Sys.getenv("CI")))) {
#'   simulated_data <- data.frame(
#'     id = seq(1, 100, 1),
#'     lat = runif(100, 44.04905, 44.17609),
#'     lng = runif(100, -74.01188, -73.83493)
#'   )
#'   simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
#'   output_files <- get_tiles(simulated_data)
#'   temptiff <- tempfile(fileext = ".tif")
#'   merge_rasters(output_files["elevation"][[1]], temptiff)
#'   raster_to_raw_tiles(temptiff, tempfile())
#' }
#' }
#'
#' @export
raster_to_raw_tiles <- function(input_file,
                                output_prefix,
                                side_length = 4097,
                                raw = TRUE) {
  .Deprecated(
    "transform_elevation",
    "terrainr",
    msg = paste("'raster_to_raw_tiles' is deprecated as of terrainr 0.5.0.",
      "Use 'transform_elevation' instead.",
      sep = "\n"
    )
  )

  if (raw) {
    transform_elevation(heightmap = input_file,
                        side_length = side_length,
                        output_prefix = output_prefix)
  } else {
    transform_overlay(overlay = input_file,
                      side_length = side_length,
                      output_prefix = output_prefix)
  }

}
