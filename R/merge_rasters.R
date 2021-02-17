#' Merge multiple raster files into a single raster
#'
#' Some functions like [get_tiles] return multiple separate files
#' when it can be useful to have a single larger raster instead. This function
#' is a thin wrapper over [sf::gdal_utils(util = "warp")], making it easy to
#' collapse those multiple raster files into a single TIFF.
#'
#' @param input_rasters A character vector containing the file paths to the
#' georeferenced rasters you want to use.
#' @param output_raster The file path to save the merged georeferenced raster
#' to.
#' @param options Optionally, a character vector of options to be passed
#' directly to [sf::gdal_utils].
#'
#' @return `output_raster`, invisibly.
#'
#' @family data manipulation functions
#'
#' @examples
#' \dontrun{
#' simulated_data <- data.frame(
#' lat = c(44.10379, 44.17573),
#' lng = c(-74.01177, -73.91171)
#' )
#'
#' simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
#'
#' img_files <- get_tiles(simulated_data)
#' merge_rasters(img_files[[1]])
#'
#' }
#'
#' @export
merge_rasters <- function(input_rasters,
                          output_raster = tempfile(fileext = ".tif"),
                          options = character(0)) {

  sf::gdal_utils(util = "warp",
                 source = as.character(input_rasters),
                 destination = output_raster,
                 options = options)

  return(invisible(output_raster))
}
