#' Merge multiple raster files into a single raster
#'
#' Some functions like [get_tiles] return multiple separate files
#' when it can be useful to have a single larger raster instead. This function
#' is a thin wrapper over [sf::gdal_utils], making it easy to
#' collapse those multiple raster files into a single TIFF.
#'
#' @param input_rasters A character vector containing the file paths to the
#' georeferenced rasters you want to use.
#' @param output_raster The file path to save the merged georeferenced raster
#' to.
#' @param options Optionally, a character vector of options to be passed
#' directly to [sf::gdal_utils]. If the fallback is used and any options (other
#' than "-overwrite") are specified, this will issue a warning.
#' @param overwrite Logical: overwrite `output_raster` if it exists? If FALSE
#' and the file exists, this function will fail with an error. The behavior if
#' this argument is TRUE and "-overwrite" is passed to `options` directly is
#' not stable.
#' @param force_fallback Logical: if TRUE, uses the much slower fallback method
#' by default. This is used for testing purposes and is not recommended for use
#' by end users.
#'
#' @return `output_raster`, invisibly.
#'
#' @family data manipulation functions
#'
#' @examples
#' \dontrun{
#' simulated_data <- data.frame(
#'   lat = c(44.10379, 44.17573),
#'   lng = c(-74.01177, -73.91171)
#' )
#'
#' simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
#'
#' img_files <- get_tiles(simulated_data)
#' merge_rasters(img_files[[1]])
#' }
#'
#' @export
merge_rasters <- function(input_rasters,
                          output_raster = tempfile(fileext = ".tif"),
                          options = character(0),
                          overwrite = FALSE,
                          force_fallback = FALSE) {
  if (file.exists(output_raster) &&
    !overwrite &&
    !any(options == "-overwrite")) {
    stop("File exists at ", output_raster, " and overwrite is not TRUE.")
  }

  if (!any(options == "-overwrite") && overwrite) {
    options <- c(options, "-overwrite")
  }

  if (!force_fallback) {
    tryCatch(
      {
        sf::gdal_utils(
          util = "warp",
          source = as.character(input_rasters),
          destination = output_raster,
          options = options
        )
      },
      error = function(e) {
        warning(
          "\nReceived error from gdalwarp.",
          "Trying another method. This may take longer than normal..."
        )
        merge_rasters_deprecated(input_rasters, output_raster, options)
      }
    )
  } else {
    options <- setdiff(options, "-overwrite")
    merge_rasters_deprecated(input_rasters, output_raster, options)
  }
  return(invisible(output_raster))
}

# This is the deprecated code that used to make up merge_rasters
# It's currently used in cases where tiles have differing numbers of bands
# All arguments are documented above
merge_rasters_deprecated <- function(input_rasters,
                                     output_raster = tempfile(fileext = ".tif"),
                                     options = character(0)) {
  if (length(options) > 0) {
    if(!(length(options == 1) && options == "-overwrite")) {
      warning("Options are not respected when trying to merge rasters with differing numbers of bands") # nolint
    }
  }

  temp_output <- tempfile(fileext = ".vrt")
  sf::gdal_utils(
    "buildvrt",
    input_rasters,
    temp_output
  )
  sf::gdal_utils(
    "warp",
    temp_output,
    output_raster,
    options = options
  )

  message(
    "\n...done.\n",
    "The alternate method seems to have worked!\n",
    "If your merged output looks right, you can ignore the above error.\n"
  )

  return(invisible(output_raster))
}
