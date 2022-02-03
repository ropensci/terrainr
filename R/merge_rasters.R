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

  initial_file <- output_raster

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
    warning("Options are not respected when trying to merge rasters with differing numbers of bands") # nolint
  }

  output_list <- vector("list")
  output_list$output_raster <- output_raster

  # writeRaster seems to write a .tif if a .tiff is specified, which means
  # mosaic_rasters does nothing and you get a useless blank .tif output unless
  # you run the function twice.
  # this is silly.
  # so, we'll work around it if necessary -- do our work in a .tif then rename
  # it at the end
  if (!grepl("\\.tif", output_raster)) {
    stop("Output files must be TIFFs.")
  }

  fix_height <- 0

  if (grepl("\\.tiff$", output_raster)) {
    fix_height <- 1
    output_raster <- substr(output_raster, 1, nchar(output_raster) - 1)
  }

  input_raster_objects <- lapply(input_rasters, function(x) raster::raster(x))

  # if some files were downloaded as RGBA and some RGB, mosaic_rasters will fail
  #
  # so drop the alpha channel if we need to, but only if we need to, because
  # this takes a while
  if (any(vapply(
    input_raster_objects,
    function(x) x@file@nbands == 4,
    logical(1)
  )) &&
    !all(vapply(
      input_raster_objects,
      function(x) x@file@nbands == 4,
      logical(1)
    ))) {
    tmprst <- vapply(
      seq_along(input_rasters),
      function(x) tempfile(fileext = ".tif"),
      character(1)
    )
    mapply(
      function(x, y) {
        raster::writeRaster(
          raster::stack(x[[1]],
            bands = 1:3
          ),
          y
        )
      },
      input_rasters,
      tmprst
    )
    input_raster_objects <- lapply(tmprst, function(x) raster::raster(x))
    input_rasters <- tmprst
  }

  total_extent <- raster::raster(raster::extent(
    min(vapply(
      input_raster_objects,
      function(x) raster::extent(x)@xmin,
      numeric(1)
    )),
    max(vapply(
      input_raster_objects,
      function(x) raster::extent(x)@xmax,
      numeric(1)
    )),
    min(vapply(
      input_raster_objects,
      function(x) raster::extent(x)@ymin,
      numeric(1)
    )),
    max(vapply(
      input_raster_objects,
      function(x) raster::extent(x)@ymax,
      numeric(1)
    ))
  ))
  raster::projection(total_extent) <- raster::projection(input_raster_objects[[1]]) # nolint

  # we're writing an entirely NA raster to file
  # raster, like a good package should
  # attempts to warn us about this silly thing we're doing
  # but we're doing it on purpose, so suppress those warnings
  suppressWarnings(raster::writeRaster(total_extent,
    output_raster,
    overwrite = TRUE
  ))

  invisible(
    utils::capture.output(
      gdalUtils::mosaic_rasters(
        gdalfile = input_rasters,
        dst_dataset = output_raster
      )
    )
  )

  if (fix_height) {
    file.rename(output_raster, paste0(output_raster, "f"))
  }

  if (exists("tmprst")) lapply(tmprst, unlink)

  message(
    "...done.\n",
    "The alternate method seems to have worked!\n",
    "If your merged output looks right, you can ignore the above error.\n"
  )

  return(output_list)
}
