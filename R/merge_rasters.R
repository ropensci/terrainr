#' Merge multiple raster files into a single raster
#'
#' Some functions like \code{\link{get_tiles}} return multiple separate files
#' when it can be useful to have a single larger raster instead. This function
#' will collapse those multiple raster files into a single TIFF.
#'
#' Additionally, some outputs from \code{\link{get_tiles}} (such as when
#' \code{service = "ortho"}) are not georeferenced, making future processing
#' with them harder. This function can georeference those images using a
#' reference raster, then merge them into a single object for further
#' processing.
#'
#' @param input_rasters A character vector containing the file paths to the
#' georeferenced rasters you want to use.
#' @param output_raster The file path to save the merged georeferenced raster
#' to. Must be a TIFF file. Ignored if \code{merge_raster} is not \code{TRUE}.
#' @param input_images A character vector, the same length as
#' \code{input_rasters}, containing the file paths to non-georeferenced images
#' to merge. It's assumed that each image in the vector corresponds to the
#' raster in the same position in input_rasters, and these images will be
#' assigned the same extent and CRS. If you don't want to merge any images,
#' leave as \code{NULL}.
#' @param output_image The file path to save the merged images to.
#' Must be a TIFF file. Leave as \code{NULL} if you aren't merging images.
#' @param overwrite Logical: overwrite the output files if they exist?
#'
#' @return A named list containing the file paths outputs were written to.
#'
#' @family data manipulation functions
#'
#' @examples
#' \dontrun{
#' tiles <- terrainr_bounding_box(
#'   bl = c(lat = 44.10379, lng = -74.01177),
#'   tr = c(lat = 44.17573, lng = -73.91171)
#' )
#'
#' img_files <- get_tiles(tiles)
#' merge_rasters(img_files[[1]])
#' }
#'
#' @export
merge_rasters <- function(input_rasters,
                          output_raster = tempfile(fileext = ".tif"),
                          input_images = NULL,
                          output_image = tempfile(fileext = ".tif"),
                          overwrite = TRUE) {
  output_list <- vector("list")
  output_list$output_raster <- output_raster

  # writeRaster seems to write a .tif if a .tiff is specified, which means
  # mosaic_rasters does nothing and you get a useless blank .tif output unless
  # you run the function twice.
  # this is silly.
  # so, we'll work around it if necessary -- do our work in a .tif then rename
  # it at the end
  if ((!grepl("\\.tif", output_raster)) ||
    (!is.null(output_image) && !grepl("\\.tif?f$", output_image))) {
    stop("Output files must be TIFFs.")
  }

  fix_height <- 0
  fix_ortho <- 0

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

  if (!is.null(input_images)) {
    stopifnot(length(input_images) == length(input_rasters))
    stopifnot(!is.null(output_image))

    output_list$output_image <- output_image

    if (grepl("\\.tiff$", output_image)) {
      fix_ortho <- 1
      output_image <- substr(output_image, 1, nchar(output_image) - 1)
    }

    tmp_orthos <- vapply(
      seq_len(length(input_images)),
      function(x) tempfile(fileext = ".tif"),
      character(1)
    )
    mapply(function(img, out, rst) {
      terrainr::georeference_overlay(
        overlay_file = img,
        reference_raster = rst,
        output_file = out
      )
    },
    img = input_images,
    out = tmp_orthos,
    rst = input_raster_objects
    )
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
    overwrite = overwrite
  ))

  invisible(
    utils::capture.output(
      gdalUtils::mosaic_rasters(
        gdalfile = input_rasters,
        dst_dataset = output_raster
      )
    )
  )

  if (!is.null(input_images)) {
    # same as above for input_rasters
    suppressWarnings(
      raster::writeRaster(
        total_extent,
        output_image,
        overwrite = overwrite
      )
    )
    invisible(
      utils::capture.output(
        gdalUtils::mosaic_rasters(
          gdalfile = tmp_orthos,
          dst_dataset = output_image
        )
      )
    )
  }

  if (fix_height) {
    file.rename(output_raster, paste0(output_raster, "f"))
  }
  if (fix_ortho) {
    file.rename(output_image, paste0(output_image, "f"))
  }

  if (exists("tmprst")) lapply(tmprst, unlink)
  if (exists("tmp_orthos")) lapply(tmp_orthos, unlink)

  return(output_list)
}
