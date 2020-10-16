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
#' @param merge_raster Logical: Should the files provided to input_rasters be
#' merged? Set to \code{FALSE} if you're only providing these input tiles to
#' georeference tiles in \code{input_images}.
#'
#' @return A named list containing the file paths outputs were written to.
#'
#' @family data manipulation functions
#'
#' @examples
#' \dontrun{
#' tile_1 <- terrainr_bounding_box(
#'   bl = c(lat = 44.10379, lng = -74.01177),
#'   tr = c(lat = 44.17573, lng = -73.91171)
#' )
#'
#' tile_2 <- terrainr_bounding_box(
#'   bl = c(lat = 44.03184, lng = -74.01177),
#'   tr = c(lat = 44.10379, lng = -73.91171)
#' )
#' raster_files <- c(tempfile(), tempfile())
#' img_bin <- lapply(
#'   c(tile_1, tile_2),
#'   function(x) hit_national_map_api(x, 8000, 8000, "3DEPElevation")
#' )
#' writeBin(img_bin[[1]], raster_files[[1]])
#' writeBin(img_bin[[2]], raster_files[[2]])
#' merge_rasters(raster_files, tempfile())
#' }
#'
#' @export
merge_rasters <- function(input_rasters,
                          output_raster = NULL,
                          input_images = NULL,
                          output_image = NULL,
                          overwrite = TRUE,
                          merge_raster = TRUE) {
  output_list <- vector("list")
  if (!is.null(output_raster)) output_list$output_raster <- output_raster
  if (!is.null(output_image)) output_list$output_image <- output_image

  if (!is.null(input_images)) {
    stopifnot(length(input_images) == length(input_rasters))
    stopifnot(!is.null(output_image))
  }

  # rather than fence all our code with merge_raster, just fake an output file
  if (!merge_raster) output_raster <- tempfile(fileext = ".tif")

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
  if (!is.null(input_images) && grepl("\\.tiff$", output_image)) {
    fix_ortho <- 1
    output_image <- substr(output_image, 1, nchar(output_image) - 1)
  }

  input_raster_objects <- lapply(input_rasters, function(x) raster::raster(x))

  if (!is.null(input_images)) {
    tmp_orthos <- vapply(
      seq_len(length(input_images)),
      function(x) tempfile(fileext = ".tif"),
      character(1)
    )
    for (i in seq_len(length(input_images))) {
      current_ortho <- raster::brick(png::readPNG(input_images[[i]]))
      raster::crs(current_ortho) <- input_raster_objects[[i]]@crs
      raster::extent(current_ortho) <- input_raster_objects[[i]]@extent
      raster::writeRaster(current_ortho, tmp_orthos[[i]])
    }
  }

  total_extent <- raster::raster(raster::extent(
    min(vapply(input_raster_objects,
               function(x) raster::extent(x)@xmin,
               numeric(1))),
    max(vapply(input_raster_objects,
               function(x) raster::extent(x)@xmax,
               numeric(1))),
    min(vapply(input_raster_objects,
               function(x) raster::extent(x)@ymin,
               numeric(1))),
    max(vapply(input_raster_objects,
               function(x) raster::extent(x)@ymax,
               numeric(1)))
  ))
  raster::projection(total_extent) <- raster::projection(input_raster_objects[[1]]) # nolint

  if (merge_raster) {
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
  }

  if (!is.null(input_images)) {
    # same as above for input_rasters
    suppressWarnings(raster::writeRaster(total_extent,
      output_image,
      overwrite = overwrite
    ))
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

  return(output_list)
}
