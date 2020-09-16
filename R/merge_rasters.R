#' Merge multiple raster files into a single raster
#'
#' @param input_files A character vector containing the file paths to the
#' rasters to merge.
#' @param output_file The file path to save the merged raster file to.
#' @param overwrite Logical: overwrite output_file if it exists?
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' tile_1 <- terrainr_bounding_box(
#'   bl = c(44.10379, -74.01177),
#'   tr = c(44.17573, -73.91171)
#' )
#'
#' tile_2 <- terrainr_bounding_box(
#'   bl = c(44.03184, -74.01177),
#'   tr = c(44.10379, -73.91171)
#' )
#' raster_files <- c(tempfile(), tempfile())
#' img_bin <- lapply(
#'   c(tile_1, tile_2),
#'   function(x) hit_heightmap_api(x, 8000, 8000)
#' )
#' writeBin(img_bin[[1]], raster_files[[1]])
#' writeBin(img_bin[[2]], raster_files[[2]])
#' merge_rasters(raster_files, tempfile())
#' }
#'
#' @export
merge_rasters <- function(input_files, output_file, overwrite = TRUE) {

  # writeRaster seems to write a .tif if a .tiff is specified, which means
  # mosaic_rasters does nothing and you get a useless blank .tif output unless
  # you run the function twice.
  # this is silly.
  # so, we'll work around it if necessary -- do our work in a .tif then rename
  # it at the end
  if (grepl("\\.tif$", output_file)) {
    fix_tiff <- 0
  } else if (grepl("\\.tiff$", output_file)) {
    fix_tiff <- 1
    output_file <- substr(output_file, 1, nchar(output_file) - 1)
  } else {
    stop("Output file must have a .tif extension")
  }
  input_rasters <- lapply(input_files, function(x) raster::raster(x))
  total_extent <- raster::raster(raster::extent(
    min(sapply(input_rasters, function(x) raster::extent(x)@xmin)),
    max(sapply(input_rasters, function(x) raster::extent(x)@xmax)),
    min(sapply(input_rasters, function(x) raster::extent(x)@ymin)),
    max(sapply(input_rasters, function(x) raster::extent(x)@ymax))
  ))
  raster::projection(total_extent) <- raster::projection(input_rasters[[1]])

  # we're writing an entirely NA raster to file
  # raster, like a good package should
  # attempts to warn us about this silly thing we're doing
  # but we're doing it on purpose, so suppress those warnings
  suppressWarnings(raster::writeRaster(total_extent,
    output_file,
    overwrite = overwrite
  ))
  invisible(
    utils::capture.output(
      gdalUtils::mosaic_rasters(
        gdalfile = input_files,
        dst_dataset = output_file
        )
      )
    )

  if (fix_tiff) {
    file.rename(output_file, paste0(output_file, "f"))
  }

  return(NULL)
}
