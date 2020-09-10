#' Merge multiple raster files into a single raster
#'
#' @param input_files A character vector containing the file paths to the
#' rasters to merge.
#' @param output_file The file path to save the merged raster file to.
#' @param overwrite Logical: overwrite output_file if it exists?
#'
#' @return NULL
#'
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
  input_tifs <- lapply(input_files, function(x) raster::raster(x))
  total_extent <- raster::raster(raster::extent(min(sapply(input_tifs, function(x) raster::extent(x)@xmin)),
                                                max(sapply(input_tifs, function(x) raster::extent(x)@xmax)),
                                                min(sapply(input_tifs, function(x) raster::extent(x)@ymin)),
                                                max(sapply(input_tifs, function(x) raster::extent(x)@ymax))))
  raster::projection(total_extent) <- raster::projection(input_tifs[[1]])

  raster::writeRaster(total_extent, output_file, overwrite = overwrite)
  gdalUtils::mosaic_rasters(gdalfile = input_files, dst_dataset = output_file)

  if (fix_tiff) {
    file.rename(output_file, paste0(output_file, "f"))
  }

  return(NULL)

}
