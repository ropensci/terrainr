#' Merge multiple raster files into a single raster
#'
#' @param input_heightmaps A character vector containing the file paths to the
#' elevation height maps to merge.
#' @param output_heightmap The file path to save the merged elevation raster to.
#' Must be a TIFF file.
#' @param input_orthos A character vector, the same length as \code{input_heightmaps},
#' containing the file paths to the orthoimages to merge. It's assumed that each
#' orthoimage in the vector corresponds to the height map in the same position
#' in input_heightmaps. If you don't want to merge any orthoimages, set to NULL.
#' @param output_ortho The file path to save the merged orthoimage to.
#' Must be a TIFF file. Set to NULL if you aren't merging orthoimages.
#' @param overwrite Logical: overwrite the output files if they exist?
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
#' @importFrom raster "crs<-" "extent<-"
#'
#' @export
merge_rasters <- function(input_heightmaps,
                          output_heightmap,
                          input_orthos = NULL,
                          output_ortho = NULL,
                          overwrite = TRUE) {

  if (!is.null(input_orthos)) {
    stopifnot(length(input_orthos) == length(input_heightmaps))
  }

  # writeRaster seems to write a .tif if a .tiff is specified, which means
  # mosaic_rasters does nothing and you get a useless blank .tif output unless
  # you run the function twice.
  # this is silly.
  # so, we'll work around it if necessary -- do our work in a .tif then rename
  # it at the end
  if (!grepl("\\.tif", output_heightmap) || !grepl("\\.tif", output_ortho)) {
    stop("Output files must be TIFFs.")
  }
  fix_height <- 0
  fix_ortho <- 0
  if (grepl("\\.tiff$", output_heightmap)) {
    fix_height <- 1
    output_heightmap <- substr(output_heightmap, 1, nchar(output_heightmap) - 1)
  }
  if (!is.null(input_orthos) && grepl("\\.tiff$", output_ortho)) {
    fix_ortho <- 1
    output_ortho <- substr(output_ortho, 1, nchar(output_ortho) - 1)
  }

  input_rasters <- lapply(input_heightmaps, function(x) raster::raster(x))

  if (!is.null(input_orthos)) {
    tmp_orthos <- vapply(seq_len(length(input_orthos)),
                         function(x) tempfile(fileext = ".tif"),
                         character(1))
    for (i in 1:length(input_orthos)) {
      current_ortho <- raster::brick(png::readPNG(input_orthos[[i]]))
      raster::crs(current_ortho) <- input_rasters[[i]]@crs
      raster::extent(current_ortho) <- input_rasters[[i]]@extent
      raster::writeRaster(current_ortho, tmp_orthos[[i]])
    }
  }

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
    output_heightmap,
    overwrite = overwrite
  ))
  invisible(
    utils::capture.output(
      gdalUtils::mosaic_rasters(
        gdalfile = input_heightmaps,
        dst_dataset = output_heightmap
        )
      )
    )

  if (!is.null(input_orthos)) {
    # same as above for elevation
    suppressWarnings(raster::writeRaster(total_extent,
                                         output_ortho,
                                         overwrite = overwrite
    ))
    invisible(
      utils::capture.output(
        gdalUtils::mosaic_rasters(
          gdalfile = tmp_orthos,
          dst_dataset = output_ortho
        )
      )
    )
  }

  if (fix_height) {
    file.rename(output_heightmap, paste0(output_heightmap, "f"))
  }
  if (fix_ortho) {
    file.rename(output_ortho, paste0(output_ortho, "f"))
  }

  return(NULL)
}
