#' Get USGS Digital Elevation Model heightmap for an area
#'
#' This function splits the area contained within a bounding box into a set of
#' tiles, retrieves USGS 3DEP heightmaps for each tile, and then merges those
#' tiles together into a final output raster.
#'
#' @param bbox A bounding box representing the lower left and upper right corner
#' of the area to retrieve a heightmap for. If not a
#' [terrainr::terrainr_bounding_box] object, it will be coerced to one.
#' @param output_file The file path to save the merged output raster to. Must
#' be a GeoTIFF file.
#' @param verbose Logical: should [terrainr::hit_heightmap_api] run in verbose
#' mode (printing out the number of tries it takes to retrieve each tile)?
#'
#' @examples
#' \dontrun{
#' simulated_data <- data.frame(
#'   id = seq(1, 100, 1),
#'   lat = runif(100, 44.04905, 44.17609),
#'   lng = runif(100, -74.01188, -73.83493)
#' )
#'
#' bbox <- get_coord_bbox(lat = simulated_data$lat, lng = simulated_data$lng)
#' bbox <- add_bbox_buffer(bbox, 100)
#' bbox <- make_unity_friendly(bbox)
#' temptiff <- tempfile(fileext = ".tif")
#' get_heightmap_tiles(temptiff)
#' raster_to_raw_tiles(temptiff, tempfile())
#' }
#'
#' @export
get_heightmap <- function(bbox,
                          output_file,
                          verbose = FALSE) {
  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  tl <- terrainr_coordinate_pair(c(bbox@tr@lat, bbox@bl@lng))
  img_width <- round(calc_haversine_distance(tl, bbox@tr), digits = 0)
  img_height <- round(calc_haversine_distance(tl, bbox@bl), digits = 0)

  x_tiles <- ceiling(img_width / 8000)
  y_tiles <- ceiling(img_height / 8000)

  tile_boxes <- lapply(vector("list", x_tiles),
                       function(x) vector("list", y_tiles))

  for (i in 1:x_tiles) {
    if (i == x_tiles) {
      left_lng <- point_from_distance(bbox@bl, 8000 * (i - 1), 90)@lng
      right_lng <- bbox@tr@lng
    } else {
      left_lng <- point_from_distance(bbox@bl, 8000 * (i - 1), 90)@lng
      right_lng <- point_from_distance(bbox@bl, 8000 * i, 90)@lng
    }
    for (j in 1:y_tiles) {
      if (j == y_tiles) {
        top_lat <- point_from_distance(bbox@tr, 8000 * (j - 1), 180)@lat
        bot_lat <- bbox@bl@lat
      } else {
        top_lat <- point_from_distance(bbox@tr, 8000 * (j - 1), 180)@lat
        bot_lat <- point_from_distance(bbox@tr, 8000 * j, 180)@lat
      }

      tile_boxes[[i]][[j]] <- list(
        bbox = terrainr_bounding_box(
          bl = terrainr_coordinate_pair(c(bot_lat, left_lng)),
          tr = terrainr_coordinate_pair(c(top_lat, right_lng))
        ),
        img_width = ifelse(((img_width - (i * 8000)) < 0),
          img_width - ((i - 1) * 8000),
          8000
        ),
        img_height = ifelse((img_height - (j * 8000) < 0),
          img_height - ((j - 1) * 8000),
          8000
        )
      )
    }
  }

  tempfiles <- character()
  while (length(tempfiles) != x_tiles * y_tiles) {
    tempfiles <- unique(vapply(
      vector(length = x_tiles * y_tiles),
      function(x) tempfile(fileext = ".tiff"),
      character(1)
    ))
  }

  file_counter <- 0
  for (i in 1:x_tiles) {
    for (j in 1:y_tiles) {
      file_counter <- file_counter + 1
      current_box <- tile_boxes[[i]][[j]]
      img_bin <- hit_heightmap_api(current_box[["bbox"]],
        current_box[["img_width"]],
        current_box[["img_height"]],
        verbose = verbose
      )

      invisible(writeBin(img_bin, tempfiles[[file_counter]]))
    }
  }

  invisible(merge_rasters(tempfiles, output_file))
  return(invisible(output_file))
}
