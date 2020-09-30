#' Get USGS elevation and imagery tiles for an area
#'
#' This function splits the area contained within a bounding box into a set of
#' tiles, and retrieves USGS 3DEP heightmaps and NAIP orthoimagery for each tile.
#' Tiles are downloaded at a resolution of 1 meter per pixel.
#'
#' @param bbox A bounding box representing the lower left and upper right corner
#' of the area to retrieve a heightmap for. If not a
#' [terrainr::terrainr_bounding_box] object, it will be coerced to one.
#' @param output_prefix The file prefix to use when saving tiles.
#' @param side_length The length, in meters (and therefore pixels), of each side
#' of tiles to download.
#' @param extend Logical: if the side length of the bounding box isn't divisible
#' by \code{side_length}, should it be extended to be divisible?
#' @param elevation Logical: download elevation tiles for the
#' @param verbose Logical: should tile retrieval functions run in verbose mode?
#'
#' @return The original bounding box, invisibly.
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
#' get_tiles(bbox, tempfile(), 4096)
#' }
#'
#' @export
get_tiles <- function(bbox,
                      output_prefix,
                      side_length,
                      extend = FALSE,
                      elevation = TRUE,
                      ortho = FALSE,
                      verbose = FALSE) {
  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  if (ortho && side_length > 4096) {
    stop("Orthoimagery tiles have a maximum side length of 4097. ",
         "Set ortho to FALSE or change your maximum side length.")
  }
  if (elevation && side_length > 8000) {
    stop("Elevation tiles have a maximum side length of 8000.")
  }

  tl <- terrainr_coordinate_pair(c(bbox@tr@lat, bbox@bl@lng))
  img_width <- round(calc_haversine_distance(tl, bbox@tr), digits = 0)
  img_height <- round(calc_haversine_distance(tl, bbox@bl), digits = 0)

  x_tiles <- ceiling(img_width / side_length)
  y_tiles <- ceiling(img_height / side_length)

  tile_boxes <- lapply(
    vector("list", x_tiles),
    function(x) vector("list", y_tiles)
  )

  for (i in 1:x_tiles) {
    if (i == x_tiles) {
      left_lng <- point_from_distance(bbox@bl, side_length * (i - 1), 90)@lng
      right_lng <- bbox@tr@lng
    } else {
      left_lng <- point_from_distance(bbox@bl, side_length * (i - 1), 90)@lng
      right_lng <- point_from_distance(bbox@bl, side_length * i, 90)@lng
    }
    for (j in 1:y_tiles) {
      if (j == y_tiles) {
        top_lat <- point_from_distance(bbox@tr, side_length * (j - 1), 180)@lat
        bot_lat <- bbox@bl@lat
      } else {
        top_lat <- point_from_distance(bbox@tr, side_length * (j - 1), 180)@lat
        bot_lat <- point_from_distance(bbox@tr, side_length * j, 180)@lat
      }

      tile_boxes[[i]][[j]] <- list(
        bbox = terrainr_bounding_box(
          bl = terrainr_coordinate_pair(c(bot_lat, left_lng)),
          tr = terrainr_coordinate_pair(c(top_lat, right_lng))
        ),
        img_width = ifelse(((img_width - (i * side_length)) < 0),
                           img_width - ((i - 1) * side_length),
                           side_length
        ),
        img_height = ifelse((img_height - (j * side_length) < 0),
                            img_height - ((j - 1) * side_length),
                            side_length
        )
      )
    }
  }

  if (any(grepl("progressr", installed.packages()))) {
    p <- progressr::progressor(steps = x_tiles * y_tiles)
  }

  for (i in 1:x_tiles) {
    for (j in 1:y_tiles) {
      current_box <- tile_boxes[[i]][[j]]

      if (elevation) {
        if (any(grepl("progressr", installed.packages()))) {
          p(message = sprintf("Retriving elevation tile (%d,%d)", i, j))
        }
        img_bin <- hit_heightmap_api(current_box[["bbox"]],
                                     current_box[["img_width"]],
                                     current_box[["img_height"]],
                                     verbose = verbose
        )

        writeBin(img_bin, paste0(output_prefix, "_", i, "_", j, ".tiff"))
      }

      if (ortho) {
        if (any(grepl("progressr", installed.packages()))) {
          p(message = sprintf("Retriving orthoimage tile (%d,%d)", i, j))
        }
        img_bin <- hit_ortho_api(current_box[["bbox"]],
                                 overlay,
                                 current_box[["img_width"]],
                                 current_box[["img_height"]],
                                 verbose = verbose
        )
        writeBin(img_bin, paste0(output_prefix, "_", i, "_", j, ".png"))
      }

    }
  }

  return(invisible(bbox))

}
