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
#' @param elevation Logical: download elevation tiles for the specified area?
#' @param ortho Logical: download orthoimagery tiles for the specified area?
#' @param verbose Logical: should tile retrieval functions run in verbose mode?
#'
#' @return A list of length 2 with names `elevation` and `ortho` containing the
#' file paths elevation and orthoimagery files were saved to, respectively.
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
                      output_prefix = tempfile(),
                      side_length = NULL,
                      services = "elevation",
                      verbose = FALSE) {

  # short codes are assigned as names; we'll cast them into the full name later
  list_of_services <- c("elevation" = "3DEPElevation",
                        "ortho" = "USGSNAIPPlus")

  stopifnot(all(services %in% list_of_services |
                  services %in% names(list_of_services)))

  tif_files <- "3DEPElevation"
  png_files <- "USGSNAIPPlus"

  if (any(services %in% names(list_of_services))) { # cast short codes now
    replacements <- which(services %in% names(list_of_services))
    services[replacements] <- as.vector(list_of_services[services[replacements]])
  }

  services <- unique(services)

  if (!methods::is(bbox, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(bbox[[1]], bbox[[2]])
  }

  if (is.null(side_length)) {
    if ("USGSNAIPPlus" %in% services) side_length <- 4096 else side_length <- 8000
  }

  if (("USGSNAIPPlus" %in% services) && side_length > 4096) {
    stop("USGSNAIPPlus tiles have a maximum side length of 4097.")
  }
  if (("3DEPElevation" %in% services) && side_length > 8000) {
    stop("3DEPElevation tiles have a maximum side length of 8000.")
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
    p <- progressr::progressor(steps = x_tiles * y_tiles * length(services))
  }

  for (i in seq_len(x_tiles)) {
    for (j in seq_len(y_tiles)) {
      current_box <- tile_boxes[[i]][[j]]
      for (k in seq_along(services)) {
        if (any(grepl("progressr", installed.packages()))) {
          p(message = sprintf("Retriving %s tile (%d, %d)",
                              services[[k]],
                              i,
                              j)
            )
        }

        if (services[[k]] %in% tif_files) {
          fileext <- ".tiff"
        } else if (services[[k]] %in% png_files) {
          fileext <- ".png"
        }

        img_bin <- hit_national_map_api(current_box[["bbox"]],
                                        current_box[["img_width"]],
                                        current_box[["img_height"]],
                                        services[[k]],
                                        verbose = verbose
                                        )

        writeBin(img_bin, paste0(output_prefix,
                                 "_",
                                 services[[k]],
                                 "_",
                                 i,
                                 "_",
                                 j,
                                 fileext))
      }


    }
  }

  res <- vector("list")
  for (i in seq_along(services)) {
    if (services[[i]] %in% tif_files) {
      fileext <- ".tiff"
    } else if (services[[i]] %in% png_files) {
      fileext <- ".png"
    }
    res[[i]] <- paste0(output_prefix,
                       "_",
                       services[[i]],
                       "_",
                       outer(1:x_tiles, 1:y_tiles, paste, sep = "_"),
                       fileext)
    names(res)[[i]] <- services[[i]]
  }

  return(invisible(res))

}
