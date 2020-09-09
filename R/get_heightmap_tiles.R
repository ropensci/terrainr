# TODO: document
#' @export
get_heightmap_tiles <- function (bbox,
                                 output_file,
                                 img_width = NULL,
                                 img_height = NULL,
                                 verbose = FALSE)
{
  if (is.null(img_width)) {
    tl <- c("lat" = bbox[["tr"]][["lat"]],
            "lng" = bbox[["bl"]][["lng"]])
    img_width <- round(calc_haversine_distance(tl, bbox[["tr"]]), digits = 0)
  }
  if (is.null(img_height)) {
    tl <- c("lat" = bbox[["tr"]][["lat"]],
            "lng" = bbox[["bl"]][["lng"]])
    img_height <- round(calc_haversine_distance(tl, bbox[["bl"]]), digits = 0)
  }

  x_tiles <- ceiling(img_width / 8000)
  y_tiles <- ceiling(img_height / 8000)

  tile_boxes <- lapply(vector("list", x_tiles), function(x) vector("list", y_tiles))

  for (i in 1:x_tiles) {
    if (i == x_tiles) {
      left_lng <- point_from_distance(bbox[["bl"]], 8000 * (i - 1), 90)[["lng"]]
      right_lng <- bbox[["tr"]][["lng"]]
    } else {
      left_lng <- point_from_distance(bbox[["bl"]], 8000 * (i - 1), 90)[["lng"]]
      right_lng <- point_from_distance(bbox[["bl"]], 8000 * i, 90)[["lng"]]
    }
    for (j in 1:y_tiles) {
      if (j == y_tiles) {
        top_lat <- point_from_distance(bbox[["tr"]], 8000 * (j - 1), 180)[["lat"]]
        bot_lat <- bbox[["bl"]][["lat"]]
      } else {
        top_lat <- point_from_distance(bbox[["tr"]], 8000 * (j - 1), 180)[["lat"]]
        bot_lat <- point_from_distance(bbox[["tr"]], 8000 * j, 180)[["lat"]]
      }

      tile_boxes[[i]][[j]] <- list(bl = c(lat = bot_lat, lng = left_lng),
                                   tr = c(lat = top_lat, lng = right_lng),
                                   img_width = ifelse(((img_width - (i * 8000)) < 0),
                                                      img_width - ((i - 1) * 8000),
                                                      8000),
                                   img_height = ifelse((img_height - (j * 8000) < 0),
                                                       img_height - ((j - 1) * 8000),
                                                       8000))
    }
  }

  tempfiles <- character()
  while (length(tempfiles) != x_tiles * y_tiles) {
    tempfiles <- unique(vapply(vector(length = x_tiles * y_tiles),
                               function(x) tempfile(fileext = ".tiff"),
                               character(1)))
  }

  file_counter <- 0
  for (i in 1:x_tiles) {
    for (j in 1:y_tiles) {
      file_counter <- file_counter + 1
      current_box <- tile_boxes[[i]][[j]]
      img_bin <- hit_heightmap_api(current_box[1:2],
                         current_box[["img_width"]],
                         current_box[["img_height"]],
                         verbose = verbose)

      invisible(writeBin(img_bin, tempfiles[[file_counter]]))
    }
  }

  invisible(merge_rasters(tempfiles, output_file))
  return(output_file)

}
