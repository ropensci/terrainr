#' A user-friendly way to get USGS National Map data tiles for an area
#'
#' This function splits the area contained within a bounding box into a set of
#' tiles, and retrieves data from the USGS National map for each tile.
#'
#' @param data An `sf` or `Raster` object; tiles will be downloaded for the full
#' extent of the provided object.
#' @param output_prefix The file prefix to use when saving tiles.
#' @param side_length The length, in meters, of each side of tiles to download.
#' If \code{NULL}, defaults to the maximum side length permitted by the least
#' permissive service requested.
#' @param resolution How many meters are represented by each pixel? The default
#' value of 1 means that 1 pixel = 1 meter, while a value of 2 means that
#' 1 pixel = 2 meters, and so on.
#' @param services A character vector of services to download data from. Current
#' options include "3DEPElevation", "USGSNAIPPlus", and "nhd". Users can also
#' use short codes to download a specific type of data without specifying the
#' source; current options for short codes include "elevation" (equivalent to
#' "3DEPElevation"), "ortho" (equivalent to "USGSNAIPPlus), and "hydro" ("nhd").
#' Short codes are
#' not guaranteed to refer to the same source across releases. Short codes are
#' converted to their service name and then duplicates are removed, so any given
#' source will only be queried once per tile.
#' @param verbose Logical: should tile retrieval functions run in verbose mode?
#' @param georeference Logical: should tiles be downloaded as PNGs without
#' georeferencing, or should they be downloaded as georeferenced TIFF files?
#' This option does nothing when only elevation data is being downloaded.
#' @param ... Additional arguments passed to [hit_national_map_api].
#' These can be used to change default query parameters or as additional options
#' for the National Map services. See below for more details.
#'
#' @section Available Datasources:
#' The following services are currently available
#' (with short codes in parentheses where applicable). See links for API
#' documentation.
#'
# nolint start
#' * [3DEPElevation](https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer)
#'   (short code: elevation)
#' * [USGSNAIPPlus](https://services.nationalmap.gov/arcgis/rest/services/USGSNAIPPlus/MapServer)
#'   (short code: ortho)
#' * [nhd](https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer)
#'   (short code: hydro)
#' * [govunits](https://carto.nationalmap.gov/arcgis/rest/services/govunits/MapServer)
#' * [contours](https://carto.nationalmap.gov/arcgis/rest/services/contours/MapServer)
#' * [geonames](https://carto.nationalmap.gov/arcgis/rest/services/geonames/MapServer)
#' * [NHDPlus_HR](https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer)
#' * [structures](https://carto.nationalmap.gov/arcgis/rest/services/structures/MapServer)
#' * [transportation](https://carto.nationalmap.gov/arcgis/rest/services/transportation/MapServer)
#' * [wbd](https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer)
#'   ("short code": watersheds)
# nolint end
#'
#' @section Additional Arguments:
#' The `...` argument can be used to pass additional arguments to the
#' National Map API or to edit the hard-coded defaults used by this function.
#' More information on common arguments to change can be found in
#' [hit_national_map_api]. Note that `...` can also be used to change
#' the formats returned by the server, but that doing so while using this
#' function will likely cause the function to error (or corrupt the output
#' data). To download files in different formats, use [hit_national_map_api].
#'
#' @family data retrieval functions
#'
#' @return A list of the same length as the number of unique services requested,
#' containing named vectors of where data files were saved to. Returned
#' invisibly.
#'
#' @examples
#' \dontrun{
#' simulated_data <- data.frame(
#'   id = seq(1, 100, 1),
#'   lat = runif(100, 44.04905, 44.17609),
#'   lng = runif(100, -74.01188, -73.83493)
#' )
#'
#' simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
#'
#' get_tiles(simulated_data, tempfile())
#' }
#'
#' @rdname get_tiles
#' @export
get_tiles <- function(data,
                      output_prefix = tempfile(),
                      side_length = NULL,
                      resolution = 1,
                      services = "elevation",
                      verbose = FALSE,
                      georeference = TRUE,
                      ...) {
  UseMethod("get_tiles")
}

#' @rdname get_tiles
#' @export
get_tiles.sf <- function(data,
                         output_prefix = tempfile(),
                         side_length = NULL,
                         resolution = 1,
                         services = "elevation",
                         verbose = FALSE,
                         georeference = TRUE,
                         ...) {

  data <- sf::st_bbox(data)
  bl <- c("lng" = data[["xmin"]], "lat" = data[["ymin"]])
  tr <- c("lng" = data[["xmax"]], "lat" = data[["ymax"]])

  get_tiles_internal(
    bl = bl,
    tr = tr,
    output_prefix = output_prefix,
    side_length = side_length,
    resolution = resolution,
    services = services,
    verbose = verbose,
    georeference = georeference,
    ...
  )

}

#' @rdname get_tiles
#' @export
get_tiles.sfc <- function(data,
                          output_prefix = tempfile(),
                          side_length = NULL,
                          resolution = 1,
                          services = "elevation",
                          verbose = FALSE,
                          georeference = TRUE,
                          ...) {

  data <- sf::st_as_sf(data)

  get_tiles(data,
           output_prefix = output_prefix,
           side_length = side_length,
           resolution = resolution,
           services = services,
           verbose = verbose,
           georeference = georeference,
           ...)

}

#' @rdname get_tiles
#' @export
get_tiles.Raster <- function(data,
                             output_prefix = tempfile(),
                             side_length = NULL,
                             resolution = 1,
                             services = "elevation",
                             verbose = FALSE,
                             georeference = TRUE,
                             ...) {

  data <- raster::extent(data)
  bl <- c("lng" = data@xmin, "lat" = data@ymin)
  tr <- c("lng" = data@xmax, "lat" = data@ymax)

  get_tiles_internal(
    bl = bl,
    tr = tr,
    output_prefix = output_prefix,
    side_length = side_length,
    resolution = resolution,
    services = services,
    verbose = verbose,
    georeference = georeference,
    ...
  )

}

#' @rdname get_tiles
#' @export
get_tiles.list <- function(data,
                           output_prefix = tempfile(),
                           side_length = NULL,
                           resolution = 1,
                           services = "elevation",
                           verbose = FALSE,
                           georeference = TRUE,
                           ...) {

  bbox <- terrainr_bounding_box(data[[1]], data[[2]])
  get_tiles_internal(
    bl = bbox@bl,
    tr = bbox@tr,
    output_prefix = output_prefix,
    side_length = side_length,
    resolution = resolution,
    services = services,
    verbose = verbose,
    georeference = georeference,
    ...
  )

}

#' @rdname get_tiles
# nolint start
get_tiles.terrainr_bounding_box <- function(data,
                                            output_prefix = tempfile(),
                                            side_length = NULL,
                                            resolution = 1,
                                            services = "elevation",
                                            verbose = FALSE,
                                            georeference = TRUE,
                                            ...) {
# nolint end
  get_tiles_internal(
    bl = data@bl,
    tr = data@tr,
    output_prefix = output_prefix,
    side_length = side_length,
    resolution = resolution,
    services = services,
    verbose = verbose,
    georeference = georeference,
    ...
  )

}

get_tiles_internal <- function(bl,
                               tr,
                               output_prefix = tempfile(),
                               side_length = NULL,
                               resolution = 1,
                               services = "elevation",
                               verbose = FALSE,
                               georeference = TRUE,
                               ...) {

  # short codes are assigned as names; we'll cast them into the full name later
  # full names are from the API URL, hence capitalization woes
  list_of_services <- c(
    "elevation" = "3DEPElevation",
    "ortho" = "USGSNAIPPlus",
    "hydro" = "nhd",
    "govunits",
    "contours",
    "geonames",
    "NHDPlus_HR",
    "structures",
    "transportation",
    "watersheds" = "wbd"
  )

  stopifnot(all(services %in% list_of_services |
                  services %in% names(list_of_services)))

  tif_files <- c("3DEPElevation")
  png_files <- list_of_services[!(list_of_services %in% tif_files)]

  services <- stats::setNames(services, services)

  if (any(services %in% names(list_of_services))) { # cast short codes now
    replacements <- which(services %in% names(list_of_services))
    services[replacements] <- as.vector(
      list_of_services[services[replacements]]
    )
  }

  # duplicated instead of unique to preserve names
  services <- services[!duplicated(services)]

  bbox <- terrainr_bounding_box(bl, tr)

  if (is.null(side_length)) {
    if (any(services %in% png_files)) {
      side_length <- 4096
    } else {
      side_length <- 8000
    }
  }

  if (any(services %in% png_files) && side_length > 4096) {
    stop("USGSNAIPPlus tiles have a maximum side length of 4096.")
  }
  if (("3DEPElevation" %in% services) && side_length > 8000) {
    stop("3DEPElevation tiles have a maximum side length of 8000.")
  }

  bbox_splits <- split_bbox(bbox, side_length, resolution)
  tile_boxes <- bbox_splits[[1]]
  x_tiles <- bbox_splits[[2]]
  y_tiles <- bbox_splits[[3]]

  if (requireNamespace("progressr", quietly = TRUE)) { # nocov start
    p <- progressr::progressor(steps = x_tiles * y_tiles * length(services))
  } # nocov end

  for (i in seq_len(x_tiles)) {
    for (j in seq_len(y_tiles)) {
      current_box <- tile_boxes[[i]][[j]]
      for (k in seq_along(services)) {
        if (requireNamespace("progressr", quietly = TRUE)) { # nocov start
          p(message = sprintf(
            "Retrieving %s tile (%d, %d)",
            services[[k]],
            i,
            j
          ))
        } # nocov end

        if (services[[k]] %in% tif_files | georeference) {
          fileext <- ".tif"
        } else if (services[[k]] %in% png_files) {
          fileext <- ".png"
        }

        final_path <- paste0(
          output_prefix,
          "_",
          services[[k]],
          "_",
          i,
          "_",
          j,
          fileext
        )

        if (georeference && services[[k]] != "3DEPElevation") {
          rm_path <- TRUE
          cur_path <- tempfile(fileext = ".png")
        } else {
          rm_path <- FALSE
          cur_path <- final_path
        }

        counter <- 0
        while ((!file.exists(cur_path) ||
                file.size(cur_path) == 0) &&
               counter < 5) {
          img_bin <- hit_national_map_api(
            current_box[["bbox"]],
            current_box[["img_width"]],
            current_box[["img_height"]],
            services[[k]],
            verbose = verbose,
            ...
          )
          if (is.raw(img_bin$imageData)) {
            writeBin(img_bin$imageData, cur_path)
          } else {
            outconn <- file(cur_path, "wb")
            base64enc::base64decode(
              what = img_bin$imageData,
              output = outconn
            )
            close(outconn)
          }
        }

        if (georeference && services[[k]] != "3DEPElevation") {
          cur_raster <- raster::brick(png::readPNG(cur_path))
          cur_raster@crs <- raster::crs(paste0(
            "+init=EPSG:",
            img_bin$extent$spatialReference$wkid
          ))
          cur_raster@extent <- raster::extent(
            img_bin$extent$xmin,
            img_bin$extent$xmax,
            img_bin$extent$ymin,
            img_bin$extent$ymax
          )

          raster::writeRaster(cur_raster,
                              final_path,
                              overwrite = TRUE
          )
          if (rm_path) unlink(cur_path)
        }
      }
    }
  }

  res <- vector("list")
  for (i in seq_along(services)) {
    if (services[[i]] %in% tif_files || georeference) {
      fileext <- ".tif"
    } else if (services[[i]] %in% png_files) {
      fileext <- ".png"
    }
    res[[i]] <- paste0(
      output_prefix,
      "_",
      services[[i]],
      "_",
      outer(1:x_tiles, 1:y_tiles, paste, sep = "_"),
      fileext
    )
    names(res)[[i]] <- names(services)[[i]]
  }

  return(invisible(res))

}

#' Split a bounding box into a smaller set of component tiles.
#'
#' This function splits the area contained within a bounding box into a set of
#' tiles.
#'
#' @param bbox The bounding box to split into tiles.
#' @param side_length The length of each side of the output tiles.
#' @param resolution How many meters are represented by each pixel? The default
#' value of 1 means that 1 pixel = 1 meter, while a value of 2 means that
#' 1 pixel = 2 meters, and so on.
#'
#' @keywords internal
#'
#' @return A list containing the split tiles in position 1, the number of tiles
#' in the x direction in position 2, and the number of tiles in the y direction
#' in position 3.
#'
#' @noRd
split_bbox <- function(bbox, side_length, resolution = 1) {
  tl <- terrainr_coordinate_pair(c(bbox@tr@lat, bbox@bl@lng))

  img_width <- round(
    calc_haversine_distance(tl, bbox@tr) / resolution,
    digits = 0
  )
  img_height <- round(
    calc_haversine_distance(tl, bbox@bl) / resolution,
    digits = 0
  )

  x_tiles <- ceiling(img_width / side_length)
  y_tiles <- ceiling(img_height / side_length)

  tile_boxes <- lapply(
    vector("list", x_tiles),
    function(x) vector("list", y_tiles)
  )

  for (i in 1:x_tiles) {
    if (i == x_tiles) {
      left_lng <- point_from_distance(
        bbox@bl,
        side_length * (i - 1) * resolution,
        90
      )@lng
      right_lng <- bbox@tr@lng
    } else {
      left_lng <- point_from_distance(
        bbox@bl,
        side_length * (i - 1) * resolution,
        90
      )@lng
      right_lng <- point_from_distance(
        bbox@bl,
        side_length * i * resolution,
        90
      )@lng
    }
    for (j in 1:y_tiles) {
      if (j == y_tiles) {
        top_lat <- point_from_distance(
          bbox@tr,
          side_length * (j - 1) * resolution,
          180
        )@lat
        bot_lat <- bbox@bl@lat
      } else {
        top_lat <- point_from_distance(
          bbox@tr,
          side_length * (j - 1) * resolution,
          180
        )@lat
        bot_lat <- point_from_distance(
          bbox@tr,
          side_length * j * resolution,
          180
        )@lat
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

  return(list(
    tile_boxes,
    x_tiles,
    y_tiles
  ))
}
