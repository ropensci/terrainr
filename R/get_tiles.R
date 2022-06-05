#' A user-friendly way to get USGS National Map data tiles for an area
#'
#' This function splits the area contained within a bounding box into a set of
#' tiles, and retrieves data from the USGS National map for each tile. As of
#' version 0.5.0, the method for lists has been deprecated.
#'
#' @param data An `sf` or `SpatRast` object; tiles will be downloaded for the
#' full extent of the provided object.
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
#' @param projected Logical: is `data` in a projected coordinate reference
#' system? If `NULL`, the default, inferred from [sf::st_is_longlat].
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
#'
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
#' * [ecosystems](https://www.usgs.gov/centers/geosciences-and-environmental-change-science-center/science/global-ecosystems)
#'
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
#' @md
#' @export
get_tiles <- function(data,
                      output_prefix = tempfile(),
                      side_length = NULL,
                      resolution = 1,
                      services = "elevation",
                      verbose = FALSE,
                      georeference = TRUE,
                      projected = NULL,
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
                         projected = NULL,
                         ...) {
  dots <- list(...)

  if (is.null(projected)) {
    projected <- !sf::st_is_longlat(data)
    if (is.na(projected)) {
      rlang::warn(c(
        "Assuming geographic CRS.",
        i = "Set 'projected' to TRUE if projected."
      ))
      projected <- FALSE
    }
  }

  if (!any(names(dots) == "bboxSR")) {
    bboxSR <- handle_bboxSR(data, projected) # nolint
  } else {
    bboxSR <- dots[["bboxSR"]]
  } # nolint

  if (!any(names(dots) == "imageSR")) {
    imageSR <- bboxSR # nolint
  } else {
    imageSR <- dots[["imageSR"]]
  } # nolint

  data <- sf::st_bbox(data)

  get_tiles_internal(
    data,
    output_prefix = output_prefix,
    side_length = side_length,
    resolution = resolution,
    services = services,
    verbose = verbose,
    georeference = georeference,
    bboxSR = bboxSR,
    imageSR = imageSR,
    projected = projected,
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
                          projected = NULL,
                          ...) {
  data <- sf::st_as_sf(data)

  get_tiles(data,
    output_prefix = output_prefix,
    side_length = side_length,
    resolution = resolution,
    services = services,
    verbose = verbose,
    georeference = georeference,
    projected = projected,
    ...
  )
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
                             projected = NULL,
                             ...) {
  tmp <- tempfile(fileext = ".tiff")
  raster::writeRaster(data, tmp)
  data <- terra::rast(tmp)
  get_tiles.SpatRaster(data,
                       output_prefix = output_prefix,
                       side_length = side_length,
                       resolution = resolution,
                       services = services,
                       verbose = verbose,
                       georeference = georeference,
                       projected = projected,
                       ...)
}


#' @rdname get_tiles
#' @export
get_tiles.SpatRaster <- function(data,
                               output_prefix = tempfile(),
                               side_length = NULL,
                               resolution = 1,
                               services = "elevation",
                               verbose = FALSE,
                               georeference = TRUE,
                               projected = NULL,
                               ...) {
  dots <- list(...)
  if (is.null(projected)) {
    projected <- !sf::st_is_longlat(data)
    if (is.na(projected)) {
      rlang::warn(c(
        "Assuming geographic CRS.",
        i = "Set 'projected' to TRUE if projected."
      ))
      projected <- FALSE
    }
  }
  if (!any(names(dots) == "bboxSR")) {
    bboxSR <- handle_bboxSR(data, projected) # nolint
  } else {
    bboxSR <- dots[["bboxSR"]]
  } # nolint

  if (!any(names(dots) == "imageSR")) {
    imageSR <- bboxSR # nolint
  } else {
    imageSR <- dots[["imageSR"]]
  } # nolint

  data <- as.vector(terra::ext(data))
  data <- data.frame(
    lng = c(data[[1]], data[[2]]),
    lat = c(data[[3]], data[[4]])
  )
  data <- sf::st_as_sf(data, coords = c("lng", "lat"))
  data <- sf::st_bbox(data)

  get_tiles_internal(
    data,
    output_prefix = output_prefix,
    side_length = side_length,
    resolution = resolution,
    services = services,
    verbose = verbose,
    georeference = georeference,
    projected = projected,
    bboxSR = bboxSR,
    imageSR = imageSR,
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
                           projected = NULL,
                           ...) {
  .Deprecated(
    "get_tiles.list",
    "terrainr",
    msg = paste("'get_tiles.list' is deprecated as of terrainr 0.5.0.",
      "Convert your list to an sf object instead.",
      sep = "\n"
    )
  )
  projected <- FALSE

  bbox <- terrainr_bounding_box(data[[1]], data[[2]])
  bbox <- sf::st_as_sfc(terrainr_st_bbox(bbox))
  get_tiles(
    bbox,
    output_prefix = output_prefix,
    side_length = side_length,
    resolution = resolution,
    services = services,
    verbose = verbose,
    georeference = georeference,
    projected = projected,
    ...
  )
}

get_tiles_internal <- function(data,
                               output_prefix = tempfile(),
                               side_length = NULL,
                               resolution = 1,
                               services = "elevation",
                               verbose = FALSE,
                               georeference = TRUE,
                               projected = NULL,
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
    "watersheds" = "wbd",
    "ecosystems"
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

  if (is.null(side_length)) {
    if (any(services %in% png_files)) {
      side_length <- 4096
    } else {
      side_length <- 8000
    }
  }

  if (any(services %in% png_files) && side_length > 4096) {
    rlang::abort(c(
      "USGSNAIPPlus tiles have a maximum side length of 4096.",
      i = "Set `side_length` to 4096 or less"
    ))
  }
  if (("3DEPElevation" %in% services) && side_length > 8000) {
    rlang::abort(c(
      "3DEPElevation tiles have a maximum side length of 8000.",
      i = "Set `side_length` to 8000 or less"
    ))
  }

  bbox_splits <- split_bbox(data, side_length, resolution, projected)
  tile_boxes <- bbox_splits[[1]]
  x_tiles <- bbox_splits[[2]]
  y_tiles <- bbox_splits[[3]]

  if (requireNamespace("progressr", quietly = TRUE)) { # nocov start
    p <- progressr::progressor(steps = x_tiles * y_tiles * length(services))
  } # nocov end

  for (i in seq_len(x_tiles)) {
    for (j in seq_len(y_tiles)) {
      current_box <- tile_boxes[tile_boxes$x_tiles == i &
        tile_boxes$y_tiles == j, ]
      current_bbox <- data.frame(
        lat = c(current_box$min_y, current_box$max_y),
        lng = c(current_box$min_x, current_box$max_x)
      )
      current_bbox <- sf::st_as_sf(current_bbox, coords = c("lng", "lat"))
      current_bbox <- sf::st_bbox(current_bbox)
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
            current_bbox,
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
          cur_raster <- terra::rast(png::readPNG(cur_path))
          terra::crs(cur_raster) <- paste0(
            "+init=EPSG:",
            img_bin$extent$spatialReference$wkid
          )
          terra::ext(cur_raster) <- c(
            img_bin$extent$xmin,
            img_bin$extent$xmax,
            img_bin$extent$ymin,
            img_bin$extent$ymax
          )

          terra::writeRaster(cur_raster,
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

handle_bboxSR <- function(data, projected) {
  if (!is.na(sf::st_crs(data)$epsg)) {
    sf::st_crs(data)$epsg
  } else if (projected) {
    rlang::warn(c(
      "Assuming CRS of EPSG 5071",
      i = "Set bboxSR explicity to override"
      ))
    5071
  } else {
    rlang::warn(c(
      "Assuming CRS of EPSG 4326",
      i = "Set bboxSR explicity to override"
    ))
    4326
  }
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
#' @param projected Logical: is `data` in a projected coordinate reference
#' system? If `NULL`, the default, inferred from [sf::st_is_longlat].
#'
#' @keywords internal
#'
#' @return A data frame containing x and y tile indices, bounding boxes, and
#' side lengths.
#'
#' @noRd
split_bbox <- function(data, side_length, resolution = 1, projected) {
  if (!methods::is(data, "terrainr_bounding_box")) {
    bbox <- terrainr_bounding_box(
      c("lng" = data[["xmin"]], "lat" = data[["ymin"]]),
      c("lng" = data[["xmax"]], "lat" = data[["ymax"]])
    )
  } else {
    bbox <- data
  }
  tl <- terrainr_coordinate_pair(c(bbox@tr@lat, bbox@bl@lng))

  if (projected) {
    img_width <- round(
      (bbox@tr@lng - tl@lng) / resolution,
      digits = 0
    )
    img_height <- round(
      (tl@lat - bbox@bl@lat) / resolution,
      digits = 0
    )
  } else {
    img_width <- round(
      calc_haversine_distance(tl, bbox@tr) / resolution,
      digits = 0
    )
    img_height <- round(
      calc_haversine_distance(tl, bbox@bl) / resolution,
      digits = 0
    )
  }

  x_tiles <- ceiling(img_width / side_length)
  y_tiles <- ceiling(img_height / side_length)

  tile_boxes <- expand.grid(
    x_tiles = 1:x_tiles,
    y_tiles = 1:y_tiles,
    min_x = NA,
    max_x = NA,
    min_y = NA,
    max_y = NA,
    img_width = NA,
    img_height = NA
  )

  if (projected) {
    for (i in 1:x_tiles) {
      tile_boxes[tile_boxes$x_tiles == i, ]$min_x <-
        bbox@bl@lng + (side_length * (i - 1) * resolution)
      if (i == x_tiles) {
        tile_boxes[tile_boxes$x_tiles == i, ]$max_x <- bbox@tr@lng
      } else {
        tile_boxes[tile_boxes$x_tiles == i, ]$max_x <-
          bbox@bl@lng + (side_length * i * resolution)
      }
    }
    for (j in 1:y_tiles) {
      tile_boxes[tile_boxes$y_tiles == j, ]$max_y <-
        bbox@tr@lat - (side_length * (j - 1) * resolution)
      if (j == y_tiles) {
        tile_boxes[tile_boxes$y_tiles == j, ]$min_y <- bbox@bl@lat
      } else {
        tile_boxes[tile_boxes$y_tiles == j, ]$min_y <-
          bbox@tr@lat - (side_length * j * resolution)
      }
    }
  } else {
    for (i in 1:x_tiles) {
      tile_boxes[tile_boxes$x_tiles == i, ]$min_x <- point_from_distance(
        bbox@bl,
        side_length * (i - 1) * resolution,
        90
      )@lng
      if (i == x_tiles) {
        tile_boxes[tile_boxes$x_tiles == i, ]$max_x <- bbox@tr@lng
      } else {
        tile_boxes[tile_boxes$x_tiles == i, ]$max_x <- point_from_distance(
          bbox@bl,
          side_length * i * resolution,
          90
        )@lng
      }
    }
    for (j in 1:y_tiles) {
      tile_boxes[tile_boxes$y_tiles == j, ]$max_y <- point_from_distance(
        bbox@tr,
        side_length * (j - 1) * resolution,
        180
      )@lat
      if (j == y_tiles) {
        tile_boxes[tile_boxes$y_tiles == j, ]$min_y <- bbox@bl@lat
      } else {
        tile_boxes[tile_boxes$y_tiles == j, ]$min_y <- point_from_distance(
          bbox@tr,
          side_length * j * resolution,
          180
        )@lat
      }
    }
  }

  for (i in seq_len(nrow(tile_boxes))) {
    x_idx <- tile_boxes[i, ]$x_tiles
    y_idx <- tile_boxes[i, ]$y_tiles
    tile_boxes[i, ]$img_width <- ifelse(
      (img_width - (x_idx * side_length)) < 0,
      img_width - ((x_idx - 1) * side_length),
      side_length
    )
    tile_boxes[i, ]$img_height <- ifelse(
      img_height - (y_idx * side_length) < 0,
      img_height - ((y_idx - 1) * side_length),
      side_length
    )
  }

  return(list(
    tile_boxes,
    x_tiles,
    y_tiles
  ))
}
