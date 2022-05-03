#' Plot RGB rasters in ggplot2
#'
#' `geom_spatial_rgb` and `stat_spatial_rgb` allow users to plot three-band RGB
#' rasters in [ggplot2], using these layers as background base maps for other
#' spatial plotting. Note that unlike [ggplot2::geom_sf], this function does
#' _not_ force [ggplot2::coord_sf]; for accurate mapping, add
#' [ggplot2::coord_sf] with a `crs` value matching your input raster as a layer.
#'
#' @rdname geom_spatial_rgb
#' @param data The data to be displayed in this layer. In addition to the three
#' options described in [ggplot2::geom_raster], there are two additional
#' methods:
#'
#' If a `SpatRaster` object (see [terra::rast]), this function will coerce
#' the raster to a data frame and assume the raster bands are in RGB order
#' (while allowing for, but ignoring, a fourth alpha band).
#'
#' If a length-1 character vector, this function will attempt to load the object
#' via [terra::rast].
#'
#' @inheritParams ggplot2::geom_raster
#' @param scale Integer. Maximum (possible) value in the three channels.
#' If `NULL`, attempts to infer proper values from data -- if all RGB values
#' are <= 1 then 1, <= 255 then 255, and otherwise 65535.
#'
#' @family visualization functions
#'
#' @examples
#' \dontrun{
#'
#' simulated_data <- data.frame(
#'   id = seq(1, 100, 1),
#'   lat = runif(100, 44.04905, 44.17609),
#'   lng = runif(100, -74.01188, -73.83493)
#' )
#'
#' simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
#' simulated_data <- sf::st_set_crs(simulated_data, 4326)
#'
#' output_tiles <- get_tiles(simulated_data,
#'   services = c("ortho"),
#'   resolution = 120
#' )
#'
#' merged_ortho <- tempfile(fileext = ".tif")
#' merge_rasters(output_tiles[["ortho"]], merged_ortho)
#'
#' merged_stack <- terra::rast(merged_ortho)
#'
#' library(ggplot2)
#'
#' ggplot() +
#'   geom_spatial_rgb(
#'     data = merged_ortho,
#'     mapping = aes(
#'       x = x,
#'       y = y,
#'       r = red,
#'       g = green,
#'       b = blue
#'     )
#'   ) +
#'   geom_sf(data = simulated_data) +
#'   coord_sf(crs = 4326)
#'
#' ggplot() +
#'   geom_spatial_rgb(
#'     data = merged_stack,
#'     mapping = aes(
#'       x = x,
#'       y = y,
#'       r = red,
#'       g = green,
#'       b = blue
#'     )
#'   ) +
#'   geom_sf(data = simulated_data) +
#'   coord_sf(crs = 4326)
#' }
#'
#' @export
geom_spatial_rgb <- function(mapping = NULL,
                             data = NULL,
                             stat = "spatialRGB",
                             position = "identity",
                             ...,
                             hjust = 0.5,
                             vjust = 0.5,
                             interpolate = FALSE,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             scale = NULL) {
  if (!(is.numeric(hjust) && length(hjust) == 1)) {
    stop("`hjust` must be a numeric scalar")
  }
  if (!(is.numeric(vjust) && length(vjust) == 1)) {
    stop("`vjust` must be a numeric scalar")
  }
  geom_spatial_rgb_internal(
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    ...,
    hjust = hjust,
    vjust = vjust,
    interpolate = interpolate,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    scale = scale
  )
}

geom_spatial_rgb_internal <- function(data = NULL,
                                      mapping = NULL,
                                      stat = "spatialRGB",
                                      position = "identity",
                                      ...,
                                      hjust = 0.5,
                                      vjust = 0.5,
                                      interpolate = FALSE,
                                      na.rm = FALSE,
                                      show.legend = NA,
                                      inherit.aes = TRUE,
                                      scale = NULL) {
  UseMethod("geom_spatial_rgb_internal")
}


geom_spatial_rgb_internal.character <- function(data = NULL,
                                                mapping = NULL,
                                                stat = "spatialRGB",
                                                position = "identity",
                                                ...,
                                                hjust = 0.5,
                                                vjust = 0.5,
                                                interpolate = FALSE,
                                                na.rm = FALSE,
                                                show.legend = NA,
                                                inherit.aes = TRUE,
                                                scale = NULL) {
  stopifnot(length(data) == 1)
  data <- terra::rast(data)
  geom_spatial_rgb_internal.SpatRaster(
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    scale = scale,
    ...
  )
}

geom_spatial_rgb_internal.RasterStack <- function(data = NULL,
                                                  mapping = NULL,
                                                  stat = "spatialRGB",
                                                  position = "identity",
                                                  ...,
                                                  hjust = 0.5,
                                                  vjust = 0.5,
                                                  interpolate = FALSE,
                                                  na.rm = FALSE,
                                                  show.legend = NA,
                                                  inherit.aes = TRUE,
                                                  scale = NULL) {
  data <- terra::rast(data)
  geom_spatial_rgb_internal(
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    scale = scale,
    ...
  )
}

geom_spatial_rgb_internal.SpatRaster <- function(data = NULL,
                                                 mapping = NULL,
                                                 stat = "spatialRGB",
                                                 position = "identity",
                                                 ...,
                                                 hjust = 0.5,
                                                 vjust = 0.5,
                                                 interpolate = FALSE,
                                                 na.rm = FALSE,
                                                 show.legend = NA,
                                                 inherit.aes = TRUE,
                                                 scale = NULL) {
  data <- terra::as.data.frame(data, xy = TRUE)
  if (terra::ncol(data) == 5) {
    data <- stats::setNames(data, c("x", "y", "red", "green", "blue"))
  } else if (terra::ncol(data) == 6) {
    data <- stats::setNames(data, c("x", "y", "red", "green", "blue", "alpha"))
  } else {
    stop("Can't assume band values from ",
         terra::ncol(data) - 2,
         " band raster.")
  }

  geom_spatial_rgb_internal(
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    scale = scale,
    ...
  )
}

geom_spatial_rgb_internal.default <- function(data = NULL,
                                              mapping = NULL,
                                              stat = "spatialRGB",
                                              position = "identity",
                                              ...,
                                              hjust = 0.5,
                                              vjust = 0.5,
                                              interpolate = FALSE,
                                              na.rm = FALSE,
                                              show.legend = NA,
                                              inherit.aes = TRUE,
                                              scale = NULL) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomRaster,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      hjust = hjust,
      vjust = vjust,
      interpolate = interpolate,
      na.rm = na.rm,
      scale = scale,
      ...
    )
  )
}

#' @export
#' @rdname geom_spatial_rgb
#' @usage NULL
#' @format NULL
StatSpatialRGB <- ggplot2::ggproto(
  "StatSpatialRGB",
  ggplot2::Stat,
  required_aes = c("x", "y", "r", "g", "b"),
  default_aes = ggplot2::aes(fill = ggplot2::stat(fill)),
  setup_params = function(data, params) {
    if (is.null(params$scale)) {
      if (all(data$r <= 1, data$g <= 1, data$b <= 1)) {
        params$scale <- 1
      } else if (all(data$r <= 256, data$g <= 256, data$b <= 256)) {
        params$scale <- 2^8 - 1
      } else {
        params$scale <- 2^16 - 1
      }
    }
    params
  },
  compute_group = function(data, scales, params, scale = NULL) {
    if (any(data$r < 0)) data[data$r < 0, ]$r <- 0
    if (any(data$g < 0)) data[data$g < 0, ]$g <- 0
    if (any(data$b < 0)) data[data$b < 0, ]$b <- 0
    data$fill <- grDevices::rgb(data$r / scale, data$g / scale, data$b / scale)
    data.frame(
      x = data$x,
      y = data$y,
      fill = data$fill
    )
  }
)

#' @export
#' @rdname geom_spatial_rgb
#' @inheritParams ggplot2::stat_identity
stat_spatial_rgb <- function(mapping = NULL,
                             data = NULL,
                             geom = "raster",
                             position = "identity",
                             na.rm = FALSE,
                             show.legend = FALSE,
                             inherit.aes = TRUE,
                             scale = NULL,
                             ...) {
  stat_spatial_rgb_internal(
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    scale = scale,
    ...
  )
}

stat_spatial_rgb_internal <- function(data = NULL,
                                      mapping = NULL,
                                      geom = "raster",
                                      position = "identity",
                                      na.rm = FALSE,
                                      show.legend = FALSE,
                                      inherit.aes = TRUE,
                                      scale = NULL,
                                      ...) {
  UseMethod("stat_spatial_rgb_internal")
}

stat_spatial_rgb_internal.character <- function(data = NULL,
                                                mapping = NULL,
                                                geom = "raster",
                                                position = "identity",
                                                na.rm = FALSE,
                                                show.legend = FALSE,
                                                inherit.aes = TRUE,
                                                scale = NULL,
                                                ...) {
  stopifnot(length(data) == 1)
  data <- terra::rast(data)
  stat_spatial_rgb_internal.SpatRaster(
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    scale = scale,
    ...
  )
}

stat_spatial_rgb_internal.RasterStack <- function(data = NULL,
                                                  mapping = NULL,
                                                  geom = "raster",
                                                  position = "identity",
                                                  na.rm = FALSE,
                                                  show.legend = FALSE,
                                                  inherit.aes = TRUE,
                                                  scale = NULL,
                                                  ...) {
  data <- terra::rast(data)
  stat_spatial_rgb_internal(
    data = data, mapping = mapping, geom = geom,
    position = position, na.rm = na.rm,
    show.legend = show.legend, inherit.aes = inherit.aes,
    scale = scale, ...
  )
}

stat_spatial_rgb_internal.SpatRaster <- function(data = NULL,
                                                 mapping = NULL,
                                                 geom = "raster",
                                                 position = "identity",
                                                 na.rm = FALSE,
                                                 show.legend = FALSE,
                                                 inherit.aes = TRUE,
                                                 scale = NULL,
                                                 ...) {
  data <- terra::as.data.frame(data, xy = TRUE)
  if (terra::ncol(data) == 5) {
    data <- stats::setNames(data, c("x", "y", "red", "green", "blue"))
  } else if (terra::ncol(data) == 6) {
    data <- stats::setNames(data, c("x", "y", "red", "green", "blue", "alpha"))
  } else {
    stop("Can't assume band values from ",
         terra::ncol(data) - 2,
         " band raster.")
  }

  stat_spatial_rgb_internal(
    data = data, mapping = mapping, geom = geom,
    position = position, na.rm = na.rm,
    show.legend = show.legend, inherit.aes = inherit.aes,
    scale = scale, ...
  )
}

stat_spatial_rgb_internal.default <- function(data = NULL,
                                              mapping = NULL,
                                              geom = "raster",
                                              position = "identity",
                                              na.rm = FALSE,
                                              show.legend = FALSE,
                                              inherit.aes = TRUE,
                                              scale = NULL,
                                              ...) {
  ggplot2::layer(
    stat = StatSpatialRGB,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      scale = scale,
      na.rm = na.rm, ...
    )
  )
}
