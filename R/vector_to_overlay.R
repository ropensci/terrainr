#' Turn spatial vector data into an image overlay
#'
#' This function allows users to quickly transform any vector data into an
#' image overlay, which may then be imported as a texture into Unity.
#'
#' @param vector_data The spatial vector data set to be transformed into an
#' overlay image. Users may provide either an `sf` object or a length 1
#' character vector containing a path to a file readable by [sf::read_sf].
#' @param reference_raster The raster file to produce an overlay for. The output
#' overlay will have the same extent and resolution as the input raster. Users
#' may provide either a Raster* object or a length 1 character
#' vector containing a path to a file readable by [raster::raster].
#' @param output_file The path to save the image overlay to. If `NULL`, saves to
#' a tempfile.
#' @param transparent The hex code for a color to be made transparent in the
#' final image. Set to `FALSE` to not set any colors to transparent.
#' @param ... Arguments passed to `...` in either [ggplot2::geom_point] (for
#' point vector data), [ggplot2::geom_line] (for line data),
#' or [ggplot2::geom_polygon] (for all other data types).
#' @param error_crs Logical: Should this function error if `data` has no CRS?
#' If `TRUE`, function errors; if `FALSE`, function quietly assumes EPSG:4326.
#' If `NULL`, the default, function assumes EPSG:4326 with a warning.
#'
#' @family data manipulation functions
#' @family overlay creation functions
#' @family visualization functions
#'
#' @return `output_file`, invisibly.
#'
#' @examples
#' \dontrun{
#'
#' # Generate points to download raster tiles for
#' set.seed(123)
#' simulated_data <- data.frame(
#'   id = seq(1, 100, 1),
#'   lat = runif(100, 44.1114, 44.1123),
#'   lng = runif(100, -73.92273, -73.92147)
#' )
#'
#' # Create an sf object from our original simulated data
#'
#' simulated_data_sf <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
#' sf::st_crs(simulated_data_sf) <- sf::st_crs(4326)
#'
#' # Download data!
#'
#' downloaded_tiles <- get_tiles(simulated_data_sf, tempfile())
#'
#' merged_file <- merge_rasters(
#'   downloaded_tiles[[1]],
#'   tempfile(fileext = ".tif")
#' )
#'
#'
#' # Create an overlay image
#' vector_to_overlay(simulated_data_sf, merged_file[[1]], na.rm = TRUE)
#' }
#'
#' @export
#' @md
vector_to_overlay <- function(vector_data,
                              reference_raster,
                              output_file = NULL,
                              transparent = "#ffffff",
                              ...,
                              error_crs = NULL) {
  if (is.character(vector_data) && length(vector_data) == 1) {
    vector_data <- sf::read_sf(vector_data)
  } else {
    stopifnot(any(grepl("^sf", class(vector_data))))
  }

  if (is.character(reference_raster) && length(reference_raster) == 1) {
    reference_raster <- raster::raster(reference_raster)
  } else {
    stopifnot(any(grepl("^Raster", class(reference_raster))))
  }

  if (is.na(sf::st_crs(vector_data))) {
    if (is.null(error_crs)) {
      warning(
        "No CRS associated with input vector data.\n",
        "Assuming it shares the CRS of reference_raster."
      )
    } else if (error_crs) {
      stop("No CRS associated with input vector data.")
    }
    sf::st_crs(vector_data) <- sf::st_crs(reference_raster)
  }
  if (!is.na(sf::st_crs(reference_raster))) {
    vector_data <- sf::st_transform(vector_data, sf::st_crs(reference_raster))
  }

  vector_data <- as.data.frame(sf::st_coordinates(vector_data))

  if (any(grepl("L", names(vector_data)))) {
    vector_data$grouping <- do.call(
      paste,
      lapply(
        grep("L", names(vector_data)),
        function(x) vector_data[[x]]
      )
    )
    if (any(grepl("L2", names(vector_data)))) {
      plot_fun <- ggplot2::geom_polygon
    } else {
      plot_fun <- ggplot2::geom_line
    }
  } else {
    vector_data$grouping <- 1 # placeholder so we can use the same ggplot
    plot_fun <- ggplot2::geom_point
  }

  # quiet R CMD check not appreciating ggplot's NSE...
  X <- Y <- NULL # nolint

  output_ggplot <- ggplot2::ggplot(
    vector_data,
    ggplot2::aes(x = X, y = Y, group = grouping)
  ) +
    plot_fun(...) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = c(
        reference_raster@extent@xmin,
        reference_raster@extent@xmax
      )
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      limits = c(
        reference_raster@extent@ymin,
        reference_raster@extent@ymax
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = NA,
        color = NA
      ),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0, "cm"),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
    )

  if (is.null(output_file)) output_file <- tempfile(fileext = ".png")

  ggplot2::ggsave(
    filename = output_file,
    plot = output_ggplot,
    width = reference_raster@ncols / 72,
    height = reference_raster@nrows / 72,
    units = "in",
    dpi = "screen",
    limitsize = FALSE
  )

  if (transparent != FALSE) {
    img <- magick::image_read(output_file)
    img <- magick::image_transparent(img, transparent)
    magick::image_write(img, output_file)
  }

  invisible(output_file)
}
