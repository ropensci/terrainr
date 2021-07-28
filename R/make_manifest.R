#' Transform rasters and write manifest file for import into Unity
#'
#' This function crops input raster files into smaller square tiles and then
#' converts them into either .png or .raw files which are ready to be imported
#' into the Unity game engine. It also writes a "manifest" file and importer
#' script which may be used to automatically import the tiles into Unity.
#'
#' @param heightmap File path to the heightmap to transform.
#' @param overlay Optionally, file path to the image overlay to transform.
#' @param output_prefix The file path to prefix output tiles with.
#' @param manifest_path File path to write the manifest file to.
#' @param importer_path File name to write the importer script to. Set to NULL
#' to not copy the importer script. Will overwrite any file at the same path.
#'
#' @return `manifest_path`, invisibly.
#'
#' @examples
#' \dontrun{
#' if (!isTRUE(as.logical(Sys.getenv("CI")))) {
#'   simulated_data <- data.frame(
#'     id = seq(1, 100, 1),
#'     lat = runif(100, 44.04905, 44.17609),
#'     lng = runif(100, -74.01188, -73.83493)
#'   )
#'   simulated_data <- sf::st_as_sf(simulated_data, coords = c("lng", "lat"))
#'   output_files <- get_tiles(simulated_data)
#'   temptiff <- tempfile(fileext = ".tif")
#'   merge_rasters(output_files["elevation"][[1]], temptiff)
#'   make_manifest(temptiff, output_prefix = tempfile(), importer_path = NULL)
#' }
#' }
#'
#' @export
make_manifest <- function(heightmap,
                          overlay = NULL,
                          output_prefix = "import",
                          manifest_path = "terrainr.manifest",
                          importer_path = "import_terrain.cs") {
  input_raster <- raster::raster(heightmap)
  max_raster <- raster::cellStats(input_raster, "max")

  x_tiles <- ceiling(input_raster@ncols / 4097)
  y_tiles <- ceiling(input_raster@nrows / 4097)
  n_tiles <- x_tiles * y_tiles

  temptiffs <- NULL
  while (length(temptiffs) != n_tiles) {
    temptiffs <- unique(vapply(
      1:(n_tiles),
      function(x) tempfile(fileext = ".tiff"),
      character(1)
    ))
  }

  file_combos <- expand.grid(
    x = 1:x_tiles,
    y = 1:y_tiles
  )
  file_names <- mapply(
    function(x, y) {
      paste0(
        output_prefix,
        "_",
        x,
        "_",
        y
      )
    },
    file_combos$x,
    file_combos$y
  )

  x_tiles <- 0:(x_tiles - 1)
  x_tiles <- (x_tiles * 4097)

  y_tiles <- 0:(y_tiles - 1)
  y_tiles <- (y_tiles * 4097)

  manifest <- data.frame(
    filename = paste0(sort(file_names), ".raw"),
    x_pos = -rep(x_tiles, each = length(file_names) / length(x_tiles)),
    z_pos = rep(y_tiles, length.out = length(file_names)),
    x_length = 4097,
    height = max_raster,
    z_length = 4097,
    resolution = 4097,
    texture = if (is.null(overlay)) "" else paste0(sort(file_names), ".png")
  )

  temptiffs <- crop_tif(heightmap, manifest, temptiffs)
  temppngs <- NULL
  while (length(temppngs) != length(temptiffs)) {
    temppngs <- unique(vapply(
      seq_along(temptiffs),
      function(x) tempfile(fileext = ".png"),
      character(1)
    ))
  }
  names(temppngs) <- names(temptiffs)
  convert_to_png(temptiffs, temppngs, max_raster)

  mapply(
    function(x, y) {
      processing_image <- magick::image_read(x)
      processing_image <- magick::image_flop(processing_image)
      processing_image <- magick::image_convert(processing_image,
        format = "RGB",
        depth = 16,
        interlace = "Plane"
      )
      magick::image_write(processing_image, y)
    },
    temppngs,
    names(temppngs)
  )

  unlink(temppngs)

  if (!is.null(overlay)) {
    input_overlay <- raster::raster(overlay)
    max_overlay <- raster::cellStats(input_overlay, "max")

    temptiffs <- NULL
    while (length(temptiffs) != n_tiles) {
      temptiffs <- unique(vapply(
        1:(n_tiles),
        function(x) tempfile(fileext = ".tiff"),
        character(1)
      ))
    }
    temptiffs <- crop_tif(overlay, manifest, temptiffs, "texture")
    temppngs <- NULL
    temppngs <- names(temptiffs)
    names(temppngs) <- names(temptiffs)
    convert_to_png(temptiffs, temppngs, max_overlay)
    mapply(
      function(x, y) {
        processing_image <- magick::image_read(x)
        processing_image <- magick::image_flip(processing_image)
        processing_image <- magick::image_flop(processing_image)
        magick::image_write(processing_image, y)
      },
      temppngs,
      names(temppngs)
    )
  }

  utils::write.table(manifest,
    manifest_path,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    quote = FALSE
  )

  if (!is.null(importer_path) && !file.exists(importer_path)) {
    file.copy(
      system.file("import_terrain.cs", package = "terrainr"),
      importer_path,
      overwrite = TRUE
    )
  }

  return(invisible(manifest_path))
}

crop_tif <- function(img, manifest, temptiffs, field = "filename") {
  for (i in seq_len(nrow(manifest))) {
    # changing this to gdalUtilities causes my computer to crash
    gdalUtils::gdal_translate(img, temptiffs[[i]],
      srcwin = paste0(
        -manifest$x_pos[[i]],
        ", ",
        manifest$z_pos[[i]],
        ", ",
        manifest$x_length[[i]],
        ", ",
        manifest$z_length[[i]]
      )
    )
    names(temptiffs)[[i]] <- manifest[[field]][[i]]
  }

  temptiffs
}

convert_to_png <- function(temptiffs,
                           temppngs,
                           max_val) {
  mapply(
    function(x, y) {
      # changing this to gdalUtils causes errors
      sf::gdal_utils(
        "translate",
        source = x,
        destination = y,
        options = c(
          "-ot", "UInt16",
          "-of", "png",
          "-scale", "0", max_val, "0", "65535"
        )
      )
    },
    temptiffs,
    temppngs
  )

  unlink(temptiffs)
}
