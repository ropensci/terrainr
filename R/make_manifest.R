#' Transform rasters and write manifest file for import into Unity
#'
#' These functions crop input raster files into smaller square tiles and then
#' converts them into either .png or .raw files which are ready to be imported
#' into the Unity game engine. [make_manifest] also writes a "manifest" file and
#' importer script which may be used to automatically import the tiles into
#' Unity.
#'
#' @param heightmap File path to the heightmap to transform.
#' @param overlay File path to the image overlay to transform. Optional for
#' [make_manifest].
#' @param output_prefix The file path to prefix output tiles with.
#' @param manifest_path File path to write the manifest file to.
#' @param importer_path File name to write the importer script to. Set to NULL
#' to not copy the importer script. Will overwrite any file at the same path.
#' @param side_length Side length, in pixels, of each output tile. If the raster
#' has dimensions not evenly divisible by `side_length`, tiles will be generated
#' with overhanging pieces set to 0 units of elevation or RGB 0 (pure black).
#' Side lengths not equal to 2^x + 1 (for x <= 12) will cause a warning, as
#' tiles must be this size for import into Unity.
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
#' @rdname unity_crops
#' @export
make_manifest <- function(heightmap,
                          overlay = NULL,
                          output_prefix = "import",
                          manifest_path = "terrainr.manifest",
                          importer_path = "import_terrain.cs") {
  manifest <- prep_table(heightmap,
    side_length = 4097,
    output_prefix = output_prefix,
    type = "elevation"
  )
  transform_elevation(
    heightmap = heightmap,
    side_length = 4097,
    output_prefix = output_prefix
  )
  if (!is.null(overlay)) {
    overlay_manifest <- prep_table(heightmap,
      side_length = 4097,
      output_prefix = output_prefix,
      type = "overlay"
    )
    manifest$texture <- overlay_manifest$texture
    transform_overlay(
      overlay = overlay,
      side_length = 4097,
      output_prefix = output_prefix
    )
  }

  utils::write.table(manifest,
    manifest_path,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    quote = FALSE
  )

  if (!is.null(importer_path)) {
    file.copy(
      system.file("import_terrain.cs", package = "terrainr"),
      importer_path,
      overwrite = TRUE
    )
  }

  return(invisible(manifest_path))
}

#' @rdname unity_crops
#' @export
transform_elevation <- function(heightmap,
                                side_length = 4097,
                                output_prefix = "import") {
  manifest <- prep_table(heightmap,
    side_length,
    output_prefix,
    type = "elevation"
  )

  temptiffs <- NULL
  while (length(temptiffs) != nrow(manifest)) {
    temptiffs <- unique(vapply(
      seq_len(nrow(manifest)),
      function(x) tempfile(fileext = ".tiff"),
      character(1)
    ))
  }
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
  convert_to_png(temptiffs, temppngs, manifest$height[[1]])

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

  return(invisible(names(temppngs)))
}

#' @rdname unity_crops
#' @export
transform_overlay <- function(overlay,
                              side_length = 4097,
                              output_prefix = "import") {
  manifest <- prep_table(overlay, side_length, output_prefix, type = "overlay")

  temptiffs <- NULL
  while (length(temptiffs) != nrow(manifest)) {
    temptiffs <- unique(vapply(
      seq_len(nrow(manifest)),
      function(x) tempfile(fileext = ".tiff"),
      character(1)
    ))
  }
  temptiffs <- crop_tif(overlay, manifest, temptiffs, "texture")

  temppngs <- NULL
  temppngs <- names(temptiffs)
  names(temppngs) <- names(temptiffs)
  convert_to_png(temptiffs, temppngs, manifest$height[[1]])

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

  return(invisible(names(temppngs)))
}

prep_table <- function(input_raster,
                       side_length,
                       output_prefix,
                       type) {
  if (!identical(log((side_length - 1), 2), round(log((side_length - 1), 2)))) {
    rlang::warn(
      c(
        "The specified `side_length` may not work with Unity",
        x = "Side lengths must be equal to 2^x + 1 (for 5 <= x <= 12) for import into Unity", # nolint
        i = "Tiles will still be produced but may not be usable"
      )
    )
  }
  input_raster <- terra::rast(input_raster)
  max_raster <- max(terra::global(input_raster, "max", na.rm = TRUE))
  if (type == "overlay") {
    if (max_raster < 1) {
      max_raster <- 1
    } else if (
      isTRUE(all.equal(as.integer(max_raster), max_raster)) &
      max_raster < 255) {
      max_raster <- 255
    } else if (
      isTRUE(all.equal(as.integer(max_raster), max_raster)) &
      max_raster < 65535) {
      max_raster <- 65535
    }
  }

  x_tiles <- ceiling(terra::ncol(input_raster) / side_length)
  y_tiles <- ceiling(terra::nrow(input_raster) / side_length)

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
  x_tiles <- (x_tiles * side_length)

  y_tiles <- 0:(y_tiles - 1)
  y_tiles <- (y_tiles * side_length)

  output <- data.frame(
    filename = paste0(sort(file_names), ".raw"),
    x_pos = -rep(x_tiles, each = length(file_names) / length(x_tiles)),
    z_pos = rep(y_tiles, length.out = length(file_names)),
    x_length = side_length,
    height = max_raster,
    z_length = side_length,
    resolution = side_length
  )
  if (type == "overlay") output$texture <- paste0(sort(file_names), ".png")
  output
}


crop_tif <- function(img, manifest, temptiffs, field = "filename") {
  for (i in seq_len(nrow(manifest))) {
    sf::gdal_utils(
      "translate",
      img,
      temptiffs[[i]],
      options = c(
        "-srcwin",
        -manifest$x_pos[[i]],
        manifest$z_pos[[i]],
        manifest$x_length[[i]],
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
      sf::gdal_utils(
        "translate",
        source = x,
        destination = y,
        options = c(
          "-ot", "UInt16",
          "-of", "png",
          "-scale", "0", max_val, "0", "65535",
          "-a_nodata", "0"
        )
      )
    },
    temptiffs,
    temppngs
  )
  unlink(temptiffs)
}
