#' Initialize terrain inside of a Unity project.
#'
#' @param project The directory path of the Unity project to create terrain
#' inside.
#' @param heightmap The file path for the raster to transform into terrain.
#' @param overlay Optionally, a file path for an image overlay to layer on top
#' of the terrain surface. Leave as NULL for no overlay.
#' @param side_length The side length, in map units, for the terrain tiles.
#' Must be equal to 2^x + 1, for any x between 5 and 12.
#' @param scene_name The name of the Unity scene to create the terrain in.
#' @param action Boolean: Execute the unifir "script" and create the Unity
#' project? If FALSE, returns a non-executed script.
#' @param unity The location of the Unity executable to create projects with.
#' By default, will be auto-detected by [unifir::find_unity]
#'
#' @return An object of class "unifir_script", containing either an executed
#' unifir script (if action = TRUE) or a non-executed script object
#' (if action = FALSE).
#'
#' @importFrom unifir find_unity
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
#'   make_unity(file.path(tempdir(), "unity"), temptiff)
#' }
#' }
#'
#' @export
make_unity <- function(project,
                       heightmap,
                       overlay = NULL,
                       side_length = 4097,
                       scene_name = "terrainr_scene",
                       action = TRUE,
                       unity = find_unity()) {
  if (!requireNamespace("unifir", quietly = TRUE)) {
    stop(
      "make_unity requires the unifir package to work correctly. ",
      "Please install unifir to continue."
    )
  }

  if (!(side_length %in% 2^(5:12) + 1)) {
    stop(
      "side_length must be equal to a value of 2^x + 1, for any x ",
      "between 5 and 12."
    )
  }

  elevation_prefix <- tempfile()
  manifest <- prep_table(heightmap,
    side_length = side_length,
    output_prefix = elevation_prefix,
    type = "elevation"
  )
  transform_elevation(
    heightmap = heightmap,
    side_length = side_length,
    output_prefix = elevation_prefix
  )
  if (!dir.exists(project)) dir.create(project)
  lapply(
    manifest$filename,
    function(x) file.rename(x, file.path(project, basename(x)))
  )
  manifest$filename <- basename(manifest$filename)

  if (!is.null(overlay)) {
    overlay_prefix <- tempfile()
    overlay_manifest <- prep_table(heightmap,
      side_length = side_length,
      output_prefix = overlay_prefix,
      type = "overlay"
    )
    manifest$texture <- overlay_manifest$texture
    transform_overlay(
      overlay = overlay,
      side_length = side_length,
      output_prefix = overlay_prefix
    )
    lapply(
      manifest$texture,
      function(x) file.rename(x, file.path(project, basename(x)))
    )
    manifest$texture <- basename(manifest$texture)
  }

  script <- unifir::make_script(project,
                                scene_name = scene_name,
                                unity = unity)
  script <- unifir::new_scene(script, "DefaultGameObjects", "Single")

  for (i in seq_len(nrow(manifest))) {
    script <- unifir::create_terrain(
      script,
      heightmap_path = manifest$filename[i],
      x_pos = manifest$x_pos[i],
      z_pos = manifest$z_pos[i],
      width = manifest$x_length[i],
      height = manifest$height[i],
      length = manifest$z_length[i],
      heightmap_resolution = manifest$resolution[i],
      texture_path = ifelse(is.null(overlay), "", manifest$texture[i])
    )
  }

  script <- unifir::save_scene(script)
  if (action) {
    script <- unifir::action(script)
  }
  script
}
