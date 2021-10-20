make_unity <- function(project,
                       heightmap,
                       overlay = NULL,
                       scene_name = "terrainr_scene") {

  elevation_prefix <- tempfile()
  manifest <- prep_table(heightmap,
                         side_length = 4097,
                         output_prefix = elevation_prefix,
                         type = "elevation"
  )
  transform_elevation(
    heightmap = heightmap,
    side_length = 4097,
    output_prefix = elevation_prefix
  )
  dir.create(project)
  lapply(
    manifest$filename,
    function(x) file.rename(x, file.path(project, basename(x)))
  )
  manifest$filename <- basename(manifest$filename)

  if (!is.null(overlay)) {
    overlay_prefix <- tempfile()
    overlay_manifest <- prep_table(heightmap,
                                   side_length = 4097,
                                   output_prefix = overlay_prefix,
                                   type = "overlay"
    )
    manifest$texture <- overlay_manifest$texture
    transform_overlay(
      overlay = overlay,
      side_length = 4097,
      output_prefix = overlay_prefix
    )
    lapply(
      manifest$texture,
      function(x) file.rename(x, file.path(project, basename(x)))
    )
  }
  manifest$texture <- basename(manifest$texture)

  script <- unifir::make_script(project, scene_name = scene_name)
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
  script <- unifir::action(script)
  script <- unifir::set_active_scene(script, scene_name)

}
