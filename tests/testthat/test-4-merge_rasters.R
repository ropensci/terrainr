test_that("merge_raster expected errors error expectedly", {
  expect_error(merge_rasters("dummy_input.tif", "dummy_output.R"))
  expect_error(merge_rasters(
    "dummy_input.tif",
    "dummy_output.tif",
    "dummy_input.png"
  ))
  expect_error(merge_rasters(
    "dummy_input.tif",
    "dummy_output.tif",
    c("dummy_input.png", "dummy_input.png"),
    "dummy_output.tif"
  ))

  expect_error(merge_rasters("dummy_input.tif",
    NULL,
    "dummy_input.png",
    "dummy_output.png",
    merge_raster = FALSE
  ))
  expect_error(merge_rasters(
    "dummy_input.tif",
    "dummy_output.png",
    "dummy_input.png",
    "dummy_output.tif"
  ))
})

test_that("merge_raster files are identical no matter the filename", {
  skip_on_cran()
  df <- data.frame(
    lat = c(44.050030001, 44.05003),
    lng = c(-74.01164, -74.011640001)
  )
  df_sf <- sf::st_as_sf(df, coords = c("lng", "lat"))
  df_sf <- sf::st_set_crs(df_sf, 4326)
  first_tile <- add_bbox_buffer(df_sf, 10)
  ft_bbox <- sf::st_bbox(first_tile)
  second_tile <- terrainr_bounding_box(
    bl = c(lat = ft_bbox[["ymin"]], lng = ft_bbox[["xmin"]]),
    tr = point_from_distance(c(lat = ft_bbox[["ymax"]],
                               lng = ft_bbox[["xmax"]]), 10, 90)
  )
  # assign the output tile filenames...
  tmptif <- vector("list")
  temp_tiles <- get_tiles(first_tile,
    services = c("elevation", "ortho"),
    georeference = FALSE
  )
  tmptif[[1]] <- temp_tiles[[1]]
  tmptif[[2]] <- get_tiles(second_tile)[[1]]

  # create two outputs, one that needs fix_tif and one that doesn't:
  tmptif[[3]] <- tempfile(fileext = ".tif")
  tmptif[[4]] <- tempfile(fileext = ".tiff")

  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[3]])
  merge_rasters(c(tmptif[[1]], tmptif[[2]]), tmptif[[4]])

  expect_equal(
    raster::raster(tmptif[[3]])@extent,
    raster::raster(tmptif[[4]])@extent
  )

  merge_orth <- tempfile(fileext = ".tiff")
  merge_rasters(
    temp_tiles[[1]],
    tempfile(fileext = ".tif"),
    temp_tiles[[2]],
    merge_orth
  )

  stored_raster <- raster::raster("testdata/merge_rasters_test.tif")
  test_raster <- raster::raster(merge_orth)

  expect_equal(stored_raster@crs, test_raster@crs)
  expect_equal(stored_raster@extent, test_raster@extent)
})
